package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object NumberProcPlanner extends ReportProcPlanner {
  private type IntegerInstrBuilder = (ps.TempValue, ps.TempValue, ps.TempValue) => ps.Step
  private type IntegerCompartor = (Long, Long) => Boolean
  private type StaticResultBuilder = (Long, Long) => Long
  
  private type DoubleCompartor = (Double, Double) => Boolean

  private def compareOperands(state : PlannerState)(
      compareCond : ps.CompareCond.CompareCond,
      staticIntCalc : IntegerCompartor,
      staticDoubleCalc : DoubleCompartor,
      val1 : iv.IntermediateValue,
      val2 : iv.IntermediateValue
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    (val1, val2) match {
      case (constantExactInt1 : iv.ConstantExactIntegerValue, constantExactInt2 : iv.ConstantExactIntegerValue) =>
        val compareResult = staticIntCalc(constantExactInt1.value, constantExactInt2.value)
        Some(new iv.ConstantBooleanValue(compareResult))
      
      case (constantFlonum1 : iv.ConstantInexactRationalValue, constantFlonum2 : iv.ConstantInexactRationalValue) =>
        val compareResult = staticDoubleCalc(constantFlonum1.value, constantFlonum2.value)
        Some(new iv.ConstantBooleanValue(compareResult))
      
      case (constantExactInt1 : iv.ConstantExactIntegerValue, constantFlonum2 : iv.ConstantInexactRationalValue) =>
        val compareResult = staticDoubleCalc(constantExactInt1.value.toDouble, constantFlonum2.value)
        Some(new iv.ConstantBooleanValue(compareResult))
      
      case (constantFlonum1 : iv.ConstantInexactRationalValue, constantExactInt2 : iv.ConstantExactIntegerValue) =>
        val compareResult = staticDoubleCalc(constantFlonum1.value, constantExactInt2.value.toDouble)
        Some(new iv.ConstantBooleanValue(compareResult))

      case (exactInt1, exactInt2) if exactInt1.hasDefiniteType(vt.ExactIntegerType) && exactInt2.hasDefiniteType(vt.ExactIntegerType) =>
        val val1Temp = exactInt1.toTempValue(vt.Int64)
        val val2Temp = exactInt2.toTempValue(vt.Int64)

        val predicateTemp = ps.Temp(vt.Predicate)

        val signed = if (compareCond == ps.CompareCond.Equal) {
          None
        }
        else {
          Some(true)
        }

        // Do a direct integer compare
        plan.steps += ps.IntegerCompare(predicateTemp, compareCond, signed, val1Temp, val2Temp)

        Some(new iv.NativePredicateValue(predicateTemp))

      case (flonum1, flonum2) if flonum1.hasDefiniteType(vt.InexactRationalType) && flonum2.hasDefiniteType(vt.InexactRationalType) =>
        val val1Temp = flonum1.toTempValue(vt.Double)
        val val2Temp = flonum2.toTempValue(vt.Double)

        val predicateTemp = ps.Temp(vt.Predicate)

        // Do a direct float compare
        plan.steps += ps.FloatCompare(predicateTemp, compareCond, val1Temp, val2Temp)

        Some(new iv.NativePredicateValue(predicateTemp))

      case _ =>
        None
    }
  }

  private def performBinaryIntegerOp(state : PlannerState)(
      instr : IntegerInstrBuilder,
      staticCalc : StaticResultBuilder,
      isCommutative : Boolean,
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    if (!operands.forall(_.hasDefiniteType(vt.ExactIntegerType))) {
      // Can't fast path this
      return None
    }

    // Split the list in to constant and non-constant values
    val constantValues = operands.collect { case constantInt : iv.ConstantExactIntegerValue =>
      constantInt.value
    }

    val dynamicValues = operands.filterNot(_.isInstanceOf[iv.ConstantExactIntegerValue])

    val instrOperands = if ((constantValues.length != operands.length) && !isCommutative) {
      // This operation is non-commutative so we can't re-order our operands
      operands
    }
    else if (constantValues.isEmpty) {
      // We have no constant values
      dynamicValues
    }
    else {
      // Calculate the static value
      val constantValue = constantValues.reduceLeft(staticCalc)

      // Add this to our dynamic values
      new iv.ConstantExactIntegerValue(constantValue) :: dynamicValues
    } : List[iv.IntermediateValue]

    val finalValue = instrOperands.reduceLeft { (op1, op2) =>
      val op1Temp = op1.toTempValue(vt.Int64)
      val op2Temp = op2.toTempValue(vt.Int64)

      val resultTemp = ps.Temp(vt.Int64)

      plan.steps += instr(resultTemp, op1Temp, op2Temp)

      TempValueToIntermediate(
        vt.Int64,
        resultTemp
      )
    }

    Some(finalValue)
  }

  def apply(state : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.Equal, _ == _, _ == _, val1, val2)
    
    case (">", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThan, _ > _, _ > _, val1, val2)
    
    case (">=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThanEqual, _ >= _, _ >= _, val1, val2)
    
    case ("<", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThan, _ < _, _ < _, val1, val2)
    
    case ("<=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThanEqual, _ <= _, _ <= _, val1, val2)

    case ("+", Nil) =>
      Some(new iv.ConstantExactIntegerValue(0))
    
    case ("*", Nil) =>
      Some(new iv.ConstantExactIntegerValue(1))
    
    case (reportName, List((operandSourceLoc, singleOperand))) if List("+", "*").contains(reportName) =>
      // Make sure the operand is numeric
      val numericTemp = plan.withContextLocation(operandSourceLoc) {
        singleOperand.toTempValue(vt.NumericType)
      }
      
      // Return it directly
      Some(TempValueToIntermediate(vt.NumericType, numericTemp))

    case ("-", Nil) =>
      // This isn't allowed - let it fail at runtime
      None
    
    case ("-", List((_, singleOperand))) =>
      // This is a special case that negates the passed value
      val constantZero = new iv.ConstantExactIntegerValue(0)
      performBinaryIntegerOp(state)(ps.IntegerSub.apply, _ - _, false, List(constantZero, singleOperand))

    case ("+", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryIntegerOp(state)(ps.IntegerAdd.apply, _ + _, true, operandValues)
    
    case ("-", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryIntegerOp(state)(ps.IntegerSub.apply, _ - _, false, operandValues)
    
    case ("*", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryIntegerOp(state)(ps.IntegerMul.apply, _ * _, true, operandValues)

    case ("exact", List(singleOperand)) =>
      val singleValue = singleOperand._2

      singleValue match  {
        case knownExactInt if vt.SatisfiesType(vt.ExactIntegerType, knownExactInt.schemeType) == Some(true) =>
          // Already an exact int
          Some(singleValue)

        case constInexact : iv.ConstantInexactRationalValue =>
          val longValue = constInexact.value.toLong

          // Make sure this was lossless
          if (longValue.toDouble== constInexact.value) {
            Some(new iv.ConstantExactIntegerValue(longValue))
          }
          else {
            None
          }

        case _ =>
          None
      }
    
    case ("inexact", List(singleOperand)) =>
      val singleValue = singleOperand._2

      singleValue match  {
        case knownInexact if vt.SatisfiesType(vt.InexactRationalType, knownInexact.schemeType) == Some(true) =>
          // Already an inexact rational
          Some(knownInexact)

        case constExactInt : iv.ConstantExactIntegerValue =>
          val doubleValue = constExactInt.value.toDouble

          // Make sure this was lossless
          if (doubleValue.toLong == constExactInt.value) {
            Some(new iv.ConstantInexactRationalValue(doubleValue))
          }
          else {
            None
          }
        
        case _ =>
          None
      }

    case _ =>
      None
  }
}
