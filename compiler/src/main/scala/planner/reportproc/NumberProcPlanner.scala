package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object NumberProcPlanner extends ReportProcPlanner {
  private type BinaryInstrBuilder = (ps.TempValue, ps.TempValue, ps.TempValue) => ps.Step
  private type IntegerCompartor = (Long, Long) => Boolean

  private type DoubleCompartor = (Double, Double) => Boolean
  
  private type StaticIntegerOp = (Long, Long) => Long
  private type StaticDoubleOp = (Double, Double) => Double

  private sealed abstract class CompareResult
  private case class StaticCompare(result : Boolean) extends CompareResult
  private case class DynamicCompare(nativePred : ps.TempValue) extends CompareResult
  private case object UnplannableCompare extends CompareResult

  private def compareOperands(state : PlannerState)(
      compareCond : ps.CompareCond.CompareCond,
      staticIntCalc : IntegerCompartor,
      staticFlonumCalc : DoubleCompartor,
      val1 : iv.IntermediateValue,
      val2 : iv.IntermediateValue
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : CompareResult = {
    (val1, val2) match {
      case (constantExactInt1 : iv.ConstantExactIntegerValue, constantExactInt2 : iv.ConstantExactIntegerValue) =>
        val compareResult = staticIntCalc(constantExactInt1.value, constantExactInt2.value)
        StaticCompare(compareResult)
      
      case (constantFlonum1 : iv.ConstantFlonumValue, constantFlonum2 : iv.ConstantFlonumValue) =>
        val compareResult = staticFlonumCalc(constantFlonum1.value, constantFlonum2.value)
        StaticCompare(compareResult)
      
      case (constantExactInt1 : iv.ConstantExactIntegerValue, constantFlonum2 : iv.ConstantFlonumValue) =>
        val compareResult = staticFlonumCalc(constantExactInt1.value.toDouble, constantFlonum2.value)
        StaticCompare(compareResult)
      
      case (constantFlonum1 : iv.ConstantFlonumValue, constantExactInt2 : iv.ConstantExactIntegerValue) =>
        val compareResult = staticFlonumCalc(constantFlonum1.value, constantExactInt2.value.toDouble)
        StaticCompare(compareResult)

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

        DynamicCompare(predicateTemp)

      case (flonum1, flonum2) if flonum1.hasDefiniteType(vt.FlonumType) && flonum2.hasDefiniteType(vt.FlonumType) =>
        val val1Temp = flonum1.toTempValue(vt.Double)
        val val2Temp = flonum2.toTempValue(vt.Double)

        val predicateTemp = ps.Temp(vt.Predicate)

        // Do a direct float compare
        plan.steps += ps.FloatCompare(predicateTemp, compareCond, val1Temp, val2Temp)

        DynamicCompare(predicateTemp)

      case _ =>
        UnplannableCompare
    }
  }
  
  private def compareOperandList(state : PlannerState)(
      compareCond : ps.CompareCond.CompareCond,
      staticIntCalc : IntegerCompartor,
      staticFlonumCalc : DoubleCompartor,
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    // Compare in a fork in case we abort the whole thing later
    val comparePlan = plan.forkPlan()

    val pairwiseResults = operands.sliding(2).toList map {
      case List(left, right) =>
        compareOperands(state)(compareCond, staticIntCalc, staticFlonumCalc, left, right)(comparePlan, worldPtr)
    }

    // Now filter out all the static results
    val pairwiseNativePreds = pairwiseResults flatMap {
      case UnplannableCompare =>
        // We can't compare this at compile time
        return None

      case StaticCompare(false) =>
        // This is false - the whole expression must be false!
        return Some(new iv.ConstantBooleanValue(false))

      case StaticCompare(true) =>
        // We don't need to include constant true values
        None

      case DynamicCompare(nativePred) =>
        Some(nativePred)
    } : List[ps.TempValue]

    if (pairwiseNativePreds.isEmpty) {
      // This is statically true
      return Some(new iv.ConstantBooleanValue(true))
    }

    // We definitely need to compare at runtime - include our plan steps
    plan.steps ++= comparePlan.steps

    // Combine all of the native predicates together
    val resultPred = pairwiseNativePreds.reduceLeft { (nativePred, trueBranchValue) =>
      val condResult = ps.Temp(vt.Predicate)
      plan.steps += ps.CondBranch(condResult, nativePred, Nil, trueBranchValue, Nil, nativePred)

      condResult
    }

    Some(new iv.NativePredicateValue(resultPred))
  }

  private def numberToDoubleTemp(
      value : iv.IntermediateValue,
      isFlonum : Boolean
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    if (isFlonum) {
      value.toTempValue(vt.Double)
    }
    else {
      val intTemp = value.toTempValue(vt.Int64)
      val convertTemp = ps.Temp(vt.Double)
      plan.steps += ps.ConvertNativeIntegerToFloat(convertTemp, intTemp, true, vt.Double)

      convertTemp
    }

  private def performBinaryMixedOp(state : PlannerState)(
      intInstr : BinaryInstrBuilder,
      flonumInstr : BinaryInstrBuilder,
      staticIntCalc : StaticIntegerOp,
      staticFlonumCalc : StaticDoubleOp,
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = operands.reduceLeft { (op1 : iv.IntermediateValue, op2 : iv.IntermediateValue) => (op1, op2) match {
      case (constantInt1 : iv.ConstantExactIntegerValue, constantInt2 : iv.ConstantExactIntegerValue) =>
        new iv.ConstantExactIntegerValue(staticIntCalc(constantInt1.value, constantInt2.value))
      
      case (constantFlonum1 : iv.ConstantFlonumValue, constantFlonum2 : iv.ConstantFlonumValue) =>
        new iv.ConstantFlonumValue(staticFlonumCalc(constantFlonum1.value, constantFlonum2.value))
      
      case (constantInt1 : iv.ConstantExactIntegerValue, constantFlonum2 : iv.ConstantFlonumValue) =>
        val flonum1 = constantInt1.value.toDouble
        new iv.ConstantFlonumValue(staticFlonumCalc(flonum1, constantFlonum2.value))
      
      case (constantFlonum1 : iv.ConstantFlonumValue, constantInt2 : iv.ConstantExactIntegerValue) =>
        val flonum2 = constantInt2.value.toDouble
        new iv.ConstantFlonumValue(staticFlonumCalc(constantFlonum1.value, flonum2))

      case (dynamic1, dynamic2) =>
        val dynamic1IsInt = dynamic1.hasDefiniteType(vt.ExactIntegerType)
        val dynamic1IsFlonum = dynamic1.hasDefiniteType(vt.FlonumType)

        val dynamic2IsInt = dynamic2.hasDefiniteType(vt.ExactIntegerType)
        val dynamic2IsFlonum = dynamic2.hasDefiniteType(vt.FlonumType)

        if (!(dynamic1IsInt || dynamic1IsFlonum) || !(dynamic2IsInt || dynamic2IsFlonum)) {
          // We don't have definite types; abort
          return None
        }
        else if (dynamic1IsInt && dynamic2IsInt) {
          // Both integers
          val intTemp1 = dynamic1.toTempValue(vt.Int64)(inlinePlan, worldPtr)
          val intTemp2 = dynamic2.toTempValue(vt.Int64)(inlinePlan, worldPtr)
          val resultTemp = ps.Temp(vt.Int64)

          inlinePlan.steps += intInstr(resultTemp, intTemp1, intTemp2) 
          
          new iv.NativeExactIntegerValue(resultTemp, vt.Int64)
        }
        else {
          // At least one is a double
          val doubleTemp1 = numberToDoubleTemp(dynamic1, dynamic1IsFlonum)(inlinePlan, worldPtr)
          val doubleTemp2 = numberToDoubleTemp(dynamic2, dynamic2IsFlonum)(inlinePlan, worldPtr)

          val resultTemp = ps.Temp(vt.Double)
          inlinePlan.steps += flonumInstr(resultTemp, doubleTemp1, doubleTemp2) 
          
          new iv.NativeFlonumValue(resultTemp, vt.Double)
        }

    }}

    // We need the inline plan now
    plan.steps ++= inlinePlan.steps

    Some(resultValue)
  } 

  private def performBinaryFlonumOp(state : PlannerState)(
      instr : BinaryInstrBuilder,
      staticCalc : StaticDoubleOp,
      operands : List[iv.IntermediateValue]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = {
    implicit val inlinePlan = plan.forkPlan()

    val resultValue = operands.reduceLeft { (op1 : iv.IntermediateValue, op2 : iv.IntermediateValue) => (op1, op2) match {
      case (constant1 : iv.ConstantNumberValue, constant2 : iv.ConstantNumberValue) =>
        new iv.ConstantFlonumValue(staticCalc(constant1.doubleValue, constant2.doubleValue))

      case (dynamic1, dynamic2) =>
        val dynamic1IsInt = dynamic1.hasDefiniteType(vt.ExactIntegerType)
        val dynamic1IsFlonum = dynamic1.hasDefiniteType(vt.FlonumType)

        val dynamic2IsInt = dynamic2.hasDefiniteType(vt.ExactIntegerType)
        val dynamic2IsFlonum = dynamic2.hasDefiniteType(vt.FlonumType)

        if (!(dynamic1IsInt || dynamic1IsFlonum) || !(dynamic2IsInt || dynamic2IsFlonum)) {
          // We don't have definite types; abort
          return None
        }
        else {
          val doubleTemp1 = numberToDoubleTemp(dynamic1, dynamic1IsFlonum)(inlinePlan, worldPtr)
          val doubleTemp2 = numberToDoubleTemp(dynamic2, dynamic2IsFlonum)(inlinePlan, worldPtr)

          val resultTemp = ps.Temp(vt.Double)
          inlinePlan.steps += instr(resultTemp, doubleTemp1, doubleTemp2) 
          
          new iv.NativeFlonumValue(resultTemp, vt.Double)
        }
    }}

    // We need the inline plan now
    plan.steps ++= inlinePlan.steps

    Some(resultValue)
  } 
  override def planWithValue(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("=", operands) if operands.length >= 2 =>
      compareOperandList(state)(ps.CompareCond.Equal, _ == _, _ == _, operands.map(_._2))
    
    case (">", operands) if operands.length >= 2 =>
      compareOperandList(state)(ps.CompareCond.GreaterThan, _ > _, _ > _, operands.map(_._2))
    
    case (">=", operands) if operands.length >= 2 =>
      compareOperandList(state)(ps.CompareCond.GreaterThanEqual, _ >= _, _ >= _, operands.map(_._2))
    
    case ("<", operands) if operands.length >= 2 =>
      compareOperandList(state)(ps.CompareCond.LessThan, _ < _, _ < _, operands.map(_._2))
    
    case ("<=", operands) if operands.length >= 2 =>
      compareOperandList(state)(ps.CompareCond.LessThanEqual, _ <= _, _ <= _, operands.map(_._2))

    case ("+", Nil) =>
      Some(new iv.ConstantExactIntegerValue(0))
    
    case ("*", Nil) =>
      Some(new iv.ConstantExactIntegerValue(1))
    
    case (reportName, List((operandSourceLoc, singleOperand))) if List("+", "*").contains(reportName) =>
      // Make sure the operand is numeric
      val numericValue = plan.withContextLocation(operandSourceLoc) {
        singleOperand.castToSchemeType(vt.NumberType)
      }
      
      // Return it directly
      Some(numericValue)

    case ("+", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryMixedOp(state)(
        intInstr=ps.IntegerAdd.apply,
        flonumInstr=ps.FloatAdd.apply,
        staticIntCalc=_ + _,
        staticFlonumCalc=_ + _,
        operands=operandValues
      )

    case ("-", Nil) =>
      // This isn't allowed - let it fail at runtime
      None
    
    case ("-", List((_, singleOperand))) =>
      // This is a special case that negates the passed value
      val constantZero = new iv.ConstantExactIntegerValue(0)
      performBinaryMixedOp(state)(
        intInstr=ps.IntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        operands=List(constantZero, singleOperand)
      )
    
    case ("-", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryMixedOp(state)(
        intInstr=ps.IntegerSub.apply,
        flonumInstr=ps.FloatSub.apply,
        staticIntCalc=_ - _,
        staticFlonumCalc=_ - _,
        operands=operandValues
      )
    
    case ("*", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryMixedOp(state)(
        intInstr=ps.IntegerMul.apply,
        flonumInstr=ps.FloatMul.apply,
        staticIntCalc=_ * _,
        staticFlonumCalc=_ * _,
        operands=operandValues
      )
    
    case ("/", Nil) =>
      // This isn't allowed - let it fail at runtime
      None
    
    case ("/", List((_, singleOperand))) =>
      // This is a special case that negates the passed value
      val constantZero = new iv.ConstantFlonumValue(1.0)
      performBinaryFlonumOp(state)(
        instr=ps.FloatDiv.apply,
        staticCalc=_ / _,
        operands=List(constantZero, singleOperand)
      )
    
    case ("/", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryFlonumOp(state)(
        instr=ps.FloatDiv.apply,
        staticCalc=_ / _,
        operands=operandValues
      )

    case ("exact", List(singleOperand)) =>
      val singleValue = singleOperand._2

      singleValue match  {
        case knownExactInt if vt.SatisfiesType(vt.ExactIntegerType, knownExactInt.schemeType) == Some(true) =>
          // Already an exact int
          Some(singleValue)

        case constFlonum : iv.ConstantFlonumValue =>
          val longValue = constFlonum.value.toLong

          // Make sure this was lossless
          if (longValue.toDouble == constFlonum.value) {
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
        case knownFlonum if vt.SatisfiesType(vt.FlonumType, knownFlonum.schemeType) == Some(true) =>
          // Already a flonum
          Some(knownFlonum)

        case constExactInt : iv.ConstantExactIntegerValue =>
          val doubleValue = constExactInt.value.toDouble

          // Make sure this was lossless
          if (doubleValue.toLong == constExactInt.value) {
            Some(new iv.ConstantFlonumValue(doubleValue))
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
