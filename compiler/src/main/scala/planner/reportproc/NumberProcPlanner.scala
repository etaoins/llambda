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

  private def compareOperands(state : PlannerState)(compareCond : ps.CompareCond.CompareCond, staticCalc : IntegerCompartor, val1 : iv.IntermediateValue, val2 : iv.IntermediateValue)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    if (!List(val1, val2).forall(_.hasDefiniteType(vt.ExactIntegerType))) {
      // Can't fast path this
      return None
    }

    (val1, val2) match {
      case (constantVal1 : iv.ConstantExactIntegerValue, constantVal2 : iv.ConstantExactIntegerValue) =>

        val compareResult = staticCalc(constantVal1.value, constantVal2.value)

        Some(PlanResult(
          state=state,
          value=new iv.ConstantBooleanValue(compareResult)
        ))

      case _ =>
        // Do a direct integer comparison
        val val1Temp = val1.toTempValue(vt.Int64)
        val val2Temp = val2.toTempValue(vt.Int64)

        val predicateTemp = ps.Temp(vt.Predicate)

        val signed = if (compareCond == ps.CompareCond.Equal) {
          None
        }
        else {
          Some(true)
        }

        // Do a direct integer compare
        plan.steps += ps.IntegerCompare(predicateTemp, compareCond, signed, val1Temp, val2Temp)

        Some(PlanResult(
          state=state,
          value=new iv.NativePredicateValue(predicateTemp)
        ))
    }
  }

  private def performBinaryIntegerOp(state : PlannerState)(instr : IntegerInstrBuilder, staticCalc : StaticResultBuilder, operands : List[iv.IntermediateValue])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    if (!operands.forall(_.hasDefiniteType(vt.ExactIntegerType))) {
      // Can't fast path this
      return None
    }

    // Split the list in to constant and non-constant values
    val constantValues = operands.collect { case constantInt : iv.ConstantExactIntegerValue =>
      constantInt.value
    }

    val dynamicValues = operands.filterNot(_.isInstanceOf[iv.ConstantExactIntegerValue])

    val instrOperands = if (constantValues.isEmpty) {
      // We have no dynamic values
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

    Some(PlanResult(
      state=state,
      value=finalValue
    ))
  }

  def apply(state : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.Equal, _ == _, val1, val2)
    
    case (">", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThan, _ > _, val1, val2)
    
    case (">=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThanEqual, _ >= _, val1, val2)
    
    case ("<", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThan, _ < _, val1, val2)
    
    case ("<=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThanEqual, _ <= _, val1, val2)

    case ("+", Nil) =>
      Some(PlanResult(
        state=state,
        value=new iv.ConstantExactIntegerValue(0)
      ))
    
    case ("*", Nil) =>
      Some(PlanResult(
        state=state,
        value=new iv.ConstantExactIntegerValue(1)
      ))
    
    case (reportName, List((operandSourceLoc, singleOperand))) if List("+", "*").contains(reportName) =>
      // Make sure the operand is numeric
      val numericTemp = plan.withContextLocation(operandSourceLoc) {
        singleOperand.toTempValue(vt.NumericType)
      }
      
      // Return it directly
      Some(PlanResult(
        state=state,
        value=TempValueToIntermediate(vt.NumericType, numericTemp)
      ))

    case ("-", Nil) =>
      // This isn't allowed - let it fail at runtime
      None
    
    case ("-", List((_, singleOperand))) =>
      // This is a special case that negates the passed value
      val constantZero = new iv.ConstantExactIntegerValue(0)
      performBinaryIntegerOp(state)(ps.IntegerSub.apply, _ - _, List(constantZero, singleOperand))

    case ("+", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryIntegerOp(state)(ps.IntegerAdd.apply, _ + _, operandValues)
    
    case ("-", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryIntegerOp(state)(ps.IntegerSub.apply, _ - _, operandValues)
    
    case ("*", multipleOperands) =>
      val operandValues = multipleOperands.map(_._2) 
      performBinaryIntegerOp(state)(ps.IntegerMul.apply, _ * _, operandValues)

    case _ =>
      None
  }
}
