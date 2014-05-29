package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.SourceLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object NumberProcPlanner extends ReportProcPlanner {
  private type IntegerOperation = (ps.TempValue, ps.TempValue, ps.TempValue) => ps.Step
  private val numericType = vt.IntrinsicCellType(ct.NumericCell)

  private def compareOperands(state : PlannerState)(compareCond : ps.CompareCond.CompareCond, val1 : iv.IntermediateValue, val2 : iv.IntermediateValue)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    if ((val1.possibleTypes == Set(ct.ExactIntegerCell)) &&
        (val2.possibleTypes == Set(ct.ExactIntegerCell)))
    {
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
    else {
      None
    }
  }

  private def performBinaryIntegerOp(state : PlannerState)(operation : IntegerOperation, operands : List[iv.IntermediateValue])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    if (!operands.forall(_.possibleTypes == Set(ct.ExactIntegerCell))) {
      // Can't fast path this
      None
    }
    else {
      val finalValue = operands.reduceLeft { (op1, op2) =>
        val op1Temp = op1.toTempValue(vt.Int64)
        val op2Temp = op2.toTempValue(vt.Int64)

        val resultTemp = ps.Temp(vt.Int64)

        plan.steps += operation(resultTemp, op1Temp, op2Temp)

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
  }

  def apply(state : PlannerState)(reportName : String, operands : List[(SourceLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("number?", List((_, singleOperand))) =>
      predicatePlanner(state)(singleOperand, ct.NumericCell)

    case ("integer?", List((_, singleOperand))) =>
      predicatePlanner(state)(singleOperand, ct.ExactIntegerCell)
    
    case ("=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.Equal, val1, val2)
    
    case (">", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThan, val1, val2)
    
    case (">=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.GreaterThanEqual, val1, val2)
    
    case ("<", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThan, val1, val2)
    
    case ("<=", List((_, val1), (_, val2))) =>
      compareOperands(state)(ps.CompareCond.LessThanEqual, val1, val2)

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
      val numericTemp = LocateExceptionsWith(operandSourceLoc) {
        singleOperand.toTempValue(numericType)
      }
      
      // Return it directly
      Some(PlanResult(
        state=state,
        value=TempValueToIntermediate(numericType, numericTemp)
      ))

    case ("-", Nil) =>
      // This isn't allowed - let it fail at runtime
      None
    
    case ("-", List((_, singleOperand))) =>
      // This is a special case that negates the passed value
      val constantZero = new iv.ConstantExactIntegerValue(0)
      performBinaryIntegerOp(state)(ps.IntegerSub.apply, List(constantZero, singleOperand))

    case ("+", multipleOperands) =>
      performBinaryIntegerOp(state)(ps.IntegerAdd.apply, multipleOperands.map(_._2))
    
    case ("-", multipleOperands) =>
      performBinaryIntegerOp(state)(ps.IntegerSub.apply, multipleOperands.map(_._2))
    
    case ("*", multipleOperands) =>
      performBinaryIntegerOp(state)(ps.IntegerMul.apply, multipleOperands.map(_._2))

    case _ =>
      None
  }
}
