package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{RuntimeErrorMessage, ContextLocated}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object ListProcPlanner extends ReportProcPlanner {
  def apply(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("null?", List((_, singleOperand))) =>
      predicatePlanner(initialState)(singleOperand, ct.EmptyListCell)
    
    case ("pair?", List((_, singleOperand))) =>
      predicatePlanner(initialState)(singleOperand, ct.PairCell)
    
    case ("list?", List((_, singleOperand))) if singleOperand.isDefiniteProperList =>
      // Definitely a proper list
      Some(PlanResult(
        state=initialState,
        value=new iv.ConstantBooleanValue(true)
      ))

    case ("list", operands) =>
      // Build a proper list directly
      val resultTemp = ps.CellTemp(ct.ListElementCell)
      val operandTemps = operands.map(_._2).map(_.toTempValue(vt.IntrinsicCellType(ct.DatumCell)))

      plan.steps += ps.BuildProperList(resultTemp, operandTemps) 
      
      val listValue = new iv.IntrinsicCellValue(
        possibleTypes=ct.ListElementCell.concreteTypes,
        cellType=ct.ListElementCell,
        tempValue=resultTemp,
        properListCell=true 
      )

      Some(PlanResult(
        state=initialState,
        value=listValue
      ))

    case ("length", List((_, singleOperand))) =>
      // Do we know the length at compile time?
      singleOperand match {
        case constantPair : iv.ConstantPairValue =>
          for(listMetrics <- constantPair.listMetricsOpt) {
            // Yes, return it directly
            return Some(PlanResult(
              state=initialState,
              value=new iv.ConstantExactIntegerValue(listMetrics.length)
            ))
          }

        case _ =>
      }

      if (singleOperand.isDefiniteProperList) {
        val listElementTemp = singleOperand.toTempValue(vt.IntrinsicCellType(ct.ListElementCell))
        val resultTemp = ps.Temp(vt.UInt32)

        // This is a guaranteed proper list - we can quickly calculate the length inline
        plan.steps += ps.CalcProperListLength(resultTemp, listElementTemp)

        return Some(PlanResult(
          state=initialState,
          value=TempValueToIntermediate(vt.UInt32, resultTemp)
        ))
      }
      else {
        // This can error out at runtime - let the stdlib deal with it
        None
      }

    case ("set-car!", List((pairLoc, pairValue), (_, newValue))) =>
      val pairTemp = plan.withContextLocation(pairLoc) {
        pairValue.toTempValue(vt.IntrinsicCellType(ct.PairCell))
      }

      val newValueTemp = newValue.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

      val errorMessage = RuntimeErrorMessage(
        name="setCarImmutable",
        text="(set-car!) attempted on pair literal"
      )

      plan.steps += ps.AssertPairMutable(worldPtr, pairTemp, errorMessage)
      plan.steps += ps.SetPairCar(pairTemp, newValueTemp)

      Some(PlanResult(
        state=initialState,
        value=iv.UnitValue
      ))
    
    case ("set-cdr!", List((pairLoc, pairValue), (_, newValue))) =>
      val pairTemp = plan.withContextLocation(pairLoc) {
        pairValue.toTempValue(vt.IntrinsicCellType(ct.PairCell))
      }

      val newValueTemp = newValue.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

      val errorMessage = RuntimeErrorMessage(
        name="setCdrImmutable",
        text="(set-cdr!) attempted on pair literal"
      )

      plan.steps += ps.AssertPairMutable(worldPtr, pairTemp, errorMessage)
      plan.steps += ps.SetPairCdr(pairTemp, newValueTemp)

      Some(PlanResult(
        state=initialState,
        value=iv.UnitValue
      ))

    case ("cons", List((_, carValue), (_, cdrValue))) =>
      val pairTemp = ps.CellTemp(ct.PairCell)

      val carTemp = carValue.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
      val cdrTemp = cdrValue.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

      plan.steps += ps.InitPair(pairTemp)
      plan.steps += ps.SetPairCar(pairTemp, carTemp)
      plan.steps += ps.SetPairCdr(pairTemp, cdrTemp)

      Some(PlanResult(
        state=initialState,
        value=TempValueToIntermediate(vt.IntrinsicCellType(ct.PairCell), pairTemp)
      ))

    case _ =>
      None
  }
}
