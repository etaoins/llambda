package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{RuntimeErrorMessage, ContextLocated}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

import scala.annotation.tailrec

object ListProcPlanner extends ReportProcPlanner {
  private val listElementType = vt.SchemeType.fromCellType(ct.ListElementCell)

  private def staticMemberSearch(compareFunc : StaticValueEqv.EqvFunction, needleValue : iv.IntermediateValue, listValue : iv.IntermediateValue) : Option[iv.IntermediateValue] = listValue match {
    case constantPairValue : iv.ConstantPairValue =>
      compareFunc(needleValue, constantPairValue.car) match {
        case Some(true) =>
          // Found it!
          Some(constantPairValue)

        case Some(false) =>
          // definitely not a match - keep looking
          staticMemberSearch(compareFunc, needleValue, constantPairValue.cdr)

        case None =>
          // Can't statically determine this
          None
      }

    case iv.EmptyListValue =>
      // Doesn't exist in the list
      Some(new iv.ConstantBooleanValue(false))

    case _ =>
      // Not a constant list
      None
  }

  def apply(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case ("list?", List((_, singleOperand))) if singleOperand.isDefiniteProperList =>
      // Definitely a proper list
      Some(PlanResult(
        state=initialState,
        value=new iv.ConstantBooleanValue(true)
      ))

    case ("list", operands) =>
      // Build a proper list directly
      val resultTemp = ps.CellTemp(ct.ListElementCell)
      val operandTemps = operands.map(_._2).map(_.toTempValue(vt.AnySchemeType))

      plan.steps += ps.BuildProperList(resultTemp, operandTemps) 
      
      val listValue = new iv.CellValue(
        schemeType=listElementType,
        tempType=ct.ListElementCell,
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
          for(listLength <- constantPair.listLengthOpt) {
            // Yes, return it directly
            return Some(PlanResult(
              state=initialState,
              value=new iv.ConstantExactIntegerValue(listLength)
            ))
          }

        case iv.EmptyListValue =>
          return Some(PlanResult(
            state=initialState,
            value=new iv.ConstantExactIntegerValue(0L)
          ))

        case _ =>
      }

      if (singleOperand.isDefiniteProperList) {
        val listElementTemp = singleOperand.toTempValue(listElementType)
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
        pairValue.toTempValue(vt.PairType)
      }

      val newValueTemp = newValue.toTempValue(vt.AnySchemeType)

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
        pairValue.toTempValue(vt.PairType)
      }

      val newValueTemp = newValue.toTempValue(vt.AnySchemeType)

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

      val carTemp = carValue.toTempValue(vt.AnySchemeType)
      val cdrTemp = cdrValue.toTempValue(vt.AnySchemeType)

      plan.steps += ps.InitPair(pairTemp)
      plan.steps += ps.SetPairCar(pairTemp, carTemp)
      plan.steps += ps.SetPairCdr(pairTemp, cdrTemp)

      Some(PlanResult(
        state=initialState,
        value=TempValueToIntermediate(vt.PairType, pairTemp)
      ))

    case (_, List((_, needleValue), (_, listValue))) if List("memq", "memv").contains(reportName) =>
      staticMemberSearch(StaticValueEqv.valuesAreEqv, needleValue, listValue) map { staticValue =>
        PlanResult(
          state=initialState,
          value=staticValue
        )
      }

    case ("member", List((_, needleValue), (_, listValue))) =>
      staticMemberSearch(StaticValueEqv.valuesAreEqual, needleValue, listValue) map { staticValue =>
        PlanResult(
          state=initialState,
          value=staticValue
        )
      }

    case _ =>
      None
  }
}
