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
  private def staticMemberSearch(compareFunc : StaticValueEqv.EqvFunction, needleValue : iv.IntermediateValue, listValue : iv.IntermediateValue) : Option[iv.IntermediateValue] = listValue match {
    case knownPair : iv.KnownPair =>
      compareFunc(needleValue, knownPair.car) match {
        case Some(true) =>
          // Found it!
          Some(knownPair)

        case Some(false) =>
          // definitely not a match - keep looking
          staticMemberSearch(compareFunc, needleValue, knownPair.cdr)

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

  def apply(initialState : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("list", operands) =>
      // Build a proper list directly
      val operandValues = operands.map(_._2)
      Some(ValuesToProperList(operandValues))

    case ("length", List((_, singleOperand))) =>
      // Do we know the length at compile time?
      singleOperand match {
        case knownListElement : iv.KnownListElement =>
          for(listLength <- knownListElement.listLengthOpt) {
            // Yes, return it directly
            return Some(
              new iv.ConstantExactIntegerValue(listLength)
            )
          }

        case _ =>
      }

      if (singleOperand.isDefiniteProperList) {
        val listElementTemp = singleOperand.toTempValue(vt.ListElementType)
        val resultTemp = ps.Temp(vt.UInt32)

        // This is a guaranteed proper list - we can quickly calculate the length inline
        plan.steps += ps.CalcProperListLength(resultTemp, listElementTemp)

        return Some(
          TempValueToIntermediate(vt.UInt32, resultTemp)(plan.config)
        )
      }
      else {
        // This can error out at runtime - let the stdlib deal with it
        None
      }

    case ("set-car!", List((pairLoc, pairValue), (_, newValue))) =>
      val pairTemp = plan.withContextLocation(pairLoc) {
        pairValue.toTempValue(vt.AnyPairType)
      }

      val newValueTemp = newValue.toTempValue(vt.AnySchemeType)

      val errorMessage = RuntimeErrorMessage(
        name="setCarImmutable",
        text="(set-car!) attempted on pair literal"
      )

      plan.steps += ps.AssertPairMutable(worldPtr, pairTemp, errorMessage)
      plan.steps += ps.SetPairCar(pairTemp, newValueTemp)

      Some(iv.UnitValue)
    
    case ("set-cdr!", List((pairLoc, pairValue), (_, newValue))) =>
      val pairTemp = plan.withContextLocation(pairLoc) {
        pairValue.toTempValue(vt.AnyPairType)
      }

      val newValueTemp = newValue.toTempValue(vt.AnySchemeType)

      val errorMessage = RuntimeErrorMessage(
        name="setCdrImmutable",
        text="(set-cdr!) attempted on pair literal"
      )

      plan.steps += ps.AssertPairMutable(worldPtr, pairTemp, errorMessage)
      plan.steps += ps.SetPairCdr(pairTemp, newValueTemp)

      Some(iv.UnitValue)

    case ("cons", List((_, carValue), (_, cdrValue))) =>
      val pairTemp = ps.CellTemp(ct.PairCell)

      val carTemp = carValue.toTempValue(vt.AnySchemeType)
      val cdrTemp = cdrValue.toTempValue(vt.AnySchemeType)

      plan.steps += ps.InitPair(pairTemp)
      plan.steps += ps.SetPairCar(pairTemp, carTemp)
      plan.steps += ps.SetPairCdr(pairTemp, cdrTemp)

      Some(TempValueToIntermediate(vt.AnyPairType, pairTemp)(plan.config))

    case (_, List((_, needleValue), (_, listValue))) if List("memq", "memv").contains(reportName) =>
      staticMemberSearch(StaticValueEqv.valuesAreEqv, needleValue, listValue)

    case ("member", List((_, needleValue), (_, listValue))) =>
      staticMemberSearch(StaticValueEqv.valuesAreEqual, needleValue, listValue)

    case _ =>
      None
  }
}
