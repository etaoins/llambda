package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{ContextLocated, RangeException, InternalCompilerErrorException}
import llambda.compiler.{ErrorCategory, RuntimeErrorMessage}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

import scala.annotation.tailrec

object ListProcPlanner extends ReportProcPlanner {
  private def staticMemberSearch(
      compareFunc : StaticValueEqv.EqvFunction,
      needleValue : iv.IntermediateValue,
      listValue : iv.IntermediateValue
  ) : Option[iv.IntermediateValue] = listValue match {
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
      Some(iv.ConstantBooleanValue(false))

    case _ =>
      // Not a constant list
      None
  }

  @tailrec
  private def listTail(listValue : iv.IntermediateValue, index : Int) : iv.IntermediateValue =
    if (index == 0) {
      listValue
    }
    else {
      listValue match {
        case knownPair : iv.KnownPair =>
          listTail(knownPair.cdr, index - 1)

        case _ =>
          throw new InternalCompilerErrorException("Invalid list passed to listTail")
      }
    }

  override def planWithValue(initialState : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, args) match {
    case ("length", List((_, singleArg))) =>
      // Do we know the length at compile time?
      singleArg match {
        case knownListElement : iv.KnownListElement =>
          for(listLength <- knownListElement.listLengthOpt) {
            // Yes, return it directly
            return Some(iv.ConstantExactIntegerValue(listLength))
          }

        case _ =>
      }

      if (singleArg.isDefiniteProperList) {
        val listElementTemp = singleArg.toTempValue(vt.ListElementType)
        val resultTemp = ps.Temp(vt.UInt32)

        // This is a guaranteed proper list - we can quickly calculate the length inline
        plan.steps += ps.CalcProperListLength(resultTemp, listElementTemp)

        return Some(TempValueToIntermediate(vt.UInt32, resultTemp)(plan.config))
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
        category=ErrorCategory.MutateLiteral,
        name="setCarImmutable",
        text="(set-car!) attempted on pair literal"
      )

      plan.steps += ps.AssertPairMutable(pairTemp, errorMessage)
      plan.steps += ps.SetPairCar(pairTemp, newValueTemp)

      Some(iv.UnitValue)
    
    case ("set-cdr!", List((pairLoc, pairValue), (_, newValue))) =>
      val pairTemp = plan.withContextLocation(pairLoc) {
        pairValue.toTempValue(vt.AnyPairType)
      }

      val newValueTemp = newValue.toTempValue(vt.AnySchemeType)

      val errorMessage = RuntimeErrorMessage(
        category=ErrorCategory.MutateLiteral,
        name="setCdrImmutable",
        text="(set-cdr!) attempted on pair literal"
      )

      plan.steps += ps.AssertPairMutable(pairTemp, errorMessage)
      plan.steps += ps.SetPairCdr(pairTemp, newValueTemp)

      Some(iv.UnitValue)

    case ("cons", List((_, carValue), (_, cdrValue))) =>
      Some(ValuesToPair(carValue, cdrValue, None))
    
    case ("append", args) =>
      val (listArgs, terminator) = args.map(_._2).reverse match {
        case Nil =>
          // No arguments; return the empty list
          (Nil, iv.EmptyListValue)

        case terminator :: reverseHead =>
          (reverseHead.reverse, terminator)
      }

      val headValues = listArgs flatMap {
        case knownList : iv.KnownListElement => 
          knownList.toValueListOpt match {
            case Some(valueList) => valueList
            case _ =>
              return None
          }

        case _ =>
          return None
      }

      Some(ValuesToList(headValues, terminator))

    case (_, List((_, needleValue), (_, listValue))) if List("memq", "memv").contains(reportName) =>
      staticMemberSearch(StaticValueEqv.valuesAreEqv, needleValue, listValue)

    case ("member", List((_, needleValue), (_, listValue))) =>
      staticMemberSearch(StaticValueEqv.valuesAreEqual, needleValue, listValue)
    
    case ("list-tail", List((_, listValue), (_, indexValue))) =>
      (listValue, indexValue) match {
        case (knownListElement : iv.KnownListElement, iv.ConstantExactIntegerValue(index)) =>
          // If listLengthOpt is defined we're a proper list
          knownListElement.listLengthOpt map { listLength =>
            if ((index < 0) || (index > listLength)) {
              throw new RangeException(plan.activeContextLocated, s"List index ${index} out of bounds")
            }

            listTail(knownListElement, index.toInt)
          }

        case _ =>
          None
      }

    case _ =>
      None
  }
}
