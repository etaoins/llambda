package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{ContextLocated, RangeException, InternalCompilerErrorException}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

import scala.annotation.tailrec

object ListProcPlanner extends StdlibProcPlanner {
  private case class SearchListItem(
      needleValue: iv.IntermediateValue,
      haystackValue: iv.IntermediateValue,
      sublist: iv.IntermediateValue,
      staticMatch: Boolean
  )

  @tailrec
  private def listToSearchItems(
      compareFunc: StaticValueEqv.EqvFunction,
      needleValue: iv.IntermediateValue,
      listValue: iv.IntermediateValue,
      acc: List[SearchListItem] = Nil
  ): Option[List[SearchListItem]] = listValue match {
    case knownPair: iv.KnownPair =>
      compareFunc(needleValue, knownPair.car) match {
        case Some(true) =>
          // Must match; don't iterate the tail in case this list become unknown later
          val searchItem = SearchListItem(needleValue, knownPair.car, knownPair, true)
          Some((searchItem :: acc).reverse)

        case Some(false) =>
          // Must not match; don't bother tracking this
          listToSearchItems(compareFunc, needleValue, knownPair.cdr, acc)

        case None =>
          // This requires a dynamic check
          val searchItem = SearchListItem(needleValue, knownPair.car, knownPair, false)

          // Constrain the type of the needle for the rest of the comparisons
          val newNeedleValue = knownPair.car.schemeType match {
            case literalValueType: vt.LiteralValueType =>
              needleValue.withSchemeType(needleValue.schemeType - literalValueType)

            case _ =>
              needleValue
          }

          listToSearchItems(compareFunc, newNeedleValue, knownPair.cdr, searchItem :: acc)
      }

    case iv.EmptyListValue =>
      Some(acc.reverse)

    case _ =>
      None
  }

  private def planListSearch(state: PlannerState)(
      dynamicCompareFunc: DynamicValueEqv.EqvFunction,
      searchItems: List[SearchListItem]
  )(implicit plan: PlanWriter): iv.IntermediateValue = searchItems match {
    case SearchListItem(_, _, sublist, true) :: _ =>
      // Known match
      sublist

    case SearchListItem(needleValue, haystackValue, sublist, false) :: tail =>
      val matchesResult = dynamicCompareFunc(state)(haystackValue, needleValue)(plan)
      val matchesPred = matchesResult.value.toTempValue(vt.Predicate)

      val truePlan = plan.forkPlan()
      val falsePlan = plan.forkPlan()

      val falseValue = planListSearch(state)(dynamicCompareFunc, tail)(falsePlan)

      val phiResult = PlanValuePhi(truePlan, sublist, falsePlan, falseValue)
      val valuePhis = phiResult.planStepPhis

      plan.steps += ps.CondBranch(matchesPred, truePlan.steps.toList, falsePlan.steps.toList, valuePhis)

      phiResult.resultValue

    case Nil =>
      iv.ConstantBooleanValue(false)
  }

  private def memberSearch(state: PlannerState)(
      staticCompareFunc: StaticValueEqv.EqvFunction,
      dynamicCompareFunc: DynamicValueEqv.EqvFunction,
      needleValue: iv.IntermediateValue,
      listValue: iv.IntermediateValue
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = {
    val searchItems = listToSearchItems(staticCompareFunc, needleValue, listValue).getOrElse {
      return None
    }

    if (searchItems.length > 4) {
      // Too complex; punt to the runtime
      return None
    }

    Some(planListSearch(state)(dynamicCompareFunc, searchItems))
  }

  @tailrec
  private def listTail(listValue: iv.IntermediateValue, index: Int): iv.IntermediateValue =
    if (index == 0) {
      listValue
    }
    else {
      listValue match {
        case knownPair: iv.KnownPair =>
          listTail(knownPair.cdr, index - 1)

        case _ =>
          throw new InternalCompilerErrorException("Invalid list passed to listTail")
      }
    }

  override def planWithValue(initialState: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("make-list", List((_, iv.ConstantIntegerValue(count)))) if count <= 4 =>
      Some(ValuesToList(List.fill(count.toInt)(iv.UnitValue)))

    case ("make-list", List((_, iv.ConstantIntegerValue(count)), (_, fillValue))) if count <= 4 =>
      Some(ValuesToList(List.fill(count.toInt)(fillValue)))

    case ("length", List((_, knownListElement: iv.KnownListElement))) =>
      knownListElement.listLengthOpt map iv.ConstantIntegerValue.apply

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
        case knownList: iv.KnownListElement =>
          knownList.toValueListOpt match {
            case Some(valueList) => valueList
            case _ =>
              return None
          }

        case _ =>
          return None
      }

      Some(ValuesToList(headValues, terminator))

    case ("memv", List((_, needleValue), (_, listValue))) =>
      memberSearch(initialState)(StaticValueEqv.valuesAreEqv, DynamicValueEqv.valuesAreEqv, needleValue, listValue)

    case ("member", List((_, needleValue), (_, listValue))) =>
      memberSearch(initialState)(StaticValueEqv.valuesAreEqual, DynamicValueEqv.valuesAreEqual, needleValue, listValue)

    case ("list-tail", List((_, listValue), (_, indexValue))) =>
      (listValue, indexValue) match {
        case (knownListElement: iv.KnownListElement, iv.ConstantIntegerValue(index)) =>
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
