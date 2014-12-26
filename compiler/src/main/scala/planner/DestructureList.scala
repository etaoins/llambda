package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.RuntimeErrorMessage

object DestructureList {
  case class DestructureResult(
    memberTemps : List[ps.TempValue],
    listTailValue : iv.IntermediateValue
  )

  /**
   * Extracts a series of values from the head of the list and returns their values along with the list's tail
   *
   * @param  listValue                  List to extract the member values from
   * @param  memberTypes                Expected types of the extracted members in list order
   * @param  insufficientLengthMessage  Error message to signal if the list has an insufficient length
   * @param  improperListMessageOpt     Error message to signal if the list is improper. This defaults to
   *                                    insufficientLengthMessage if it's not defined
   */
  def apply(
      listValue : iv.IntermediateValue,
      memberTypes : List[vt.ValueType],
      insufficientLengthMessage : RuntimeErrorMessage,
      improperListMessageOpt : Option[RuntimeErrorMessage] = None
  )(implicit plan : PlanWriter) : DestructureResult = {
    val initialState = DestructureResult(Nil, listValue)

    memberTypes.foldLeft(initialState) {
      case (DestructureResult(memberTemps, listHeadValue), memberType) =>
        // Load the member value
        val memberValue = PlanCadr.loadCar(listHeadValue, Some(insufficientLengthMessage), improperListMessageOpt)
        val memberTemp = memberValue.toTempValue(memberType)

        // We know we just loaded from a pair
        val listHeadValueType = listHeadValue.schemeType & vt.AnyPairType
        val retypedListHeadValue = listHeadValue.withSchemeType(listHeadValueType)

        val listTailValue = PlanCadr.loadCdr(retypedListHeadValue)

        DestructureResult(
          memberTemps=memberTemps :+ memberTemp,
          listTailValue=listTailValue
        )
    }
  }
}

