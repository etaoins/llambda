package llambda.planner.intermediatevalue

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.StepBuffer

abstract class IntermediateValue {
  val possibleTypes : Set[bt.ConcreteBoxedType]

  case class PlanPhiResult(
    ourTempValue : ps.TempValue,
    theirTempValue : ps.TempValue,
    resultTemp : ps.TempValue,
    resultIntermediate : IntermediateValue
  )

  protected def toBoxedTempValue(boxedType : bt.BoxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue]
  protected def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue]

  def toTruthyPredicate()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val trueTemp = new ps.TempValue
    planSteps += ps.StoreNativeInteger(trueTemp, 1, 1) 

    trueTemp
  }

  protected def toTempValue(targetType : nfi.NativeType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = targetType match {
    case nfi.CTruthyBool =>
      val truthyPredTemp = toTruthyPredicate()

      val intConvTemp = new ps.TempValue
      planSteps += ps.ConvertNativeInteger(intConvTemp, truthyPredTemp, nfi.CTruthyBool.bits, false)

      Some(intConvTemp)

    case nfi.BoxedValue(boxedType) =>
      toBoxedTempValue(boxedType)

    case unboxedType : nfi.UnboxedType =>
      toUnboxedTempValue(unboxedType)
  }
  
  def toRequiredTempValue(targetType : nfi.NativeType)(implicit planSteps : StepBuffer) =
    toTempValue(targetType) getOrElse {
      throw new ImpossibleTypeConversionException(s"Unable to convert ${this.toString} to ${targetType}")
    }

  def planPhiWith(theirValue : IntermediateValue)(ourSteps : StepBuffer, theirSteps : StepBuffer) : PlanPhiResult = {
    // This is extremely inefficient for compatible unboxed types
    // This should be overridden where possible
    val ourTempValue = this.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))(ourSteps)
    val theirTempValue = theirValue.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))(theirSteps)

    val phiResultTemp = new ps.TempValue
    val phiPossibleTypes = possibleTypes ++ theirValue.possibleTypes

    PlanPhiResult(
      ourTempValue=ourTempValue,
      theirTempValue=theirTempValue,
      resultTemp=phiResultTemp,
      resultIntermediate=new DynamicBoxedValue(phiPossibleTypes, bt.BoxedDatum, phiResultTemp)
    )
  }
}

