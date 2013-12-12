package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, UnlocatedImpossibleTypeConversionException, InvokableProcedure}

abstract class IntermediateValue {
  val possibleTypes : Set[ct.ConcreteCellType]

  case class PlanPhiResult(
    ourTempValue : ps.TempValue,
    theirTempValue : ps.TempValue,
    resultTemp : ps.TempValue,
    resultIntermediate : IntermediateValue
  )

  protected def toCellTempValue(cellType : ct.CellType)(implicit plan : PlanWriter) : Option[ps.TempValue]
  protected def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue]
  protected def toRecordTempValue(recordType : vt.RecordType)(implicit plan : PlanWriter) : Option[ps.TempValue]

  def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val trueTemp = new ps.TempValue
    plan.steps += ps.StoreNativeInteger(trueTemp, 1, 1) 

    trueTemp
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure]

  protected def toTempValue(targetType : vt.ValueType)(implicit plan : PlanWriter) : Option[ps.TempValue] = targetType match {
    case vt.CBool =>
      val truthyPredTemp = toTruthyPredicate()

      val intConvTemp = new ps.TempValue
      plan.steps += ps.ConvertNativeInteger(intConvTemp, truthyPredTemp, vt.CBool.bits, false)

      Some(intConvTemp)

    case nativeType : vt.NativeType =>
      toNativeTempValue(nativeType)

    case vt.IntrinsicCellType(cellType) =>
      toCellTempValue(cellType)

    case recordType : vt.RecordType =>
      toRecordTempValue(recordType)

    case closureType : vt.ClosureType =>
      // Closure types are an internal implementation detail.
      // Nothing should attempt to convert to them
      None
  }
  
  def toRequiredTempValue(targetType : vt.ValueType)(implicit plan : PlanWriter) =
    toTempValue(targetType) getOrElse {
      throw new UnlocatedImpossibleTypeConversionException(s"Unable to convert ${this.toString} to ${targetType}")
    }

  def planPhiWith(theirValue : IntermediateValue)(ourPlan : PlanWriter, theirPlan : PlanWriter) : PlanPhiResult = {
    // This is extremely inefficient for compatible native types
    // This should be overridden where possible
    val ourTempValue = this.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))(ourPlan)
    val theirTempValue = theirValue.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))(theirPlan)

    val phiResultTemp = new ps.TempValue
    val phiPossibleTypes = possibleTypes ++ theirValue.possibleTypes

    PlanPhiResult(
      ourTempValue=ourTempValue,
      theirTempValue=theirTempValue,
      resultTemp=phiResultTemp,
      resultIntermediate=new IntrinsicCellValue(phiPossibleTypes, ct.DatumCell, phiResultTemp)
    )
  }

  /** Returns the preferred type to represent this value
    * 
    * For realized values this will be the type of the TempValue. For unrealized
    * values such as constants and known procedures this will be the type
    * that can specifically represent the value with the minimum of boxing
    * and conversion.
    */
  def preferredRepresentation : vt.ValueType

  /** Returns the type that should be used to capture this value inside a 
    * closure
    *
    * If None is returned the value does not need to be captured and can instead 
    * be used directly without storing any data in the closure.
    */
  def closureRepresentation : Option[vt.ValueType]
}

