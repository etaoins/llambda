package llambda.planner.intermediatevalue

import llambda.{valuetype => vt}
import llambda.{celltype => ct}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, UnlocatedImpossibleTypeConversionException, InvokableProcedure}

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
  protected def toRecordCellTempValue(recordCellType : vt.RecordCellType)(implicit plan : PlanWriter) : Option[ps.TempValue]

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

    case recordCellType : vt.RecordCellType =>
      toRecordCellTempValue(recordCellType)
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
}

