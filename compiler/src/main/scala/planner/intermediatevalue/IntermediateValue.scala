package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.RuntimeErrorMessage
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, TempValueToIntermediate, InvokableProcedure}
import llambda.compiler.ImpossibleTypeConversionException
import llambda.compiler.InternalCompilerErrorException

trait IntermediateValueHelpers {
  /** Helper for signalling impossible conversions */
  protected def impossibleConversion(message : String)(implicit plan : PlanWriter) = { 
    throw new ImpossibleTypeConversionException(plan.activeContextLocated, message)
  }
  
  /** Converts the temp value of the given actual type to the passed super type
    *
    * This is a noop if the actual type matches the supertype
    */
  protected def cellTempToSupertype(cellTemp : ps.TempValue, actualType : ct.CellType, supertype : ct.CellType)(implicit plan : PlanWriter) : ps.TempValue = 
    if (actualType != supertype) {
      // Cast this to super

      // This only needs to be GC managed if the original is GC managed
      // This prevents us from rooting super casts of constant cells
      val castTemp = new ps.TempValue(cellTemp.isGcManaged)
      plan.steps += ps.CastCellToTypeUnchecked(castTemp, cellTemp, supertype)

      castTemp
    }
    else {
      cellTemp
    }
}

abstract class IntermediateValue extends IntermediateValueHelpers {
  val possibleTypes : Set[ct.ConcreteCellType]
  
  /** Provides a human-readable description of the value's type */
  def typeDescription : String

  val isDefiniteProperList : Boolean =
    possibleTypes == Set(ct.EmptyListCell)

  case class PlanPhiResult(
    ourTempValue : ps.TempValue,
    theirTempValue : ps.TempValue,
    resultTemp : ps.TempValue,
    resultIntermediate : IntermediateValue
  )

  protected def toCellTempValue(cellType : ct.CellType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue
  protected def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue
  protected def toRecordTempValue(recordType : vt.RecordType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue

  def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val trueTemp = ps.Temp(vt.Predicate)
    plan.steps += ps.CreateNativeInteger(trueTemp, 1, 1) 

    trueTemp
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[InvokableProcedure]

  def toTempValue(targetType : vt.ValueType, errorMessageOpt : Option[RuntimeErrorMessage] = None)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = targetType match {
    case vt.CBool =>
      val truthyPredTemp = toTruthyPredicate()

      val intConvTemp = ps.Temp(vt.CBool)
      plan.steps += ps.ConvertNativeInteger(intConvTemp, truthyPredTemp, vt.CBool.bits, false)

      intConvTemp

    case nativeType : vt.NativeType =>
      toNativeTempValue(nativeType, errorMessageOpt)

    case vt.IntrinsicCellType(cellType) =>
      toCellTempValue(cellType, errorMessageOpt)

    case recordType : vt.RecordType =>
      toRecordTempValue(recordType, errorMessageOpt)

    case closureType : vt.ClosureType =>
      // Closure types are an internal implementation detail.
      // Nothing should attempt to convert to them
      throw new InternalCompilerErrorException("Attempt to convert value to a closure type")
  }
  
  def planPhiWith(theirValue : IntermediateValue)(ourPlan : PlanWriter, theirPlan : PlanWriter)(implicit worldPtr : ps.WorldPtrValue) : PlanPhiResult = {
    // This is extremely inefficient for compatible native types
    // This should be overridden where possible
    val ourTempValue = this.toTempValue(vt.IntrinsicCellType(ct.DatumCell))(ourPlan, worldPtr)
    val theirTempValue = theirValue.toTempValue(vt.IntrinsicCellType(ct.DatumCell))(theirPlan, worldPtr)

    // If we're constants on both sides we don't need to be GC managed
    val isGcManaged = ourTempValue.isGcManaged || theirTempValue.isGcManaged

    val phiResultTemp = new ps.TempValue(isGcManaged)
    val phiPossibleTypes = possibleTypes ++ theirValue.possibleTypes

    PlanPhiResult(
      ourTempValue=ourTempValue,
      theirTempValue=theirTempValue,
      resultTemp=phiResultTemp,
      resultIntermediate=new IntrinsicCellValue(phiPossibleTypes, ct.DatumCell, phiResultTemp)
    )
  }

  /** Casts this value to the specified cell value type
    *
    * The result may not be of represented by the specified cell value type (e.g. it may be unboxed) but it is 
    * guaranteed to be convertable to that type. toTempValue should be used when a particular representation is 
    * explicitly required
    */
  def castToSchemeType(targetType : vt.CellValueType)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : IntermediateValue= {
    targetType match {
      case vt.IntrinsicCellType(cellType) =>
        val targetConcreteTypes = cellType.concreteTypes

        if (possibleTypes.subsetOf(targetConcreteTypes)) {
          // We don't need to do anything 
          return this
        }

      case _ =>
    }

    val castTemp = toTempValue(targetType)
    TempValueToIntermediate(targetType, castTemp)
  }

  /** Returns the preferred type to represent this value
    * 
    * For realized values this will be the type of the TempValue. For unrealized values such as constants and known
    * procedures this will be the type that can specifically represent the value with the minimum of boxing and
    * conversion.
    */
  def preferredRepresentation : vt.ValueType

  /** Indicates if this values need to be stored inside a closure
    *
    * If false is returned the value does not need to be captured and can instead be used directly without storing any
    * data in the closure.
    */
  def needsClosureRepresentation : Boolean

  /** Restores this value from a closure's ps.TempValue
    *
    * This can be overriden to carry value-specific metadata that isn't contained in the value's type alone. This can
    * include things like procedure signature, value ranges, etc.
    */
  def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    TempValueToIntermediate(valueType, varTemp) 
  }
}

