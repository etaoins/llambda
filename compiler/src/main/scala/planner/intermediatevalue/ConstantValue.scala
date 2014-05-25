package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.RuntimeErrorMessage

case class ConstantListMetrics(
  length : Long,
  memberType : Option[ct.ConcreteCellType]
)

sealed abstract class ConstantValue(val cellType : ct.ConcreteCellType) extends IntermediateValue with UninvokableValue with NonRecordValue {
  val possibleTypes = Set(cellType)
    
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue

  def toCellTempValue(targetType : ct.CellType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    if (!targetType.isTypeOrSupertypeOf(cellType)) {
      impossibleConversion(s"Cannot convert constant of type ${cellType.schemeName} to incompatible type ${targetType.schemeName}")
    }

    val boxedTempValue = toConstantCellTempValue()
    
    cellTempToSupertype(boxedTempValue, cellType, targetType)
  }
  
  def preferredRepresentation : vt.ValueType =
    vt.IntrinsicCellType(cellType)
  
  def closureRepresentation : Option[vt.ValueType] = 
    // Constants don't need to be captured
    None
}

sealed abstract class TrivialConstantValue[T, U <: ps.StoreConstantCell](cellType : ct.ConcreteCellType, value : T, stepConstructor : (ps.TempValue, T) => U) extends ConstantValue(cellType) {
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)
    plan.steps += stepConstructor(constantTemp, value)
    constantTemp
  }
}

class ConstantStringValue(value : String) extends TrivialConstantValue(ct.StringCell, value, ps.StoreStringCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = 
    impossibleConversion(s"Cannot convert constant string to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantSymbolValue(value : String) extends TrivialConstantValue(ct.SymbolCell, value, ps.StoreSymbolCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    impossibleConversion(s"Cannot convert constant symbol to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantExactIntegerValue(value : Long) extends TrivialConstantValue(ct.ExactIntegerCell, value, ps.StoreExactIntegerCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = nativeType match {
    case intType : vt.IntType =>
      val constantTemp = ps.Temp(intType)
      plan.steps += ps.StoreNativeInteger(constantTemp, value, intType.bits)
      constantTemp

    case fpType : vt.FpType =>
      val constantTemp = ps.Temp(fpType)
      plan.steps += ps.StoreNativeFloat(constantTemp, value.toDouble, fpType)
      constantTemp

    case _ =>
      impossibleConversion(s"Cannot convert constant exact integer to non-numeric native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Int64
}

class ConstantInexactRationalValue(value : Double) extends TrivialConstantValue(ct.InexactRationalCell, value, ps.StoreInexactRationalCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = nativeType match {
    case fpType : vt.FpType =>
      val constantTemp = ps.Temp(fpType)
      plan.steps += ps.StoreNativeFloat(constantTemp, value, fpType)
      constantTemp

    case _ => 
      impossibleConversion(s"Cannot convert constant inexact rational to non-float native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Double
}

class ConstantCharacterValue(value : Char) extends TrivialConstantValue(ct.CharacterCell, value, ps.StoreCharacterCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = nativeType match {
    case vt.UnicodeChar =>
      val constantTemp = ps.Temp(vt.UnicodeChar)
      plan.steps += ps.StoreNativeInteger(constantTemp, value, vt.UnicodeChar.bits)
      constantTemp

    case _ =>
      impossibleConversion(s"Cannot convert constant character to non-character native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.UnicodeChar
}

class ConstantBooleanValue(value : Boolean) extends TrivialConstantValue(ct.BooleanCell, value, ps.StoreBooleanCell.apply) {
  private val intValue = if (value) 1 else 0

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val predTemp = ps.Temp(vt.Predicate)
    plan.steps += ps.StoreNativeInteger(predTemp, intValue, 1) 

    predTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = 
    // toTruthyPredicate() will catch our conversion to bool
    impossibleConversion(s"Cannot convert constant boolean to non-boolean native type ${nativeType.schemeName}")
  
  override def preferredRepresentation : vt.ValueType =
    vt.CBool
}

class ConstantBytevectorValue(value : Vector[Short]) extends TrivialConstantValue(ct.BytevectorCell, value, ps.StoreBytevectorCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    // Bytevectors can't be unboxed
    impossibleConversion(s"Cannot convert constant bytevector to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantPairValue(car : ConstantValue, cdr : ConstantValue, val listMetricsOpt : Option[ConstantListMetrics]) extends ConstantValue(ct.PairCell) {
  override val isDefiniteProperList = listMetricsOpt.isDefined
  
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)

    // Box our car/cdr first
    val carTemp = car.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
    val cdrTemp = cdr.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

    plan.steps += ps.StorePairCell(constantTemp, carTemp, cdrTemp, listMetricsOpt.map(_.length), listMetricsOpt.flatMap(_.memberType))

    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    // Pairs cannot be unboxed
    impossibleConversion(s"Cannot convert constant pair to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantVectorValue(elements : Vector[ConstantValue]) extends ConstantValue(ct.VectorCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)

    // Box our elements
    val elementTemps = elements.map {
      _.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
    }

    plan.steps += ps.StoreVectorCell(constantTemp, elementTemps)

    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    // Vectors cannot be unboxed
    impossibleConversion(s"Cannot convert constant vector to requested type ${nativeType.schemeName} or any other native type")
}

object EmptyListValue extends ConstantValue(ct.EmptyListCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)
    plan.steps += ps.StoreEmptyListCell(constantTemp)
    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue  =
    impossibleConversion(s"Cannot convert constant empty list to requested type ${nativeType.schemeName} or any other native type")
}

object UnitValue extends ConstantValue(ct.UnitCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)
    plan.steps += ps.StoreUnitCell(constantTemp)
    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    impossibleConversion(s"Cannot convert constant unit value to requested type ${nativeType.schemeName} or any other native type")
}

