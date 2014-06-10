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
      impossibleConversion(s"Cannot convert ${typeDescription} to incompatible type ${targetType.schemeName}")
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

sealed abstract class TrivialConstantValue[T, U <: ps.CreateConstantCell](cellType : ct.ConcreteCellType, value : T, stepConstructor : (ps.TempValue, T) => U) extends ConstantValue(cellType) {
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)
    plan.steps += stepConstructor(constantTemp, value)
    constantTemp
  }
}

class ConstantStringValue(value : String) extends TrivialConstantValue(ct.StringCell, value, ps.CreateStringCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant string"
}

class ConstantSymbolValue(value : String) extends TrivialConstantValue(ct.SymbolCell, value, ps.CreateSymbolCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant symbol"
}

class ConstantExactIntegerValue(value : Long) extends TrivialConstantValue(ct.ExactIntegerCell, value, ps.CreateExactIntegerCell.apply) {
  val typeDescription = "constant exact integer"

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = nativeType match {
    case intType : vt.IntType =>
      val constantTemp = ps.Temp(intType)
      plan.steps += ps.CreateNativeInteger(constantTemp, value, intType.bits)
      constantTemp

    case fpType : vt.FpType =>
      val constantTemp = ps.Temp(fpType)
      plan.steps += ps.CreateNativeFloat(constantTemp, value.toDouble, fpType)
      constantTemp

    case _ =>
      impossibleConversion(s"Cannot convert ${typeDescription} to non-numeric native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Int64
}

class ConstantInexactRationalValue(value : Double) extends TrivialConstantValue(ct.InexactRationalCell, value, ps.CreateInexactRationalCell.apply) {
  val typeDescription = "constant inexact rational"

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = nativeType match {
    case fpType : vt.FpType =>
      val constantTemp = ps.Temp(fpType)
      plan.steps += ps.CreateNativeFloat(constantTemp, value, fpType)
      constantTemp

    case _ => 
      impossibleConversion(s"Cannot convert ${typeDescription} to non-float native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Double
}

class ConstantCharacterValue(value : Char) extends TrivialConstantValue(ct.CharacterCell, value, ps.CreateCharacterCell.apply) {
  val typeDescription = "constant character"

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = nativeType match {
    case vt.UnicodeChar =>
      val constantTemp = ps.Temp(vt.UnicodeChar)
      plan.steps += ps.CreateNativeInteger(constantTemp, value, vt.UnicodeChar.bits)
      constantTemp

    case _ =>
      impossibleConversion(s"Cannot convert ${typeDescription} to non-character native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.UnicodeChar
}

class ConstantBooleanValue(value : Boolean) extends TrivialConstantValue(ct.BooleanCell, value, ps.CreateBooleanCell.apply) {
  val typeDescription = "constant boolean"

  private val intValue = if (value) 1 else 0

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val predTemp = ps.Temp(vt.Predicate)
    plan.steps += ps.CreateNativeInteger(predTemp, intValue, 1) 

    predTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = 
    // toTruthyPredicate() will catch our conversion to bool
    impossibleConversion(s"Cannot convert ${typeDescription} to non-boolean native type ${nativeType.schemeName}")
  
  override def preferredRepresentation : vt.ValueType =
    vt.CBool
}

class ConstantBytevectorValue(value : Vector[Short]) extends TrivialConstantValue(ct.BytevectorCell, value, ps.CreateBytevectorCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant bytevector"
}

class ConstantPairValue(car : ConstantValue, cdr : ConstantValue, val listMetricsOpt : Option[ConstantListMetrics]) extends ConstantValue(ct.PairCell) with BoxedOnlyValue {
  val typeDescription = "constant pair"

  override val isDefiniteProperList = listMetricsOpt.isDefined
  
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)

    // Box our car/cdr first
    val carTemp = car.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
    val cdrTemp = cdr.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

    plan.steps += ps.CreatePairCell(constantTemp, carTemp, cdrTemp, listMetricsOpt.map(_.length), listMetricsOpt.flatMap(_.memberType))

    constantTemp
  }
}

class ConstantVectorValue(elements : Vector[ConstantValue]) extends ConstantValue(ct.VectorCell) with BoxedOnlyValue {
  val typeDescription = "constant vector"

  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)

    // Box our elements
    val elementTemps = elements.map {
      _.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
    }

    plan.steps += ps.CreateVectorCell(constantTemp, elementTemps)

    constantTemp
  }
}

object EmptyListValue extends ConstantValue(ct.EmptyListCell) with BoxedOnlyValue {
  val typeDescription = "constant empty list"

  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)
    plan.steps += ps.CreateEmptyListCell(constantTemp)
    constantTemp
  }
}

object UnitValue extends ConstantValue(ct.UnitCell) with BoxedOnlyValue {
  val typeDescription = "constant unit value"

  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)
    plan.steps += ps.CreateUnitCell(constantTemp)
    constantTemp
  }
}

