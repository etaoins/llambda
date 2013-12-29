package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, InvokableProcedure}
import llambda.compiler.RuntimeErrorMessage

sealed abstract class ConstantValue(cellType : ct.ConcreteCellType) extends IntermediateValue with UninvokableValue with NonRecordValue {
  val possibleTypes = Set(cellType)
    
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue

  def toCellTempValue(targetType : ct.CellType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = {
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
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    plan.steps += stepConstructor(constantTemp, value)
    constantTemp
  }
}

class ConstantStringValue(value : String) extends TrivialConstantValue(ct.StringCell, value, ps.StoreStringCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = 
    impossibleConversion(s"Cannot convert constant string to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantSymbolValue(value : String) extends TrivialConstantValue(ct.SymbolCell, value, ps.StoreSymbolCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue =
    impossibleConversion(s"Cannot convert constant symbol to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantExactIntegerValue(value : Long) extends TrivialConstantValue(ct.ExactIntegerCell, value, ps.StoreExactIntegerCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = nativeType match {
    case intType : vt.IntType =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeInteger(constantTemp, value, intType.bits)
      constantTemp

    case fpType : vt.FpType =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeFloat(constantTemp, value.toDouble, fpType)
      constantTemp

    case _ =>
      impossibleConversion(s"Cannot convert constant exact integer to non-numeric native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Int64
}

class ConstantInexactRationalValue(value : Double) extends TrivialConstantValue(ct.InexactRationalCell, value, ps.StoreInexactRationalCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = nativeType match {
    case fpType : vt.FpType =>
      val constantTemp = new ps.TempValue
      plan.steps += ps.StoreNativeFloat(constantTemp, value, fpType)
      constantTemp

    case _ => 
      impossibleConversion(s"Cannot convert constant inexact rational to non-float native type ${nativeType.schemeName}")
  }
  
  override def preferredRepresentation : vt.ValueType =
    vt.Double
}

class ConstantCharacterValue(value : Char) extends TrivialConstantValue(ct.CharacterCell, value, ps.StoreCharacterCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = nativeType match {
    case vt.UnicodeChar =>
      val constantTemp = new ps.TempValue
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
    val predTemp = new ps.TempValue
    plan.steps += ps.StoreNativeInteger(predTemp, intValue, 1) 

    predTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue = 
    // toTruthyPredicate() will catch our conversion to bool
    impossibleConversion(s"Cannot convert constant boolean to non-boolean native type ${nativeType.schemeName}")
  
  override def preferredRepresentation : vt.ValueType =
    vt.CBool
}

class ConstantBytevectorValue(value : Vector[Short]) extends TrivialConstantValue(ct.BytevectorCell, value, ps.StoreBytevectorCell.apply) {
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue =
    // Bytevectors can't be unboxed
    impossibleConversion(s"Cannot convert constant bytevector to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantPairValue(car : ConstantValue, cdr : ConstantValue) extends ConstantValue(ct.PairCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue

    // Box our car/cdr first
    val carTemp = car.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
    val cdrTemp = cdr.toTempValue(vt.IntrinsicCellType(ct.DatumCell))

    plan.steps += ps.StorePairCell(constantTemp, carTemp, cdrTemp)

    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue =
    // Pairs cannot be unboxed
    impossibleConversion(s"Cannot convert constant pair to requested type ${nativeType.schemeName} or any other native type")
}

class ConstantVectorValue(elements : Vector[ConstantValue]) extends ConstantValue(ct.VectorCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue

    // Box our elements
    val elementTemps = elements.map {
      _.toTempValue(vt.IntrinsicCellType(ct.DatumCell))
    }

    plan.steps += ps.StoreVectorCell(constantTemp, elementTemps)

    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue =
    // Vectors cannot be unboxed
    impossibleConversion(s"Cannot convert constant vector to requested type ${nativeType.schemeName} or any other native type")
}

object EmptyListValue extends ConstantValue(ct.EmptyListCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    plan.steps += ps.StoreEmptyListCell(constantTemp)
    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue  =
    impossibleConversion(s"Cannot convert constant empty list to requested type ${nativeType.schemeName} or any other native type")
}

object UnspecificValue extends ConstantValue(ct.UnspecificCell) {
  def toConstantCellTempValue()(implicit plan : PlanWriter) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    plan.steps += ps.StoreUnspecificCell(constantTemp)
    constantTemp
  }

  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter) : ps.TempValue =
    impossibleConversion(s"Cannot convert constant unspecific to requested type ${nativeType.schemeName} or any other native type")
}

