package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.RuntimeErrorMessage

sealed abstract class ConstantValue(val cellType : ct.ConcreteCellType) extends IntermediateValue with UninvokableValue {
  val schemeType : vt.SchemeType = vt.SchemeTypeAtom(cellType)
    
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue

  def toSchemeTempValue(targetType : vt.SchemeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    if (schemeType.satisfiesType(targetType) != Some(true)) {
      impossibleConversion(s"Cannot convert ${typeDescription} to incompatible type ${targetType.schemeName}")
    }

    val boxedTempValue = toConstantCellTempValue()
    
    cellTempToSupertype(boxedTempValue, cellType, targetType.cellType)
  }
  
  def preferredRepresentation : vt.ValueType =
    schemeType
  
  def needsClosureRepresentation : Boolean =
    false
}

sealed abstract class TrivialConstantValue[T, U <: ps.CreateConstantCell](cellType : ct.ConcreteCellType, value : T, stepConstructor : (ps.TempValue, T) => U) extends ConstantValue(cellType) {
  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)
    plan.steps += stepConstructor(constantTemp, value)
    constantTemp
  }
}

class ConstantStringValue(val value : String) extends TrivialConstantValue(ct.StringCell, value, ps.CreateStringCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant string"
}

class ConstantSymbolValue(val value : String) extends TrivialConstantValue(ct.SymbolCell, value, ps.CreateSymbolCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant symbol"
}

class ConstantExactIntegerValue(val value : Long) extends TrivialConstantValue(ct.ExactIntegerCell, value, ps.CreateExactIntegerCell.apply) {
  val typeDescription = "constant exact integer"
  
  override def toSchemeTempValue(targetType : vt.SchemeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    if ((vt.ExactIntegerType.satisfiesType(targetType).get == false) &&
        (vt.InexactRationalType.satisfiesType(targetType).get == true)) {
      // Do a special implicit cast to an inexact cell
      val constantTemp = ps.CellTemp(ct.InexactRationalCell, knownConstant=true)
      plan.steps += ps.CreateInexactRationalCell(constantTemp, value.toDouble)
    
      cellTempToSupertype(constantTemp, ct.InexactRationalCell, targetType.cellType)
    }
    else {
      super.toSchemeTempValue(targetType, errorMessageOpt)
    }
  }

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

class ConstantInexactRationalValue(val value : Double) extends TrivialConstantValue(ct.InexactRationalCell, value, ps.CreateInexactRationalCell.apply) {
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

class ConstantCharacterValue(val value : Char) extends TrivialConstantValue(ct.CharacterCell, value, ps.CreateCharacterCell.apply) {
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

class ConstantBooleanValue(val value : Boolean) extends TrivialConstantValue(ct.BooleanCell, value, ps.CreateBooleanCell.apply) {
  override val schemeType = vt.ConstantBooleanType(value)
  val typeDescription = schemeType.schemeName

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

class ConstantBytevectorValue(val elements : Vector[Short]) extends TrivialConstantValue(ct.BytevectorCell, elements, ps.CreateBytevectorCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant bytevector"
}

class ConstantPairValue(val car : ConstantValue, val cdr : ConstantValue) extends ConstantValue(ct.PairCell) with BoxedOnlyValue with KnownPair {
  val typeDescription = "constant pair"

  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)

    // Box our car/cdr first
    val carTemp = car.toTempValue(vt.AnySchemeType)
    val cdrTemp = cdr.toTempValue(vt.AnySchemeType)

    plan.steps += ps.CreatePairCell(constantTemp, carTemp, cdrTemp, listLengthOpt)

    constantTemp
  }
}

class ConstantVectorValue(val elements : Vector[ConstantValue]) extends ConstantValue(ct.VectorCell) with BoxedOnlyValue {
  val typeDescription = "constant vector"

  def toConstantCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val constantTemp = ps.CellTemp(cellType, knownConstant=true)

    // Box our elements
    val elementTemps = elements.map {
      _.toTempValue(vt.AnySchemeType)
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

