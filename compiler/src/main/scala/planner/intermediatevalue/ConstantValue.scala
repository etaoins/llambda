package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, BoxedValue}
import llambda.compiler.{RuntimeErrorMessage, IntervalSet}


sealed abstract class ConstantValue(val cellType: ct.ConcreteCellType) extends IntermediateValue with UninvokableValue {
  val schemeType: vt.SchemeType = vt.SchemeTypeAtom(cellType)

  def isEqv(other: ConstantValue): Boolean =
    (this == other)

  def preferredRepresentation: vt.ValueType =
    schemeType

  def needsClosureRepresentation: Boolean =
    false
}

sealed abstract class TrivialConstantValue[T, U <: ps.CreateConstantCell](cellType: ct.ConcreteCellType, value: T, stepConstructor: (ps.TempValue, T) => U) extends ConstantValue(cellType) {
  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    val constantTemp = ps.TempValue()
    plan.steps += stepConstructor(constantTemp, value)

    BoxedValue(cellType, constantTemp)
  }
}

case class ConstantStringValue(value: String) extends TrivialConstantValue(ct.StringCell, value, ps.CreateStringCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant string"
}

case class ConstantSymbolValue(value: String) extends TrivialConstantValue(ct.SymbolCell, value, ps.CreateSymbolCell.apply) with BoxedOnlyValue {
  val typeDescription = "constant symbol"

  override val schemeType = vt.LiteralSymbolType(value)
}

trait ConstantNumberValue extends ConstantValue with UnboxedValue {
  def longValue: Long
  def doubleValue: Double
}

case class ConstantIntegerValue(value: Long) extends TrivialConstantValue(ct.IntegerCell, value, ps.CreateIntegerCell.apply) with ConstantNumberValue with KnownInteger {
  val typeDescription = "constant integer"
  val nativeType = vt.Int64

  def toNativeTempValue(nativeType: vt.NativeType, errorMessageOpt: Option[RuntimeErrorMessage])(implicit plan: PlanWriter): ps.TempValue = nativeType match {
    case intType: vt.IntType =>
      if ((value < intType.minIntValue) || (value > intType.maxIntValue)) {
        val message = s"Constant integer ${value} cannot be represented by native integer type ${nativeType}"
        impossibleConversion(message)
      }

      val constantTemp = ps.TempValue()
      plan.steps += ps.CreateNativeInteger(constantTemp, value, intType.bits)
      constantTemp

    case fpType: vt.FpType =>
      impossibleConversion(s"Cannot convert ${typeDescription} to floating point native type ${vt.NameForType(nativeType)}. Consider using (flonum) to explicitly convert the value.")

    case _ =>
      impossibleConversion(s"Cannot convert ${typeDescription} to non-integer native type ${vt.NameForType(nativeType)}")
  }

  def longValue: Long =
    value

  def doubleValue: Double =
    value.toDouble

  val possibleValues: IntervalSet = IntervalSet(value)
}

case class ConstantFlonumValue(value: Double) extends TrivialConstantValue(ct.FlonumCell, value, ps.CreateFlonumCell.apply) with ConstantNumberValue {
  val typeDescription = "constant flonum"
  val nativeType = vt.Double

  def toNativeTempValue(nativeType: vt.NativeType, errorMessageOpt: Option[RuntimeErrorMessage])(implicit plan: PlanWriter): ps.TempValue = nativeType match {
    case fpType: vt.FpType =>
      val constantTemp = ps.TempValue()
      plan.steps += ps.CreateNativeFloat(constantTemp, value, fpType)
      constantTemp

    case intType: vt.IntType =>
      impossibleConversion(s"Cannot convert ${typeDescription} to integer native type ${vt.NameForType(nativeType)}. Consider using (integer) to explicitly convert the value.")

    case _ =>
      impossibleConversion(s"Cannot convert ${typeDescription} to non-floating point native type ${vt.NameForType(nativeType)}")
  }

  def longValue: Long =
    value.toLong

  def doubleValue: Double =
    value

  override def isEqv(other: ConstantValue) = other match {
    case ConstantFlonumValue(otherValue) =>
      // This is needed to handle NaN
      (this.value equals otherValue)

    case _ =>
      false
  }
}

case class ConstantCharValue(value: Int) extends TrivialConstantValue(ct.CharCell, value, ps.CreateCharCell.apply) with UnboxedValue {
  val typeDescription = "constant character"
  val nativeType = vt.UnicodeChar

  def toNativeTempValue(nativeType: vt.NativeType, errorMessageOpt: Option[RuntimeErrorMessage])(implicit plan: PlanWriter): ps.TempValue = nativeType match {
    case vt.UnicodeChar =>
      val constantTemp = ps.TempValue()
      plan.steps += ps.CreateNativeInteger(constantTemp, value, vt.UnicodeChar.bits)
      constantTemp

    case _ =>
      impossibleConversion(s"Cannot convert ${typeDescription} to non-character native type ${vt.NameForType(nativeType)}")
  }
}

case class ConstantBooleanValue(value: Boolean) extends TrivialConstantValue(ct.BooleanCell, value, ps.CreateBooleanCell.apply) with UnboxedValue {
  override val schemeType = vt.LiteralBooleanType(value)
  val typeDescription = vt.NameForType(schemeType)
  val nativeType = vt.Predicate

  private val intValue = if (value) 1 else 0

  def toNativeTempValue(nativeType: vt.NativeType, errorMessageOpt: Option[RuntimeErrorMessage])(implicit plan: PlanWriter): ps.TempValue = nativeType match {
    case vt.Predicate =>
      val predTemp = ps.TempValue()
      plan.steps += ps.CreateNativeInteger(predTemp, intValue, vt.Predicate.bits)

      predTemp

    case _ =>
      impossibleConversion(s"Cannot convert ${typeDescription} to non-predicate native type ${vt.NameForType(nativeType)}")
  }
}

case class ConstantBytevectorValue(elements: Vector[Byte]) extends TrivialConstantValue(ct.BytevectorCell, elements, ps.CreateBytevectorCell.apply) with BoxedOnlyValue with KnownBytevector {
  val typeDescription = "constant bytevector"
  val bytevectorLength = elements.size
}

case class ConstantPairValue(car: ConstantValue, cdr: ConstantValue) extends ConstantValue(ct.PairCell) with BoxedOnlyValue with KnownPair {
  override val schemeType: vt.SchemeType = vt.PairType(car.schemeType, cdr.schemeType)
  val typeDescription = "constant pair"

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    val constantTemp = ps.TempValue()

    // Box our car/cdr first
    val carTemp = car.toTempValue(vt.AnySchemeType)
    val cdrTemp = cdr.toTempValue(vt.AnySchemeType)

    plan.steps += ps.CreatePairCell(constantTemp, carTemp, cdrTemp, listLengthOpt)

    BoxedValue(cellType, constantTemp)
  }
}

case class ConstantRecordValue(
    recordType: vt.RecordType,
    fieldValues: Map[vt.RecordField, ConstantValue],
    isUndefined: Boolean
) extends ConstantValue(ct.RecordCell) with BoxedOnlyValue with KnownRecord {
  override val schemeType: vt.SchemeType = recordType
  val typeDescription = s"constant ${recordType.sourceName}"

  // We know all of our field values
  val knownFieldValues = fieldValues

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    val constantTemp = ps.TempValue()

    // Box our values
    val fieldValueTemps = fieldValues map { case (field, value) =>
      val fieldType = recordType.typeForField(field)
      field -> value.toTempValue(fieldType)
    }

    plan.steps += ps.CreateRecordCell(constantTemp, recordType, fieldValueTemps, isUndefined)

    BoxedValue(cellType, constantTemp)
  }
}

case class ConstantVectorValue(
    elements: Vector[ConstantValue]
) extends ConstantValue(ct.VectorCell) with BoxedOnlyValue with KnownVector {
  val typeDescription = "constant vector"

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    val constantTemp = ps.TempValue()

    // Box our elements
    val elementTemps = elements.map {
      _.toTempValue(vt.AnySchemeType)
    }

    plan.steps += ps.CreateVectorCell(constantTemp, elementTemps)

    BoxedValue(cellType, constantTemp)
  }

  val vectorLength = elements.length.toLong
}

case object EmptyListValue extends ConstantValue(ct.EmptyListCell) with BoxedOnlyValue with KnownListElement {
  val typeDescription = "constant empty list"

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    val constantTemp = ps.TempValue()
    plan.steps += ps.CreateEmptyListCell(constantTemp)
    BoxedValue(cellType, constantTemp)
  }

  // KnownListElement implementation
  lazy val listLengthOpt = Some(0L)
  def toValueListOpt = Some(Nil)
}

case object UnitValue extends ConstantValue(ct.UnitCell) with BoxedOnlyValue {
  val typeDescription = "constant unit value"

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    val constantTemp = ps.TempValue()
    plan.steps += ps.CreateUnitCell(constantTemp)
    BoxedValue(cellType, constantTemp)
  }
}

case object UnreachableValue extends ConstantValue(ct.UnitCell) with BoxedOnlyValue {
  val typeDescription = "unreachable value"

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    val constantTemp = ps.TempValue()
    plan.steps += ps.CreateUnitCell(constantTemp)
    BoxedValue(cellType, constantTemp)
  }

  override def preferredReturnType = vt.ReturnType.Unreachable

  override def castToSchemeType(
      targetType: vt.SchemeType,
      errorMessageOpt: Option[RuntimeErrorMessage] = None,
      staticCheck: Boolean = false
  )(implicit plan: PlanWriter): IntermediateValue = this

  override def withSchemeType(newType: vt.SchemeType): IntermediateValue = this
}
