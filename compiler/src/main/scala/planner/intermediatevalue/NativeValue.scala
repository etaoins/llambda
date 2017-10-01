package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, BoxedValue, AssertIntInRange}
import llambda.compiler.{RuntimeErrorMessage, IntervalSet}

sealed abstract class NativeValue(
    val nativeType: vt.NativeType,
    val cellType: ct.ConcreteCellType,
    val tempValue: ps.TempValue
) extends IntermediateValue with UnapplicableValue with UnboxedValue {
  val schemeType = vt.SchemeTypeAtom(cellType)

  lazy val typeDescription =
    s"native value of type ${vt.NameForType(nativeType)}"

  protected def planCastToNativeTempValue(targetType: vt.NativeType)(implicit plan: PlanWriter): ps.TempValue =
    impossibleConversion(s"Cannot convert ${typeDescription} to requested type ${vt.NameForType(targetType)} or any other native type")

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue

  def toNativeTempValue(targetType: vt.NativeType, errorMessageOpt: Option[RuntimeErrorMessage])(implicit plan: PlanWriter): ps.TempValue =
    if (targetType == nativeType) {
      tempValue
    }
    else {
      planCastToNativeTempValue(targetType)
    }

  def needsClosureRepresentation =
    true
}

class NativePredicateValue(tempValue: ps.TempValue) extends NativeValue(vt.Predicate, ct.BooleanCell, tempValue) {
  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue =  {
    val boxedTemp = ps.TempValue()
    plan.steps += ps.BoxBoolean(boxedTemp, tempValue)

    BoxedValue(cellType, boxedTemp)
  }
}

class NativeIntegerValue(
    tempValue: ps.TempValue,
    override val nativeType: vt.IntType
)
(
    val possibleValues: IntervalSet = IntervalSet(nativeType.valueInterval)
) extends NativeValue(nativeType, ct.IntegerCell, tempValue) with KnownInteger {
  override def planCastToNativeTempValue(targetType: vt.NativeType)(implicit plan: PlanWriter): ps.TempValue = targetType match {
    case intType: vt.IntType =>
      AssertIntInRange(tempValue, nativeType, intType)(possibleValues)

      val convTemp = ps.TempValue()
      plan.steps += ps.ConvertNativeInteger(convTemp, tempValue, intType.bits, nativeType.signed)

      convTemp

    case _ =>
      impossibleConversion(s"Cannot convert ${typeDescription} to non-integer native type ${vt.NameForType(targetType)}")
  }

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue = {
    // We can only box 64bit signed ints
    val boxedTemp = ps.TempValue()
    plan.steps += ps.BoxInteger(boxedTemp, toTempValue(vt.Int64))

    BoxedValue(cellType, boxedTemp)
  }
}

class NativeFlonumValue(tempValue: ps.TempValue, nativeType: vt.FpType) extends NativeValue(nativeType, ct.FlonumCell, tempValue) {
  override def planCastToNativeTempValue(targetType: vt.NativeType)(implicit plan: PlanWriter): ps.TempValue = targetType match {
    case fpType: vt.FpType =>
      val convTemp = ps.TempValue()
      plan.steps += ps.ConvertNativeFloat(convTemp, tempValue, fpType)

      convTemp

    case _ =>
      impossibleConversion(s"Cannot convert native floating value of type ${vt.NameForType(nativeType)} to non-float native type ${vt.NameForType(targetType)}")
  }

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue =  {
    // We can only box doubles
    val boxedTemp = ps.TempValue()
    plan.steps += ps.BoxFlonum(boxedTemp, toTempValue(vt.Double))

    BoxedValue(cellType, boxedTemp)
  }
}

class NativeCharValue(tempValue: ps.TempValue) extends NativeValue(vt.UnicodeChar, ct.CharCell, tempValue) {
  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue =  {
    val boxedTemp = ps.TempValue()
    plan.steps += ps.BoxChar(boxedTemp, tempValue)

    BoxedValue(cellType, boxedTemp)
  }
}
