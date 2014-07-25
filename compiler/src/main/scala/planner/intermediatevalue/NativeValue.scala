package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, BoxedValue}
import llambda.compiler.RuntimeErrorMessage

sealed abstract class NativeValue(val nativeType : vt.NativeType, val cellType : ct.ConcreteCellType, val tempValue : ps.TempValue) extends IntermediateValue with UninvokableValue {
  val schemeType = vt.SchemeTypeAtom(cellType)

  lazy val typeDescription = 
    s"native value of type ${nativeType.schemeName}"

  // This is used for our shortcut in planPhiWith to build a new phi'ed intermediate
  protected def withNewTempValue(tempValue : ps.TempValue) : NativeValue

  protected def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : ps.TempValue = 
    impossibleConversion(s"Cannot convert ${typeDescription} to requested type ${targetType.schemeName} or any other native type")
  
  protected def planCastToSchemeTempValue(targetType : vt.SchemeType)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue  = 
    impossibleConversion(s"Cannot convert ${typeDescription} to requested type ${targetType.schemeName} or any other Scheme type except ${cellType.schemeName}")
  
  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue

  def toNativeTempValue(targetType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = 
    if (targetType == nativeType) {
      tempValue
    }
    else {
      planCastToNativeTempValue(targetType)
    }
  
  override def planPhiWith(theirValue : IntermediateValue)(ourPlan : PlanWriter, theirPlan : PlanWriter)(implicit worldPtr : ps.WorldPtrValue) : PlanPhiResult = theirValue match {
    case theirUnboxed : NativeValue if nativeType == theirUnboxed.nativeType =>
      // Our types exactly match - no conversion needed!
      val phiResultTemp = ps.Temp(nativeType)

      PlanPhiResult(
        ourTempValue=tempValue,
        theirTempValue=theirUnboxed.tempValue,
        resultTemp=phiResultTemp,
        resultIntermediate=this.withNewTempValue(phiResultTemp)
      )

    case _ =>
      // Fall back to dumb unboxing
      super.planPhiWith(theirValue)(ourPlan, theirPlan)
  }
  
  def preferredRepresentation : vt.ValueType =
    nativeType
  
  def needsClosureRepresentation =
    true
}

class NativePredicateValue(tempValue : ps.TempValue) extends NativeValue(vt.Predicate, ct.BooleanCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativePredicateValue(tempValue)

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    tempValue
  }
  
  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue =  {
    val boxedTemp = ps.CellTemp(ct.BooleanCell)
    plan.steps += ps.BoxBoolean(boxedTemp, tempValue)

    BoxedValue(cellType, boxedTemp)
  }
}

class NativeExactIntegerValue(tempValue : ps.TempValue, nativeType : vt.IntType) extends NativeValue(nativeType, ct.ExactIntegerCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeExactIntegerValue(tempValue, nativeType)

  override def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : ps.TempValue = targetType match {
    case intType : vt.IntType =>
      val convTemp = ps.Temp(nativeType)
      plan.steps += ps.ConvertNativeInteger(convTemp, tempValue, intType.bits, intType.signed)

      convTemp

    case _ => 
      impossibleConversion(s"Cannot convert ${typeDescription} to non-integer native type ${targetType.schemeName}") 
  }
  
  override def planCastToSchemeTempValue(targetType : vt.SchemeType)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    // Will an inexact rational make this conversion happy?
    if (vt.SatisfiesType(targetType, vt.InexactRationalType) == Some(true)) {
      // Convert us to double and box
      val boxedTemp = ps.CellTemp(ct.InexactRationalCell)
      
      plan.steps += ps.BoxInexactRational(boxedTemp, toTempValue(vt.Double))

      BoxedValue(ct.InexactRationalCell, boxedTemp).castToCellTempValue(targetType.cellType)
    }
    else {
      impossibleConversion(s"Cannot convert ${typeDescription} to non-numeric cell type ${cellType.schemeName}") 
    }
  }

  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue = {
    // We can only box 64bit signed ints
    val boxedTemp = ps.CellTemp(ct.ExactIntegerCell)
    plan.steps += ps.BoxExactInteger(boxedTemp, toTempValue(vt.Int64))

    BoxedValue(cellType, boxedTemp)
  }
}

class NativeInexactRationalValue(tempValue : ps.TempValue, nativeType : vt.FpType) extends NativeValue(nativeType, ct.InexactRationalCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeInexactRationalValue(tempValue, nativeType)

  override def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : ps.TempValue = targetType match {
    case fpType : vt.FpType =>
      val convTemp = ps.Temp(fpType)
      plan.steps += ps.ConvertNativeFloat(convTemp, tempValue, fpType)

      convTemp

    case _ => 
      impossibleConversion(s"Cannot convert native floating value of type ${nativeType.schemeName} to non-float native type ${targetType.schemeName}") 
  }
  
  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue =  {
    // We can only box doubles
    val boxedTemp = ps.CellTemp(ct.InexactRationalCell)
    plan.steps += ps.BoxInexactRational(boxedTemp, toTempValue(vt.Double))

    BoxedValue(cellType, boxedTemp)
  }
}

class NativeCharacterValue(tempValue : ps.TempValue) extends NativeValue(vt.UnicodeChar, ct.CharacterCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeCharacterValue(tempValue)

  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue =  {
    val boxedTemp = ps.CellTemp(cellType)
    plan.steps += ps.BoxCharacter(boxedTemp, tempValue)

    BoxedValue(cellType, boxedTemp)
  }
}
