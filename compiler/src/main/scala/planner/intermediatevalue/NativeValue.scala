package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.PlanWriter
import llambda.compiler.RuntimeErrorMessage

sealed abstract class NativeValue(val nativeType : vt.NativeType, val cellType : ct.ConcreteCellType, val tempValue : ps.TempValue) extends IntermediateValue with UninvokableValue with NonRecordValue {
  val possibleTypes = Set(cellType)

  // This is used for our shortcut in planPhiWith to build a new phi'ed intermediate
  protected def withNewTempValue(tempValue : ps.TempValue) : NativeValue

  protected def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : ps.TempValue = 
    impossibleConversion(s"Cannot convert native value of type ${nativeType.schemeName} to requested type ${targetType.schemeName} or any other native type")
  
  protected def planCastToCellTempValue(targetType : ct.CellType)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue  = 
    impossibleConversion(s"Cannot convert native value of type ${nativeType.schemeName} to requested type ${targetType.schemeName} or any other cell type except ${cellType.schemeName}")
  
  protected def planCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue

  def toNativeTempValue(targetType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = 
    if (targetType == nativeType) {
      tempValue
    }
    else {
      planCastToNativeTempValue(targetType)
    }
  
  def toCellTempValue(targetType : ct.CellType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    if (targetType.isTypeOrSupertypeOf(cellType)) {
      val boxedTemp = planCellTempValue()

      cellTempToSupertype(boxedTemp, cellType, targetType)
    }
    else {
      planCastToCellTempValue(targetType)
    }
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
  
  def closureRepresentation : Option[vt.ValueType] = 
    Some(nativeType)
}

class NativeBooleanValue(tempValue : ps.TempValue) extends NativeValue(vt.CBool, ct.BooleanCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeBooleanValue(tempValue)

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val predTemp = ps.Temp(nativeType)
    plan.steps += ps.ConvertNativeInteger(predTemp, tempValue, 1, false) 

    predTemp
  }
  
  def planCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =  {
    val boxedTemp = ps.CellTemp(ct.BooleanCell)
    plan.steps += ps.BoxBoolean(boxedTemp, toTruthyPredicate())

    boxedTemp
  }
}

class NativeExactIntegerValue(tempValue : ps.TempValue, nativeType : vt.IntType) extends NativeValue(nativeType, ct.ExactIntegerCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeExactIntegerValue(tempValue, nativeType)

  override def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : ps.TempValue = targetType match {
    case intType : vt.IntType =>
      val convTemp = ps.Temp(nativeType)
      plan.steps += ps.ConvertNativeInteger(convTemp, tempValue, intType.bits, intType.signed)

      convTemp

    case fpType : vt.FpType =>
      val convTemp = ps.Temp(nativeType)
      plan.steps += ps.ConvertNativeIntegerToFloat(convTemp, tempValue, nativeType.signed, fpType)

      convTemp

    case _ => 
      impossibleConversion(s"Cannot convert native value of type ${nativeType.schemeName} to non-numeric native type ${targetType.schemeName}") 
  }
  
  override def planCastToCellTempValue(cellType : ct.CellType)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = cellType match {
    case ct.InexactRationalCell =>
      // Convert us to double and box
      val boxedTemp = ps.CellTemp(cellType)
      
      plan.steps += ps.BoxInexactRational(boxedTemp, toTempValue(vt.Double))

      boxedTemp
    
    case _ =>
      impossibleConversion(s"Cannot convert native value of type ${nativeType.schemeName} to non-numeric cell type ${cellType.schemeName}") 
  }

  def planCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =  {
    // We can only box 64bit signed ints
    val boxedTemp = ps.CellTemp(ct.ExactIntegerCell)
    plan.steps += ps.BoxExactInteger(boxedTemp, toTempValue(vt.Int64))

    boxedTemp
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
  
  def planCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =  {
    // We can only box doubles
    val boxedTemp = ps.CellTemp(ct.InexactRationalCell)
    plan.steps += ps.BoxInexactRational(boxedTemp, toTempValue(vt.Double))

    boxedTemp
  }
}

class NativeCharacterValue(tempValue : ps.TempValue) extends NativeValue(vt.UnicodeChar, ct.CharacterCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeCharacterValue(tempValue)

  def planCellTempValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =  {
    val boxedTemp = ps.CellTemp(cellType)
    plan.steps += ps.BoxCharacter(boxedTemp, tempValue)

    boxedTemp
  }
}
