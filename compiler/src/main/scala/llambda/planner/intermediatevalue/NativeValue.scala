package llambda.planner.intermediatevalue

import llambda.{celltype => ct}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.PlanWriter

sealed abstract class NativeValue(val nativeType : vt.NativeType, val cellType : ct.ConcreteCellType, val tempValue : ps.TempValue) extends IntermediateValue with UninvokableValue with NonRecordValue {
  val possibleTypes = Set(cellType)

  // This is used for our shortcut in planPhiWith to build a new phi'ed intermediate
  protected def withNewTempValue(tempValue : ps.TempValue) : NativeValue

  protected def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = 
    None
  
  protected def planCastToCellTempValue(cellType : ct.CellType)(implicit plan : PlanWriter) : Option[ps.TempValue] = 
    None
  
  protected def planCellTempValue()(implicit plan : PlanWriter) : ps.TempValue

  def toNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = 
    if (targetType == nativeType) {
      return Some(tempValue)
    }
    else {
      planCastToNativeTempValue(targetType)
    }
  
  def toCellTempValue(targetType : ct.CellType)(implicit plan : PlanWriter) : Option[ps.TempValue] = {
    if (targetType.isTypeOrSupertypeOf(cellType)) {
      val boxedTemp = planCellTempValue()

      if (cellType == targetType) {
        // No casting
        Some(boxedTemp)
      }
      else {
        // Cast to its supertype
        val castTemp = new ps.TempValue
        plan.steps += ps.CastCellToTypeUnchecked(castTemp, boxedTemp, targetType)

        Some(castTemp)
      }
    }
    else {
      planCastToCellTempValue(cellType)
    }
  }
  
  override def planPhiWith(theirValue : IntermediateValue)(ourPlan : PlanWriter, theirPlan : PlanWriter) : PlanPhiResult = theirValue match {
    case theirUnboxed : NativeValue if nativeType == theirUnboxed.nativeType =>
      // Our types exactly match - no conversion needed!
      val phiResultTemp = new ps.TempValue

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
}

class NativeBooleanValue(tempValue : ps.TempValue) extends NativeValue(vt.CBool, ct.BooleanCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeBooleanValue(tempValue)

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val predTemp = new ps.TempValue
    plan.steps += ps.ConvertNativeInteger(predTemp, tempValue, 1, false) 

    predTemp
  }
  
  def planCellTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxBoolean(boxedTemp, toTruthyPredicate())

    boxedTemp
  }
}

class NativeExactIntegerValue(tempValue : ps.TempValue, nativeType : vt.IntType) extends NativeValue(nativeType, ct.ExactIntegerCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeExactIntegerValue(tempValue, nativeType)

  override def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = targetType match {
    case intType : vt.IntType =>
      val convTemp = new ps.TempValue
      plan.steps += ps.ConvertNativeInteger(convTemp, tempValue, intType.bits, intType.signed)

      Some(convTemp)

    case fpType : vt.FpType =>
      val convTemp = new ps.TempValue
      plan.steps += ps.ConvertNativeIntegerToFloat(convTemp, tempValue, nativeType.signed, fpType)

      Some(convTemp)

    case _ => 
      None
  }
  
  override def planCastToCellTempValue(cellType : ct.CellType)(implicit plan : PlanWriter) : Option[ps.TempValue] = cellType match {
    case ct.InexactRationalCell =>
      // Allocate space for the int
      val allocTemp = new ps.TempAllocation
      plan.steps += ps.AllocateCons(allocTemp, 1)

      // Convert us to double and box
      val boxedTemp = new ps.TempValue
      
      plan.steps += ps.BoxInexactRational(boxedTemp, allocTemp, 0, toRequiredTempValue(vt.Double))

      Some(boxedTemp)
    
    case _ =>
      None
  }

  def planCellTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    plan.steps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxExactInteger(boxedTemp, allocTemp, 0, toRequiredTempValue(vt.Int64))

    boxedTemp
  }
}

class NativeInexactRationalValue(tempValue : ps.TempValue, nativeType : vt.FpType) extends NativeValue(nativeType, ct.InexactRationalCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeInexactRationalValue(tempValue, nativeType)

  override def planCastToNativeTempValue(targetType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = targetType match {
    case fpType : vt.FpType =>
      val convTemp = new ps.TempValue
      plan.steps += ps.ConvertNativeFloat(convTemp, tempValue, fpType)

      Some(convTemp)

    case _ => 
      None
  }
  
  def planCellTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    plan.steps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxInexactRational(boxedTemp, allocTemp, 0, toRequiredTempValue(vt.Double))

    boxedTemp
  }
}

class NativeCharacterValue(tempValue : ps.TempValue) extends NativeValue(vt.UnicodeChar, ct.CharacterCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeCharacterValue(tempValue)

  def planCellTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    plan.steps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxCharacter(boxedTemp, allocTemp, 0, tempValue)

    boxedTemp
  }
}

class NativeUtf8String(tempValue : ps.TempValue) extends NativeValue(vt.Utf8CString, ct.StringCell, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new NativeUtf8String(tempValue)

  def planCellTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxUtf8String(boxedTemp, tempValue)

    boxedTemp
  }
}

