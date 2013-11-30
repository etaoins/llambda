package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, InvokableProcedure}

sealed abstract class ScalarValue(val nativeType : nfi.NativeType, val boxedType : bt.ConcreteBoxedType, val tempValue : ps.TempValue) extends IntermediateValue {
  val possibleTypes = Set(boxedType)
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] = 
    // Procedure values have no unboxed intermediate value
    // They're either KnownProcedures which are a special direct subclass of
    // IntermediateValue or they're DynamicBoxedValues of type bt.BoxedProcedure
    None
  
  def toBoxedRecordTempValue(recordDataType : vt.RecordDataType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // It's impossible for a scalar to be a record by definition
    None

  // This is used for our shortcut in planPhiWith to build a new phi'ed intermediate
  protected def withNewTempValue(tempValue : ps.TempValue) : ScalarValue

  protected def planCastToUnboxedTempValue(unboxedType : nfi.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = 
    None
  
  protected def planCastToBoxedTempValue(boxedType : bt.BoxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] = 
    None
  
  protected def planBoxedTempValue()(implicit plan : PlanWriter) : ps.TempValue

  def toScalarTempValue(unboxedType : nfi.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = 
    if (unboxedType == nativeType) {
      return Some(tempValue)
    }
    else {
      planCastToUnboxedTempValue(unboxedType)
    }
  
  def toBoxedTempValue(targetType : bt.BoxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] = {
    if (targetType.isTypeOrSupertypeOf(boxedType)) {
      val boxedTemp = planBoxedTempValue()

      if (boxedType == targetType) {
        // No casting
        Some(boxedTemp)
      }
      else {
        // Cast to its supertype
        val castTemp = new ps.TempValue
        plan.steps += ps.CastBoxedToTypeUnchecked(castTemp, boxedTemp, targetType)

        Some(castTemp)
      }
    }
    else {
      planCastToBoxedTempValue(boxedType)
    }
  }
  
  override def planPhiWith(theirValue : IntermediateValue)(ourPlan : PlanWriter, theirPlan : PlanWriter) : PlanPhiResult = theirValue match {
    case theirUnboxed : ScalarValue if nativeType == theirUnboxed.nativeType =>
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

class UnboxedBooleanValue(tempValue : ps.TempValue) extends ScalarValue(nfi.CBool, bt.BoxedBoolean, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedBooleanValue(tempValue)

  override def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val predTemp = new ps.TempValue
    plan.steps += ps.ConvertNativeInteger(predTemp, tempValue, 1, false) 

    predTemp
  }
  
  def planBoxedTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxBoolean(boxedTemp, toTruthyPredicate())

    boxedTemp
  }
}

class UnboxedExactIntegerValue(tempValue : ps.TempValue, nativeType : nfi.IntType) extends ScalarValue(nativeType, bt.BoxedExactInteger, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedExactIntegerValue(tempValue, nativeType)

  override def planCastToUnboxedTempValue(unboxedType : nfi.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = unboxedType match {
    case intType : nfi.IntType =>
      val convTemp = new ps.TempValue
      plan.steps += ps.ConvertNativeInteger(convTemp, tempValue, intType.bits, intType.signed)

      Some(convTemp)

    case fpType : nfi.FpType =>
      val convTemp = new ps.TempValue
      plan.steps += ps.ConvertNativeIntegerToFloat(convTemp, tempValue, nativeType.signed, fpType)

      Some(convTemp)

    case _ => 
      None
  }
  
  override def planCastToBoxedTempValue(boxedType : bt.BoxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] = boxedType match {
    case bt.BoxedInexactRational =>
      // Allocate space for the int
      val allocTemp = new ps.TempAllocation
      plan.steps += ps.AllocateCons(allocTemp, 1)

      // Convert us to double and box
      val boxedTemp = new ps.TempValue
      
      plan.steps += ps.BoxInexactRational(boxedTemp, allocTemp, 0, toRequiredTempValue(vt.ScalarType(nfi.Double)))

      Some(boxedTemp)
    
    case _ =>
      None
  }

  def planBoxedTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    plan.steps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxExactInteger(boxedTemp, allocTemp, 0, toRequiredTempValue(vt.ScalarType(nfi.Int64)))

    boxedTemp
  }
}

class UnboxedInexactRationalValue(tempValue : ps.TempValue, nativeType : nfi.FpType) extends ScalarValue(nativeType, bt.BoxedInexactRational, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedInexactRationalValue(tempValue, nativeType)

  override def planCastToUnboxedTempValue(unboxedType : nfi.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] = unboxedType match {
    case fpType : nfi.FpType =>
      val convTemp = new ps.TempValue
      plan.steps += ps.ConvertNativeFloat(convTemp, tempValue, fpType)

      Some(convTemp)

    case _ => 
      None
  }
  
  def planBoxedTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    plan.steps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxInexactRational(boxedTemp, allocTemp, 0, toRequiredTempValue(vt.ScalarType(nfi.Double)))

    boxedTemp
  }
}

class UnboxedCharacterValue(tempValue : ps.TempValue) extends ScalarValue(nfi.UnicodeChar, bt.BoxedCharacter, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedCharacterValue(tempValue)

  def planBoxedTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    plan.steps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxCharacter(boxedTemp, allocTemp, 0, tempValue)

    boxedTemp
  }
}

class UnboxedUtf8String(tempValue : ps.TempValue) extends ScalarValue(nfi.Utf8CString, bt.BoxedString, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedUtf8String(tempValue)

  def planBoxedTempValue()(implicit plan : PlanWriter) : ps.TempValue =  {
    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    plan.steps += ps.BoxUtf8String(boxedTemp, tempValue)

    boxedTemp
  }
}

