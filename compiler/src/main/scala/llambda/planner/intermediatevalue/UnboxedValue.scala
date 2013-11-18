package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{StepBuffer, InvokableProcedure}

sealed abstract class UnboxedValue(val nativeType : nfi.NativeType, val boxedType : bt.ConcreteBoxedType, val tempValue : ps.TempValue) extends IntermediateValue {
  val possibleTypes = Set(boxedType)
  
  def toInvokableProcedure()(implicit planSteps : StepBuffer) : Option[InvokableProcedure] = 
    // Procedure values have no unboxed intermediate value
    // They're either KnownProcedures which are a special direct subclass of
    // IntermediateValue or they're DynamicBoxedValues of type bt.BoxedProcedure
    None

  // This is used for our shortcut in planPhiWith to build a new phi'ed intermediate
  protected def withNewTempValue(tempValue : ps.TempValue) : UnboxedValue

  protected def planCastToUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = 
    None
  
  protected def planCastToBoxedTempValue(boxedType : bt.BoxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = 
    None
  
  protected def planBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue

  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = 
    if (unboxedType == nativeType) {
      return Some(tempValue )
    }
    else {
      planCastToUnboxedTempValue(unboxedType)
    }
  
  def toBoxedTempValue(targetType : bt.BoxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = {
    if (targetType.isTypeOrSupertypeOf(boxedType)) {
      val boxedTemp = planBoxedTempValue()

      if (boxedType == targetType) {
        // No casting
        Some(boxedTemp)
      }
      else {
        // Cast to its supertype
        val castTemp = new ps.TempValue
        planSteps += ps.CastBoxedToTypeUnchecked(castTemp, boxedTemp, targetType)

        Some(castTemp)
      }
    }
    else {
      planCastToBoxedTempValue(boxedType)
    }
  }
  
  override def planPhiWith(theirValue : IntermediateValue)(ourSteps : StepBuffer, theirSteps : StepBuffer) : PlanPhiResult = theirValue match {
    case theirUnboxed : UnboxedValue if nativeType == theirUnboxed.nativeType =>
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
      super.planPhiWith(theirValue)(ourSteps, theirSteps)
  }
}

class UnboxedBooleanValue(tempValue : ps.TempValue) extends UnboxedValue(nfi.CStrictBool, bt.BoxedBoolean, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedBooleanValue(tempValue)

  override def toTruthyPredicate()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val predTemp = new ps.TempValue
    planSteps += ps.ConvertNativeInteger(predTemp, tempValue, 1, false) 

    predTemp
  }
  
  def planBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue =  {
    val boxedTemp = new ps.TempValue
    planSteps += ps.BoxBoolean(boxedTemp, toTruthyPredicate())

    boxedTemp
  }
}

class UnboxedExactIntegerValue(tempValue : ps.TempValue, nativeType : nfi.IntType) extends UnboxedValue(nativeType, bt.BoxedExactInteger, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedExactIntegerValue(tempValue, nativeType)

  override def planCastToUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = unboxedType match {
    case intType : nfi.IntType =>
      val convTemp = new ps.TempValue
      planSteps += ps.ConvertNativeInteger(convTemp, tempValue, intType.bits, intType.signed)

      Some(convTemp)

    case fpType : nfi.FpType =>
      val convTemp = new ps.TempValue
      planSteps += ps.ConvertNativeIntegerToFloat(convTemp, tempValue, nativeType.signed, fpType)

      Some(convTemp)

    case _ => 
      None
  }
  
  override def planCastToBoxedTempValue(boxedType : bt.BoxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = boxedType match {
    case bt.BoxedInexactRational =>
      // Allocate space for the int
      val allocTemp = new ps.TempAllocation
      planSteps += ps.AllocateCons(allocTemp, 1)

      // Convert us to double and box
      val boxedTemp = new ps.TempValue
      
      planSteps += ps.BoxInexactRational(boxedTemp, allocTemp, 0, toRequiredTempValue(nfi.Double))

      Some(boxedTemp)
    
    case _ =>
      None
  }

  def planBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    planSteps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    planSteps += ps.BoxExactInteger(boxedTemp, allocTemp, 0, toRequiredTempValue(nfi.Int64))

    boxedTemp
  }
}

class UnboxedInexactRationalValue(tempValue : ps.TempValue, nativeType : nfi.FpType) extends UnboxedValue(nativeType, bt.BoxedInexactRational, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedInexactRationalValue(tempValue, nativeType)

  override def planCastToUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = unboxedType match {
    case fpType : nfi.FpType =>
      val convTemp = new ps.TempValue
      planSteps += ps.ConvertNativeFloat(convTemp, tempValue, fpType)

      Some(convTemp)

    case _ => 
      None
  }
  
  def planBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    planSteps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    planSteps += ps.BoxInexactRational(boxedTemp, allocTemp, 0, toRequiredTempValue(nfi.Double))

    boxedTemp
  }
}

class UnboxedCharacterValue(tempValue : ps.TempValue) extends UnboxedValue(nfi.UnicodeChar, bt.BoxedCharacter, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedCharacterValue(tempValue)

  def planBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue =  {
    // Allocate space for the int
    val allocTemp = new ps.TempAllocation
    planSteps += ps.AllocateCons(allocTemp, 1)

    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    planSteps += ps.BoxCharacter(boxedTemp, allocTemp, 0, tempValue)

    boxedTemp
  }
}

class UnboxedUtf8String(tempValue : ps.TempValue) extends UnboxedValue(nfi.Utf8CString, bt.BoxedString, tempValue) {
  def withNewTempValue(tempValue : ps.TempValue) = new UnboxedUtf8String(tempValue)

  def planBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue =  {
    // We can only box 64bit signed ints
    val boxedTemp = new ps.TempValue
    planSteps += ps.BoxUtf8String(boxedTemp, tempValue)

    boxedTemp
  }
}

