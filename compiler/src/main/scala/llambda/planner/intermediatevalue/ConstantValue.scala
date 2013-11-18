package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{StepBuffer, InvokableProcedure}

sealed abstract class ConstantValue(boxedType : bt.ConcreteBoxedType) extends IntermediateValue {
  val possibleTypes = Set(boxedType)
    
  def toConstantBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue

  def toInvokableProcedure()(implicit planSteps : StepBuffer) : Option[InvokableProcedure] = 
    None

  def toBoxedTempValue(targetType : bt.BoxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    if (targetType.isTypeOrSupertypeOf(boxedType)) {
      val boxedTempValue = toConstantBoxedTempValue()
      
      if (targetType == boxedType) {
        // Perfect!
        Some(boxedTempValue)
      }
      else {
        // Cast to supertype before returning
        val castTemp = new ps.TempValue
        planSteps += ps.CastBoxedToTypeUnchecked(castTemp, boxedTempValue, targetType)

        Some(castTemp)
      }
    }
    else {
      // Impossible conversion
      None
    }
}

sealed abstract class TrivialConstantValue[T, U <: ps.StoreBoxedConstant](boxedType : bt.ConcreteBoxedType, value : T, stepConstructor : (ps.TempValue, T) => U) extends ConstantValue(boxedType) {
  def toConstantBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    planSteps += stepConstructor(constantTemp, value)
    constantTemp
  }
}

class ConstantStringValue(value : String) extends TrivialConstantValue(bt.BoxedString, value, ps.StoreBoxedString.apply) {
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = unboxedType match {
    case nfi.Utf8CString =>
      val constantTemp = new ps.TempValue
      planSteps += ps.StoreNativeUtf8String(constantTemp, value)
      Some(constantTemp)
    
    case _ => None
  }
}

class ConstantSymbolValue(value : String) extends TrivialConstantValue(bt.BoxedSymbol, value, ps.StoreBoxedSymbol.apply) {
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    // Symbols have no NFI representation
    None
}

class ConstantExactIntegerValue(value : Long) extends TrivialConstantValue(bt.BoxedExactInteger, value, ps.StoreBoxedExactInteger.apply) {
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = unboxedType match {
    case intType : nfi.IntType =>
      val constantTemp = new ps.TempValue
      planSteps += ps.StoreNativeInteger(constantTemp, value, intType.bits)
      Some(constantTemp)

    case fpType : nfi.FpType =>
      val constantTemp = new ps.TempValue
      planSteps += ps.StoreNativeFloat(constantTemp, value.toDouble, fpType)
      Some(constantTemp)

    case _ => None
  }
}

class ConstantInexactRationalValue(value : Double) extends TrivialConstantValue(bt.BoxedInexactRational, value, ps.StoreBoxedInexactRational.apply) {
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = unboxedType match {
    case fpType : nfi.FpType =>
      val constantTemp = new ps.TempValue
      planSteps += ps.StoreNativeFloat(constantTemp, value, fpType)
      Some(constantTemp)

    case _ => None
  }
}

class ConstantCharacterValue(value : Char) extends TrivialConstantValue(bt.BoxedCharacter, value, ps.StoreBoxedCharacter.apply) {
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = unboxedType match {
    case nfi.UnicodeChar =>
      val constantTemp = new ps.TempValue
      planSteps += ps.StoreNativeInteger(constantTemp, value, nfi.UnicodeChar.bits)
      Some(constantTemp)

    case _ => None
  }
}

class ConstantBooleanValue(value : Boolean) extends TrivialConstantValue(bt.BoxedBoolean, value, ps.StoreBoxedBoolean.apply) {
  private val intValue = if (value) 1 else 0

  override def toTruthyPredicate()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val predTemp = new ps.TempValue
    planSteps += ps.StoreNativeInteger(predTemp, intValue, 1) 

    predTemp
  }

  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] = unboxedType match {
    case nfi.CStrictBool =>
      val constantTemp = new ps.TempValue

      planSteps += ps.StoreNativeInteger(constantTemp, intValue, nfi.CStrictBool.bits)
      Some(constantTemp)

    case _ =>
      None
  }
}

class ConstantBytevectorValue(value : Vector[Short]) extends TrivialConstantValue(bt.BoxedBytevector, value, ps.StoreBoxedBytevector.apply) {
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    // Bytevectors can't be unboxed
    None
}

class ConstantPairValue(car : ConstantValue, cdr : ConstantValue) extends ConstantValue(bt.BoxedPair) {
  def toConstantBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val constantTemp = new ps.TempValue

    // Box our car/cdr first
    val carTemp = car.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))
    val cdrTemp = cdr.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))

    planSteps += ps.StoreBoxedPair(constantTemp, carTemp, cdrTemp)

    constantTemp
  }

  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    // Pairs cannot be unboxed
    None
}

class ConstantVectorValue(elements : Vector[ConstantValue]) extends ConstantValue(bt.BoxedVector) {
  def toConstantBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val constantTemp = new ps.TempValue

    // Box our elements
    val elementTemps = elements.map {
      _.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))
    }

    planSteps += ps.StoreBoxedVector(constantTemp, elementTemps)

    constantTemp
  }

  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    // Pairs cannot be unboxed
    None
}

object EmptyListValue extends ConstantValue(bt.BoxedEmptyList) {
  def toConstantBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    planSteps += ps.StoreBoxedEmptyList(constantTemp)
    constantTemp
  }

  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    None
}

object UnspecificValue extends ConstantValue(bt.BoxedUnspecific) {
  def toConstantBoxedTempValue()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val constantTemp = new ps.TempValue
    planSteps += ps.StoreBoxedUnspecific(constantTemp)
    constantTemp
  }

  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    None
}

