package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

abstract class LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType]

  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)]

  def genTruthyPredicate(state : GenerationState) : IrValue

  def toRequiredNativeType(state : GenerationState)(targetType : nfi.NativeType) : (GenerationState, IrValue) = {
    toNativeType(state)(targetType) getOrElse {
      throw new ImpossibleTypeConversionException("Unable to convert " + this.toString + " to " + targetType)
    }
  }
}

abstract class ConstantLiveValue(boxedType : bt.ConcreteBoxedType) extends LiveValue {
  val possibleTypes = Set(boxedType)
  val booleanValue = true

  def genBoxedConstant() : IrConstant
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant]

  def genCastBoxedConstant(targetType : bt.BoxedType) = {
    val uncastIrValue = genBoxedConstant()
    BitcastToConstant(uncastIrValue, PointerType(targetType.irType))
  }

  def genTruthyPredicate(state : GenerationState) : IrValue = {
    val booleanIntValue = if (booleanValue) 1 else 0

    IntegerConstant(IntegerType(1), booleanIntValue)
  }
  
  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrConstant)] = {
    targetType match {
      case nfi.BoxedValue(expectedType) =>
        if (!boxedType.isTypeOrSubtypeOf(expectedType)) {
          // Not possible
          None
        }
        else {
          Some((state, genCastBoxedConstant(expectedType)))
        }

      case nfi.CBool =>
        // Make a constant out of it
        val booleanIntValue = if (booleanValue) 1 else 0
        val boolConstant = IntegerConstant(IntegerType(8), booleanIntValue)

        Some((state, boolConstant))

      case unboxedType =>
        genUnboxedConstant.lift.apply(unboxedType).map((state, _))
    }
  }

  protected def declareBoxedConstant(module : IrModuleBuilder)(name : String, initializer  : IrConstant) : IrConstant = {
    val boxedConstantDef = IrGlobalVariableDef(
      name=name,
      initializer=initializer,
      visibility=Visibility.Hidden,
      unnamedAddr=true,
      constant=true)

    module.defineGlobalVariable(boxedConstantDef)

    boxedConstantDef.variable
  }
}

abstract class UnboxedLiveValue(boxedType : bt.ConcreteBoxedType, nativeType : nfi.NativeType, unboxedValue : IrValue) extends LiveValue {
  val possibleTypes = Set(boxedType)

  // Should only be overriden by LiveBoolean
  def genTruthyPredicate(state : GenerationState) : IrValue =
    IntegerConstant(IntegerType(1), 1)

  def genBoxedValue(state : GenerationState) : IrValue

  def genCastBoxedValue(state : GenerationState)(targetType : bt.BoxedType) : IrValue = {
    val uncastIrValue = genBoxedValue(state)
    val castValueName = "castTo" + targetType.name

    state.currentBlock.bitcastTo(castValueName)(uncastIrValue, PointerType(targetType.irType))
  }

  // This should be good for most subclasses except for numerics which support
  // implicit conversions
  def genUnboxedValue(state : GenerationState)(targetType : nfi.UnboxedType) : Option[IrValue] = {
    if (targetType != nativeType) {
      None
    }
    else {
      Some(unboxedValue)
    }
  }
  
  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)] = {
    targetType match {
      case nfi.BoxedValue(expectedType) =>
        if (!boxedType.isTypeOrSubtypeOf(expectedType)) {
          // Not possible
          None
        }
        else {
          Some((state, genCastBoxedValue(state)(expectedType)))
        }

      // If we're already a CBool we want to fall to genUnboxedValue so we 
      // directly return our unboxed value instead of truncating then extending
      case nfi.CBool if (nativeType != nfi.CBool) =>
        val block = state.currentBlock

        val truthyPred = genTruthyPredicate(state)
        val truthyBool = block.zextTo("truthyBool")(truthyPred, IntegerType(nfi.CBool.bits))

        Some((state, truthyBool))

      case unboxedType : nfi.UnboxedType =>
        genUnboxedValue(state)(unboxedType).map((state, _))
    }
  }
}
