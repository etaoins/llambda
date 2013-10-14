package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

abstract class LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType]

  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)]

  def toRequiredNativeType(state : GenerationState)(targetType : nfi.NativeType) : (GenerationState, IrValue) = {
    toNativeType(state)(targetType) getOrElse {
      throw new ImpossibleTypeConversionException("Unable to convert " + this.toString + " to " + targetType)
    }
  }
}

abstract class ConstantLiveValue(boxedType : bt.ConcreteBoxedType) extends LiveValue {
  val possibleTypes = Set(boxedType)
  val booleanValue = true

  def genBoxedConstant(module : IrModuleBuilder) : IrConstant
  val genUnboxedConstant : PartialFunction[nfi.NativeType, IrConstant]

  def genCastBoxedConstant(module : IrModuleBuilder)(targetType : bt.BoxedType) = {
    val uncastIrValue = genBoxedConstant(module)
    BitcastToConstant(uncastIrValue, PointerType(targetType.irType))
  }
  
  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrConstant)] = {
    targetType match {
      case nfi.BoxedValue(expectedType) =>
        if (!boxedType.isTypeOrSubtypeOf(expectedType)) {
          // Not possible
          None
        }
        else {
          Some((state, genCastBoxedConstant(state.module)(expectedType)))
        }

      case nfi.CBool =>
        // Convert our truthiness to 1/0
        val boolInt = if (booleanValue) {
          1
        }
        else {
          0
        }
      
        // Make a constant out of it
        val boolConstant = IntegerConstant(IntegerType(8), boolInt)

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
  def genBooleanValue(state : GenerationState) : IrValue = 
    GlobalVariable("lliby_true_value", PointerType(bt.BoxedBoolean.irType))

  def genBoxedValue(state : GenerationState) : IrValue

  def genCastBoxedValue(state : GenerationState)(targetType : bt.BoxedType) : IrValue = {
    val uncastIrValue = genBoxedValue(state)
    val castValueName = "castTo" + targetType.name

    state.currentBlock.bitcastTo(castValueName)(uncastIrValue, PointerType(targetType.irType))
  }

  // This should be good for most subclasses except for numerics which support
  // implicit conversions
  def genUnboxedValue(state : GenerationState)(targetType : nfi.NativeType) : Option[IrValue] = {
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

      case nfi.CBool =>
        Some((state, genBooleanValue(state)))

      case unboxedType =>
        genUnboxedValue(state)(targetType).map((state, _))
    }
  }
}
