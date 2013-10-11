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
  val booleanValue = false

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
        val boolConstant = IntegerConstant(IntegerType(1), boolInt)

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

