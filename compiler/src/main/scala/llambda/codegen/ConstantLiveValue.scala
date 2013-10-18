package llambda.codegen

import llambda.nfi
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

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

      case nfi.CTruthyBool =>
        // Make a constant out of it
        val booleanIntValue = if (booleanValue) 1 else 0
        val boolConstant = IntegerConstant(IntegerType(nfi.CStrictBool.bits), booleanIntValue)

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


