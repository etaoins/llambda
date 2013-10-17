package llambda.codegen

import llambda.nfi
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

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

