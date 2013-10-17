package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

class BoxedLiveValue(val possibleTypes : Set[bt.ConcreteBoxedType], boxedValue : IrValue) extends LiveValue {
  private def genGenericUnboxing(initialState : GenerationState)(targetType : nfi.UnboxedType) : Option[(GenerationState, IrValue)] = 
    targetType match {
      case nfi.CBool =>
        if (possibleTypes.contains(bt.BoxedBoolean)) {
          LiveBoolean.genTruthinessCheck(initialState)(boxedValue)
        }
        else {
          // All non-boolean values evaluate as true
          Some((initialState, IntegerConstant(IntegerType(8), 1)))
        }

      case intType : nfi.IntType =>
        genCastToBoxed(initialState)(bt.BoxedExactInteger).map({ case (state, boxedValue) =>
          val unboxedValue = LiveExactInteger.genIntUnboxing(state.currentBlock)(boxedValue, intType)

          (state, unboxedValue)
        })

      case fpType : nfi.FpType => 
        if (possibleTypes == Set(bt.BoxedExactInteger)) {
          // Unbox directly from exact int
          val block = initialState.currentBlock

          val exactIntBoxedValue = bt.BoxedExactInteger.genPointerBitcast(block)(boxedValue)
          val unboxedValue = LiveExactInteger.genFpUnboxing(block)(exactIntBoxedValue, fpType)

          Some((initialState, unboxedValue))
        }
        else if (possibleTypes == Set(bt.BoxedInexactRational)) {
          // Unbox directly from inexact rational
          val block = initialState.currentBlock

          val rationalBoxedValue = bt.BoxedInexactRational.genPointerBitcast(block)(boxedValue)
          val unboxedValue = LiveInexactRational.genUnboxing(block)(rationalBoxedValue, fpType)

          Some((initialState, unboxedValue))
        }
        else if (possibleTypes.contains(bt.BoxedExactInteger) || possibleTypes.contains(bt.BoxedInexactRational)) {
          // This is quite complex. Consult LiveNumeric for details.
          Some(LiveNumeric.genCheckedUnboxing(initialState)(boxedValue, fpType))
        }
        else {
          // Not possible
          None
        }
    }

  protected def genCastToBoxed(initialState : GenerationState)(targetType : bt.BoxedType) : Option[(GenerationState, IrValue)] = {
    // Is the target type either equal to or the supertype of each of our possible types?
    if (possibleTypes.forall(targetType.isTypeOrSupertypeOf(_))) {
      // This doesn't require any type assertions
      val supertypeValue = targetType.genPointerBitcast(initialState.currentBlock)(boxedValue)

      Some((initialState, supertypeValue))
    }
    else if (possibleTypes.exists(targetType.isTypeOrSubtypeOf(_))) {
      // We need to build a type assertion
      val successBlock = initialState.currentBlock.startChildBlock(targetType.name + "SubcastSuccess") 
      val failBlock = initialState.currentBlock.startChildBlock(targetType.name + "SubcastFail") 

      // Do the actual check
      targetType.genTypeCheck(initialState.currentBlock)(boxedValue, successBlock, failBlock)
    
      // Generate the fail branch
      val errorName = s"subcastTo${targetType.name.capitalize}Failed"
      val errorMessage = s"Runtime cast to subtype '${targetType.name}' failed" 
      GenFatalError(initialState.module, failBlock)(errorName, errorMessage)

      // Continue building on the success block
      val subtypeValue = targetType.genPointerBitcast(successBlock)(boxedValue)
      Some((initialState.copy(currentBlock=successBlock), subtypeValue))
    }
    else {
      // This conversion is impossible
      None
    }
  }

  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)] = {
    targetType match {
      case nfi.BoxedValue(expectedType) =>
        genCastToBoxed(state)(expectedType)

      case unboxedType : nfi.UnboxedType =>
        genGenericUnboxing(state)(unboxedType)
    }
  }
}
