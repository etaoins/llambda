package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

class BoxedLiveValue(boxedType : bt.BoxedType, boxedValue : IrValue) extends LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType] =
    (boxedType :: boxedType.subtypes).collect({
      case concrete : bt.ConcreteBoxedType => concrete
    }).toSet
  
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
        if (boxedType == bt.BoxedExactInteger) {
          // Unbox directly from exact int
          val block = initialState.currentBlock

          val exactIntBoxedValue = bt.BoxedExactInteger.genPointerBitcast(block)(boxedValue)
          val unboxedValue = LiveExactInteger.genFpUnboxing(block)(exactIntBoxedValue, fpType)

          Some((initialState, unboxedValue))
        }
        else if (boxedType == bt.BoxedInexactRational) {
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
    if (targetType.isTypeOrSupertypeOf(boxedType)) {
      // This doesn't require any type assertions
      val supertypeValue = targetType.genPointerBitcast(initialState.currentBlock)(boxedValue)

      Some((initialState, supertypeValue))
    }
    else if (targetType.isTypeOrSubtypeOf(boxedType)) {
      // We need to build a type assertion
      val successBlock = initialState.currentBlock.startChildBlock(targetType.name + "SubcastSuccess") 
      val failBlock = initialState.currentBlock.startChildBlock(targetType.name + "SubcastFail") 

      // Do the actual check
      targetType.genTypeCheck(initialState.currentBlock)(boxedValue, successBlock, failBlock)
    
      // Generate the fail branch
      val errorName = s"subcastFrom${boxedType.name.capitalize}To${targetType.name.capitalize}"
      val errorMessage = s"Runtime cast from '${boxedType.name}' to subtype '${targetType.name}' failed" 
      GenFatalError(initialState.module, failBlock)(errorName, errorMessage)

      // Continue building on the success block
      val subtypeValue = targetType.genPointerBitcast(successBlock)(boxedValue)
      Some((initialState.copy(currentBlock=successBlock), subtypeValue))
    }
    else {
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
