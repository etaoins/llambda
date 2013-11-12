package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

class BoxedLiveValue(val possibleTypes : Set[bt.ConcreteBoxedType], boxedValue : IrValue) extends LiveValue {
  def genTruthyPredicate(state : GenerationState) : IrValue = {
    if (possibleTypes.contains(bt.BoxedBoolean)) {
      LiveBoolean.genTruthyCheck(state)(boxedValue)
    }
    else {
      // All non-boolean values evaluate as true
      IntegerConstant(IntegerType(1), 1)
    }
  }
  
  private def genGenericUnboxing(initialState : GenerationState)(targetType : nfi.UnboxedType) : Option[(GenerationState, IrValue)] = 
    targetType match {
      case nfi.CTruthyBool =>
        // Generate the boolean predicate
        val truthPred = genTruthyPredicate(initialState)

        // Sign extend to the CBool size
        val block = initialState.currentBlock
        val truthBool = block.zextTo("truthBool")(truthPred, IntegerType(nfi.CStrictBool.bits))

        Some((initialState, truthBool))

      case nfi.CStrictBool =>
        // Strict bools must be of type boolean
        genCastToBoxed(initialState)(bt.BoxedBoolean).map({ case (state, boxedValue) =>
          // Otherwise it's the same as truthy bools
          genGenericUnboxing(state)(nfi.CTruthyBool).get
        })

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

      case nfi.UnicodeChar =>
        genCastToBoxed(initialState)(bt.BoxedCharacter).map({ case (state, boxedValue) =>
          val unboxedValue = LiveCharacter.genUnboxing(state.currentBlock)(boxedValue)

          (state, unboxedValue)
        })

      case nfi.Utf8CString =>
        genCastToBoxed(initialState)(bt.BoxedString).map({ case (state, boxedValue) =>
          val unboxedValue = LiveString.genUtf8Unboxing(state.currentBlock)(boxedValue)

          (state, unboxedValue)
        })
    }

  protected def genCastToBoxed(initialState : GenerationState)(targetType : bt.BoxedType) : Option[(GenerationState, IrValue)] = {
    val targetConcreteTypes = targetType.concreteTypes

    // Are our possible concrete types a subset of the target types?
    if (possibleTypes.subsetOf(targetConcreteTypes)) {
      // This doesn't require any type assertions
      val supertypeValue = targetType.genPointerBitcast(initialState.currentBlock)(boxedValue)

      Some((initialState, supertypeValue))
    }
    else if (!possibleTypes.intersect(targetConcreteTypes).isEmpty) {
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
