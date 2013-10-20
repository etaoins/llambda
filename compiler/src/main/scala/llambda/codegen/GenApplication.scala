package llambda.codegen

import llambda.codegen.llvmir._
import llambda.nfi
import llambda.codegen.{boxedtype => bt}
import llambda.nfi.NativeSignature
import llambda.{NotImplementedException, IncompatibleArityException}

object GenApplication {
  private def genProcedureCall(initialState : GenerationState)(procedure : LiveProcedure, liveOperands : List[LiveValue]) : (GenerationState, LiveValue) = {
    val signature = procedure.signature

    if (signature.hasSelfArg) {
      throw new NotImplementedException("Self arguments not implemented")
    }
    
    // Ensure our arity is sane
    if (signature.hasRestArg) {
      if (liveOperands.length < signature.fixedArgs.length) {
        throw new IncompatibleArityException(s"Called function with ${liveOperands.length} arguments; requires at least ${signature.fixedArgs.length} arguments")
      }
    }
    else {
      if (signature.fixedArgs.length != liveOperands.length) {
        throw new IncompatibleArityException(s"Called function with ${liveOperands.length} arguments; requires exactly ${signature.fixedArgs.length} arguments")
      }
    }

    // Give up and use a var here
    // foldLeft + zip ends up being a right mess
    var currentState = initialState 

    // Convert our operands to IrValues of the expected type
    val irFixedOperands = liveOperands.zip(signature.fixedArgs).map { case (liveOperand, nativeType) =>
      val (newState, value) = liveOperand.toRequiredNativeType(currentState)(nativeType)
      currentState = newState

      value
    }

    val irRestOperands = if (signature.hasRestArg) {
      // Drop all the arguments we've already passed
      val restOperands = liveOperands.drop(signature.fixedArgs.length)

      // Box all of our rest operands
      val restBoxedValues = restOperands map { restOperand => 
        val (newState, value) = restOperand.toRequiredNativeType(currentState)(nfi.BoxedValue(bt.BoxedDatum))
        currentState = newState

        value
      }

      // Build a proper list with the rest of our arguments
      val (newState, listValue) = GenProperList(currentState)(restBoxedValues)
      currentState = newState

      listValue :: Nil
    }
    else {
      Nil
    }

    val irOperands = irFixedOperands ++ irRestOperands

    // Get our IrSignature
    val irSignature = NativeSignatureToIr(signature)

    signature.returnType match {
      case Some(returnType) =>
        val Some(nativeReturn) = currentState.currentBlock.call(Some("ret"))(irSignature, procedure.functionPointer, irOperands)

        NativeToLiveValue(currentState)(returnType, nativeReturn)
      
      case None =>
        currentState.currentBlock.call(None)(irSignature, procedure.functionPointer, irOperands)

        (currentState, LiveUnspecific)
    }
  }

  def apply(state : GenerationState)(procedure : LiveValue, operands : List[LiveValue]) : (GenerationState, LiveValue) = {
    procedure match {
      case procedure : LiveProcedure =>
        genProcedureCall(state)(procedure, operands) 
    }
  }
}

