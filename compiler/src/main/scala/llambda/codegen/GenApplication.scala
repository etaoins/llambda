package llambda.codegen

import llambda.codegen.llvmir._
import llambda.nfi.NativeSignature
import llambda.{NotImplementedException, InternalCompilerErrorException}

object GenApplication {
  private def genProcedureCall(initialState : GenerationState)(procedure : LiveProcedure, liveOperands : List[LiveValue]) : ExpressionResult = {
    val signature = procedure.signature

    if (signature.hasSelfArg) {
      throw new NotImplementedException("Self arguments not implemented")
    }
    
    if (signature.hasRestArg) {
      throw new NotImplementedException("Rest arguments not implemented")
    }

    if (signature.fixedArgs.length != procedure.signature.fixedArgs.length) {
      // XXX: Make the frontend catch this
      throw new InternalCompilerErrorException("Attempted to generate function application with incompatible arity")
    }

    // Give up and use a var here
    // foldLeft + zip ends up being a right mess
    var currentState = initialState 

    // Convert our operands to IrValues of the expected type
    val irOperands = liveOperands.zip(signature.fixedArgs).map { case (liveOperand, nativeType) =>
      val (newState, value) = liveOperand.toRequiredNativeType(currentState)(nativeType)
      currentState = newState

      value
    }

    // Get our IrSignature
    val irSignature = NativeSignatureToIr(signature)

    signature.returnType match {
      case Some(returnType) =>
        val Some(nativeReturn) = currentState.currentBlock.call(Some("ret"))(irSignature, procedure.functionPointer, irOperands)

        val (finalState, liveValue) = NativeToLiveValue(currentState)(returnType, nativeReturn)

        ExpressionResult(
          state=finalState, 
          value=liveValue
        ) 
      
      case None =>
        currentState.currentBlock.call(None)(irSignature, procedure.functionPointer, irOperands)

        ExpressionResult(
          state=currentState, 
          value=LiveUnspecific
        ) 
    }
  }

  def apply(state : GenerationState)(procedure : LiveValue, operands : List[LiveValue]) : ExpressionResult = {
    procedure match {
      case procedure : LiveProcedure =>
        genProcedureCall(state)(procedure, operands) 
    }
  }
}

