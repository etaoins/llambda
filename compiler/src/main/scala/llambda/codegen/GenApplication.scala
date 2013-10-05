package llambda.codegen

import llambda.codegen.llvmir._
import llambda.nfi.NativeSignature
import llambda.{NotImplementedException, InternalCompilerErrorException}

object GenApplication {
  private case class FunctionCallResult(
    exitBlock : IrBlockBuilder,
    returnValue : LiveValue 
  )

  private def genProcedureCall(state : GenerationState)(procedure : LiveProcedure, liveOperands : List[LiveValue]) : FunctionCallResult = {
    val signature = procedure.signature

    if (signature.hasSelfArg) {
      throw new NotImplementedException("Self arguments not implemented")
    }
    
    if (signature.hasRestArg) {
      throw new NotImplementedException("Rest arguments not implemented")
    }

    if (signature.returnType.isDefined) {
      throw new NotImplementedException("Procedure returns not implemented")
    }

    if (signature.fixedArgs.length != procedure.signature.fixedArgs.length) {
      // XXX: Make the frontend catch this
      throw new InternalCompilerErrorException("Attempted to generate function application with incompatible arity")
    }

    // Convert our operands to IrValues of the expected type
    val irOperands = liveOperands.zip(signature.fixedArgs).map { case (liveOperand, nativeType) =>
      LiveValueToNative(state.module, state.currentBlock)(liveOperand, nativeType)
    }

    // Get our IrSignature
    val irSignature = NativeSignatureToIr(signature)

    state.currentBlock.call(None)(irSignature, procedure.functionPointer, irOperands)

    FunctionCallResult(state.currentBlock, LiveUnspecific) 
  }

  def apply(state : GenerationState)(procedure : LiveValue, operands : List[LiveValue]) : ExpressionResult = {
    procedure match {
      case procedure : LiveProcedure =>
        val callResult = genProcedureCall(state)(procedure, operands) 

        ExpressionResult(
          state=state.copy(currentBlock=callResult.exitBlock),
          value=callResult.returnValue)
    }
  }
}

