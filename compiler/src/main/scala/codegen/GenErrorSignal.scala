package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.RuntimeErrorMessage

object GenErrorSignal {
  def apply(state : GenerationState)(worldPtr : IrValue, errorMessage : RuntimeErrorMessage, evidence : Option[IrValue] = None) = {
    val block = state.currentBlock
    val module = block.function.module

    // Define the error string
    val stringConstantName = s"${errorMessage.name}ErrorString"
    val stringConstantVar = IrGlobalVariableDef(
      name=stringConstantName,
      initializer=StringConstant.fromUtf8String(errorMessage.text),
      visibility=Visibility.Hidden,
      constant=true,
      unnamedAddr=true)

    module.unlessDeclared(stringConstantName) {
      module.defineGlobalVariable(stringConstantVar)
    }

    // Define _lliby_signal_error
    val llibySignalErrorDecl = IrFunctionDecl(
      result=IrFunction.Result(VoidType),
      name="_lliby_signal_error",
      arguments=List(
        IrFunction.Argument(PointerType(WorldValue.irType)),
        IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)),
        IrFunction.Argument(PointerType(ct.DatumCell.irType))
      )
    )

    module.unlessDeclared(llibySignalErrorDecl) {
      module.declareFunction(llibySignalErrorDecl)
    }

    // Unwind any partial allocations we have
    GenCellAllocation.genDeallocation(state)(worldPtr)

    // Build our evidence - assume it's a datum cell
    val evidencePtr = evidence.map(
      ct.DatumCell.genPointerBitcast(block)(_)
    ).getOrElse(
      NullPointerConstant(PointerType(ct.DatumCell.irType))
    )

    // Get a pointer to the first element
    val stringStartPtr = ElementPointerConstant(
      IntegerType(8),
      stringConstantVar.variable,
      indices=List(0, 0),
      inbounds=true)

    // Call _lliby_fatal
    block.callDecl(None)(llibySignalErrorDecl, List(worldPtr, stringStartPtr, evidencePtr))

    // Terminate the failure block
    block.unreachable
  }
}
