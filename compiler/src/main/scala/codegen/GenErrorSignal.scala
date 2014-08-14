package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.RuntimeErrorMessage

object GenErrorSignal {
  def apply(state : GenerationState)(worldPtr : IrValue, errorMessage : RuntimeErrorMessage, evidence : Option[IrValue] = None) = {
    val block = state.currentBlock
    val module = block.function.module
    val signalErrorDecl = RuntimeFunctions.signalError

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

    module.unlessDeclared(signalErrorDecl) {
      module.declareFunction(signalErrorDecl)
    }

    // Unwind any partial allocations we have
    GenCellAllocation.genDeallocation(state)(worldPtr)

    // Build our evidence - assume it's a datum cell
    val evidencePtr = evidence.map(
      ct.AnyCell.genPointerBitcast(block)(_)
    ).getOrElse(
      NullPointerConstant(PointerType(ct.AnyCell.irType))
    )

    // Get a pointer to the first element
    val stringStartPtr = ElementPointerConstant(
      IntegerType(8),
      stringConstantVar.variable,
      indices=List(0, 0),
      inbounds=true)

    state.terminateFunction(() => {
      // Call _lliby_fatal
      block.callDecl(None)(signalErrorDecl, List(worldPtr, stringStartPtr, evidencePtr))

      // Terminate the failure block
      block.unreachable
    })
  }
}
