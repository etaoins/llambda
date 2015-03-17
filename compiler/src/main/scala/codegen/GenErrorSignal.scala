package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{RuntimeErrorMessage, SourceLocated}

object GenErrorSignal {
  private val categoryIrType = IntegerType(16)
  private val pathIrType = PointerType(IntegerType(8))
  private val lineNumberIrType = IntegerType(32)

  private def defineConstantString(module : IrModuleBuilder)(constantName : String, text : String) : IrConstant = {
    val stringConstantVar = IrGlobalVariableDef(
      name=constantName,
      initializer=StringConstant.fromUtf8String(text),
      linkage=Linkage.Private,
      constant=true,
      unnamedAddr=true)

    module.unlessDeclared(constantName) {
      module.defineGlobalVariable(stringConstantVar)
    }

    // Get a pointer to the first element
    ElementPointerConstant(
      IntegerType(8),
      stringConstantVar.variable,
      indices=List(0, 0),
      inbounds=true
    )
  }

  def apply(state : GenerationState)(
      worldPtr : IrValue,
      errorMessage : RuntimeErrorMessage,
      evidence : Option[IrValue] = None, 
      locatedOpt : Option[SourceLocated] = None 
  ) = {
    val block = state.currentBlock
    val module = block.function.module
    val signalErrorDecl = RuntimeFunctions.signalError

    val categoryIr = IntegerConstant(categoryIrType, errorMessage.category.runtimeId)

    // Define the error string
    val messageStartPtr = defineConstantString(module)(s"${errorMessage.name}ErrorString", errorMessage.text)

    val locationOpt = for(located <- locatedOpt; location <- located.locationOpt) yield location

    val (fileIr, lineIr) = locationOpt match {
      case Some(location) =>
        val fileIr = location.filenameOpt.map({ filename =>
          defineConstantString(module)(s"${filename}Path", filename)
        }).getOrElse(NullPointerConstant(pathIrType))

        val lineIr = IntegerConstant(lineNumberIrType, location.line)

        (fileIr, lineIr)

      case None =>
        // No location information
        (NullPointerConstant(pathIrType), IntegerConstant(lineNumberIrType, 0))
    }

    // Unwind any partial allocations we have
    GenCellAllocation.genDeallocation(state)(worldPtr)

    // Build our evidence - assume it's a datum cell
    val evidencePtr = evidence.map(
      ct.AnyCell.genPointerBitcast(block)(_)
    ).getOrElse(
      NullPointerConstant(PointerType(ct.AnyCell.irType))
    )

    module.unlessDeclared(signalErrorDecl) {
      module.declareFunction(signalErrorDecl)
    }

    state.terminateFunction(() => {
      // Call llcore_signal_error
      block.callDecl(None)(signalErrorDecl, List(worldPtr, categoryIr, messageStartPtr, evidencePtr, fileIr, lineIr))

      // Terminate the failure block
      block.unreachable
    })
  }
}
