package llambda.codegen

import llambda.codegen.llvmir._
import llambda.codegen.{boxedtype => bt}

object GenFatalError {
  def apply(module : IrModuleBuilder, block : IrBlockBuilder)(errorName : String, errorText : String, evidence : Option[IrValue] = None) = {
    // Define the error string
    val stringConstantName = s"${errorName}ErrorString"
    val stringConstantVar = IrGlobalVariableDef(
      name=stringConstantName,
      initializer=StringConstant(errorText),
      visibility=Visibility.Hidden,
      constant=true,
      unnamedAddr=true)

    module.unlessDeclared(stringConstantName) {
      module.defineGlobalVariable(stringConstantVar)
    }

    // Define _lliby_fatal
    val llibyFatalDecl = IrFunctionDecl(
      result=IrFunction.Result(VoidType),
      name="_lliby_fatal",
      arguments=List(
        IrFunction.Argument(PointerType(IntegerType(8)), Set(IrFunction.NoCapture)),
        IrFunction.Argument(PointerType(bt.BoxedDatum.irType), Set(IrFunction.NoCapture))
      ),
      attributes=Set(IrFunction.NoReturn, IrFunction.ReadOnly, IrFunction.NoUnwind)
    )

    module.unlessDeclared(llibyFatalDecl) {
      module.declareFunction(llibyFatalDecl)
    }

    // Build our evidence - assume it's a boxed datum
    val evidencePtr = evidence.map(
      bt.BoxedDatum.genPointerBitcast(block)(_)
    ).getOrElse(
      NullPointerConstant(PointerType(bt.BoxedDatum.irType))
    )

    // Get a pointer to the first element
    val stringStartPtr = ElementPointerConstant(
      IntegerType(8),
      stringConstantVar.variable,
      indices=List(0, 0),
      inbounds=true)

    // Call _lliby_fatal
    block.callDecl(None)(llibyFatalDecl, List(stringStartPtr, evidencePtr))

    // Terminate the failure block
    block.unreachable
  }
}
