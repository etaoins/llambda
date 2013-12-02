package llambda.codegen

import llambda.codegen.llvmir._

object GenRecordDataAllocate {
  def apply(module : IrModuleBuilder, block : IrBlockBuilder)(recordDataIrType : UserDefinedType) : IrValue = {
    // Declare _lliby_record_data_alloc
    val llibyRecordDataAlloc = IrFunctionDecl(
      result=IrFunction.Result(PointerType(IntegerType(8))),
      name="_lliby_record_data_alloc",
      arguments=List(
        IrFunction.Argument(IntegerType(64))
      ),
      attributes=Set(IrFunction.NoUnwind)
    )

    module.unlessDeclared(llibyRecordDataAlloc) {
      module.declareFunction(llibyRecordDataAlloc)
    }

    val irSize = GenSizeOf(block)(recordDataIrType)

    val uncastData = block.callDecl(Some("rawRecordData"))(llibyRecordDataAlloc, List(irSize)).get
    block.bitcastTo("castRecordData")(uncastData, PointerType(recordDataIrType))
  }
}
