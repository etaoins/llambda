package io.llambda.compiler.codegen

import io.llambda.llvmir._

object GenGcCleanUpBlock {
  private val personalityResultType = StructureType(List(
    PointerType(IntegerType(8)),
    IntegerType(32)
  ))

  private val personalityFunctionDecl = IrFunctionDecl(
    result=IrFunction.Result(IntegerType(32)),
    name="__gcc_personality_v0",
    arguments=Nil,
    hasVararg=true
  )

  def apply(block : IrBlockBuilder, gcSlots : GcSlotGenerator) {
    val module = block.function.module

    module.unlessDeclared(personalityFunctionDecl) {
      module.declareFunction(personalityFunctionDecl)
    }

    val exceptionResultIr = block.landingpad("exceptionResultIr")(
      resultType=personalityResultType,
      personalityFunction=personalityFunctionDecl.irValue,
      clauses=Nil,
      cleanup=true
    )

    gcSlots.unrootAllAndTerminate(block)(() => {
      block.resume(exceptionResultIr)
    })
  }
}
