package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

protected[llvmir] case class PhiSource(value : IrValue, block : IrBlockBuilder)

private[llvmir] trait PhiInstr extends IrInstrBuilder {
  def phi(resultName : String)(sources : PhiSource*) : LocalVariable = {
    if (sources.isEmpty) {
      throw new InternalCompilerErrorException("Attempted phi with no sources")
    }

    val resultType = sources.map(_.value.irType) reduceLeft { (currentType, newType) =>
      if (currentType != newType) {
        throw new InternalCompilerErrorException("Attempted phi with incompatible types")
      }

      newType
    }

    val resultVar = allocateLocalVar(resultType, resultName)

    val sourceIr = (sources map { source =>
      s"[ ${source.value.toIr}, %${source.block.label} ]"
    }).mkString(", ")

    instructions += s"${resultVar.toIr} = phi ${resultType.toIr} $sourceIr"

    resultVar
  }

}
