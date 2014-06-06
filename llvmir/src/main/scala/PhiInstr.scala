package io.llambda.llvmir

case class PhiSource(value : IrValue, block : IrBlockBuilder)

private[llvmir] trait PhiInstr extends IrInstrBuilder {
  def phi(resultDest : ResultDestination)(sources : PhiSource*) : LocalVariable = {
    if (sources.isEmpty) {
      throw new InconsistentIrException("Attempted phi with no sources")
    }

    val resultType = sources.map(_.value.irType) reduceLeft { (currentType, newType) =>
      if (currentType != newType) {
        throw new InconsistentIrException("Attempted phi with incompatible types")
      }

      newType
    }

    val resultVar = resultDest.asLocalVariable(nameSource, resultType)

    val sourceIr = (sources map { source =>
      s"[ ${source.value.toIr}, %${source.block.label} ]"
    }).mkString(", ")

    addInstruction(s"${resultVar.toIr} = phi ${resultType.toIr} $sourceIr")

    resultVar
  }

}
