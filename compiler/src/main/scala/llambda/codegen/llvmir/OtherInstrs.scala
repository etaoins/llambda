package llambda.codegen.llvmir

import llambda.InternalCompilerErrorException

private[llvmir] trait OtherInstrs extends IrBuilder {
  protected case class PhiSource(value : IrValue, label : IrLabel)

  protected def phi(sources : PhiSource*) : LocalVariable = {
    if (sources.isEmpty) {
      throw new InternalCompilerErrorException("Attempted phi with no sources")
    }

    val resultType = sources.map(_.value.irType) reduceLeft { (currentType, newType) =>
      if (currentType != newType) {
        throw new InternalCompilerErrorException("Attempted phi with incompatible types")
      }

      newType
    }

    val resultVar = allocateLocalVar(resultType)

    val sourceIr = (sources map { source =>
      s"[ ${source.value.toIr}, ${source.label.toIr} ]"
    }).mkString(", ")

    instructions += s"${resultVar.toIr} = phi ${resultType.toIr} $sourceIr"

    resultVar
  }
}
