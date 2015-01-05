package io.llambda.llvmir

private[llvmir] trait AggregateInstrs extends IrInstrBuilder {
  def extractvalue(resultDest : ResultDestination)(valueType : FirstClassType, aggregateValue : IrValue, indices : Seq[Long]) : LocalVariable = {
    val resultVar = resultDest.asLocalVariable(nameSource, valueType)

    aggregateValue.irType match {
      case _ : AggregateType | _ : UserDefinedType =>
      case _ =>
        throw new InconsistentIrException("Attempted extractvalue on non-aggregate type")
    }

    if (indices.length < 1) {
      throw new InconsistentIrException("Attempted extractvalue with insufficient indices")
    }

    val baseIr = s"${resultVar.toIr} = extractvalue ${aggregateValue.toIrWithType}"

    val irParts = baseIr :: indices.toList.map(_.toString)
    addInstruction(irParts.mkString(", "))

    resultVar
  }
}
