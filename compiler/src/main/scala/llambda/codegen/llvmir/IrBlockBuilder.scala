package llambda.codegen.llvmir

import collection.mutable.ListBuffer

private[llvmir] abstract class IrInstrBuilder(nameSource : LocalNameSource) {
  // This contains our instructions as they're built
  private[llvmir] val instructions = new ListBuffer[String]

  private[llvmir] def allocateLocalVar(irType : FirstClassType, name : String) : LocalVariable = {
    LocalVariable(nameSource.allocate(name), irType)
  }
}

class IrBlockBuilder()(implicit nameSource : LocalNameSource) extends IrInstrBuilder(nameSource) with Irable with TerminatorInstrs with MemoryInstrs with BitwiseInstrs with ConversionInstrs with OtherInstrs {
  def comment(text : String) {
    instructions += s"; ${text}"
  }

  def toIr : String = {
    // Tab indent and join with newlines
    instructions.map("\t" + _).mkString("\n")
  }
}
