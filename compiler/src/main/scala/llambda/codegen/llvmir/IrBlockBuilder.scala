package llambda.codegen.llvmir

import collection.mutable.ListBuffer

private[llvmir] abstract class IrInstrBuilder {
  // This contains our instructions as they're built
  private[llvmir] val instructions = new ListBuffer[String]

  private[llvmir] def allocateLocalVar(irType : FirstClassType, name : String)(implicit nameSource : LocalNameSource) : LocalVariable = {
    LocalVariable(nameSource.allocate(name), irType)
  }
}

class IrBlockBuilder extends IrInstrBuilder with Irable with TerminatorInstrs with MemoryInstrs with OtherInstrs {
  def comment(text : String) {
    instructions += s"; ${text}"
  }

  def toIr : String = {
    // Tab indent and join with newlines
    instructions.map("\t" + _).mkString("\n")
  }
}
