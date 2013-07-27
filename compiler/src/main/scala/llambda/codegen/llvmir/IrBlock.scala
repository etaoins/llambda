package llambda.codegen.llvmir

import collection.mutable.ListBuffer

private[llvmir] abstract class IrBuilder {
  // This contains our instructions as they're built
  private[llvmir] val instructions = new ListBuffer[String]

  private[llvmir] def allocateLocalVar(irType : FirstClassType)(implicit nameSource : LocalNameSource) : LocalVariable = {
    LocalVariable(nameSource.allocate(""), irType)
  }
}

class IrBlock extends IrBuilder with Irable with TerminatorInstrs with MemoryInstrs with OtherInstrs {
  protected def comment(text : String) {
    instructions += s"; ${text}"
  }

  def toIr : String = {
    // Tab indent and join with newlines
    instructions.map("\t" + _).mkString("\n")
  }
}
