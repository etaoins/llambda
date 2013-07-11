package llambda.codegen.llvmir

import collection.mutable.ListBuffer

private[llvmir] abstract class IrBuilder {
  // This contains our instructions as they're built
  private[llvmir] val instructions = new ListBuffer[String]
  private[llvmir] var nextLocalVar = 0 : Integer

  private[llvmir] def allocateLocalVar(irType : FirstClassType) : LocalVariable = {
    nextLocalVar = nextLocalVar + 1

    LocalVariable(nextLocalVar.toString, irType)
  }
}

class IrBlock extends IrBuilder with Irable with TerminatorInstrs with OtherInstrs {
  protected def comment(text : String) {
    instructions += s"; ${text}"
  }

  def toIr : String = {
    // Tab indent and join with newlines
    instructions.map("\t" + _).mkString("\n")
  }
}
