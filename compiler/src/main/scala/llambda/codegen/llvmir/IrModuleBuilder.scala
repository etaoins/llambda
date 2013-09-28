package llambda.codegen.llvmir

import collection.mutable.ListBuffer

class IrModuleBuilder extends Irable {
  private val globalVariableDefs = new ListBuffer[IrGlobalVariableDef]
  private val functionDecls = new ListBuffer[IrFunctionDecl]
  private val functionDefs = new ListBuffer[IrFunctionBuilder]

  // This generates global names
  protected implicit val nameSource = new GlobalNameSource

  def defineGlobalVariable(variable : IrGlobalVariableDef) {
    globalVariableDefs.append(variable)
  }

  def declareFunction(function : IrFunctionDecl) {
    functionDecls.append(function)
  }
  
  def defineFunction(function : IrFunctionBuilder) {
    functionDefs.append(function)
  }

  def toIr : String = {
    val allIr : List[Irable] =
      globalVariableDefs.toList ++ functionDecls.toList ++ functionDefs.toList
    
    allIr.map(_.toIr).mkString("\n")
  }
}
