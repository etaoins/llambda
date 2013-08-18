package llambda.codegen.llvmir

import collection.mutable.ListBuffer

class IrModule extends Irable {
  private val globalVariableDefs = new ListBuffer[IrGlobalVariableDef]
  private val functionDecls = new ListBuffer[IrFunctionDecl]
  private val functionDefs = new ListBuffer[IrFunctionDef]

  // This generates global names
  protected implicit val nameSource = new GlobalNameSource

  final def defineGlobalVariable(variable : IrGlobalVariableDef) {
    globalVariableDefs.append(variable)
  }

  final def declareFunction(function : IrFunctionDecl) {
    functionDecls.append(function)
  }
  
  final def defineFunction(function : IrFunctionDef) {
    functionDefs.append(function)
  }

  def toIr : String = {
    val allIr : List[Irable] =
      globalVariableDefs.toList ++ functionDecls.toList ++ functionDefs.toList
    
    allIr.map(_.toIr).mkString("\n")
  }
}
