package llambda.codegen.llvmir

import collection.mutable.ListBuffer

class IrModuleBuilder extends Irable {
  private val globalVariableDefs = new ListBuffer[IrGlobalVariableDef]
  private val functionDecls = new ListBuffer[IrFunctionDecl]
  private val functionDefs = new ListBuffer[IrFunctionBuilder]

  private val declaredNames = collection.mutable.Set[String]()

  // This generates global names
  val nameSource = new GlobalNameSource

  def defineGlobalVariable(variableDef : IrGlobalVariableDef) {
    globalVariableDefs.append(variableDef)
    declaredNames += variableDef.name
  }

  def declareFunction(function : IrFunctionDecl) {
    functionDecls.append(function)
    declaredNames += function.name
  }
  
  def defineFunction(function : IrFunctionBuilder) {
    functionDefs.append(function)
    declaredNames += function.name
  }

  def isDeclared(name : String) = 
    declaredNames.contains(name)

  def unlessDeclared(name : String)(unlessBlock : => Unit) : Unit = {
    if (!isDeclared(name)) {
      unlessBlock
    }
  }

  def unlessDeclared(global : IrNamedGlobal)(unlessBlock : => Unit) : Unit =
    unlessDeclared(global.name)(unlessBlock)

  def toIr : String = {
    val allIr : List[Irable] =
      globalVariableDefs.toList ++ functionDecls.toList ++ functionDefs.toList
    
    allIr.map(_.toIr).mkString("\n")
  }
}
