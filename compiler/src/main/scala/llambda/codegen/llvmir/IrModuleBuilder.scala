package llambda.codegen.llvmir

import collection.mutable.ListBuffer

class IrModuleBuilder extends Irable {
  private case class NamedType(name : String, irType : IrType) extends Irable {
    def toIr = s"%${name} = type ${irType.toIr}"
  }

  private val globalVariableDefs = new ListBuffer[IrGlobalVariableDef]
  private val functionDecls = new ListBuffer[IrFunctionDecl]
  private val functionDefs = new ListBuffer[IrFunctionBuilder]
  private val namedTypes = new ListBuffer[NamedType]
  private val tbaaNodes = new ListBuffer[IrTbaaNode]

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

  def nameType(name : String, irType : IrType) : UserDefinedType = {
    namedTypes += NamedType(name, irType)
    declaredNames += name

    UserDefinedType(name)
  }

  def defineTbaaNode(tbaaNode : IrTbaaNode) {
    tbaaNodes += tbaaNode
  }

  def isDeclared(name : String) : Boolean = 
    declaredNames.contains(name)
  
  def isDeclared(global : IrNamedGlobal) : Boolean = 
    isDeclared(global.name)

  def unlessDeclared(name : String)(unlessBlock : => Unit) : Unit = {
    if (!isDeclared(name)) {
      unlessBlock
    }
  }

  def unlessDeclared(global : IrNamedGlobal)(unlessBlock : => Unit) : Unit =
    unlessDeclared(global.name)(unlessBlock)

  def toIr : String = {
    val allIr : List[Irable] =
      namedTypes.toList ++
      tbaaNodes.toList ++
      globalVariableDefs.toList ++
      functionDecls.toList ++
      functionDefs.toList
    
    allIr.map(_.toIr).mkString("\n")
  }
}
