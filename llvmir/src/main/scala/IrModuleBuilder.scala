package io.llambda.llvmir

import collection.mutable.ListBuffer

class IrModuleBuilder extends Irable {
  private case class NamedType(name : String, irType : IrType) extends Irable {
    private val escapedName = EscapeIdentifier(name)

    def toIr = s"%${escapedName} = type ${irType.toIr}"
  }

  private val globalVariableDefs = new ListBuffer[IrGlobalVariableDef]
  private val functionDecls = new ListBuffer[IrFunctionDecl]
  private val functionDefs = new ListBuffer[IrFunctionBuilder]
  private val aliasDefs = new ListBuffer[IrAliasDef]
  private val namedTypes = new ListBuffer[NamedType]
  private val metadataDefs = new ListBuffer[MetadataDef]

  private val declaredNames = collection.mutable.Set[String]()

  // This generates global names
  val nameSource = new GlobalNameSource

  val metadataNameSource = new MetadataNameSource

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

  def defineAlias(aliasDef : IrAliasDef) {
    aliasDefs.append(aliasDef)
    declaredNames += aliasDef.name
  }

  def nameType(name : String, irType : IrType) : UserDefinedType = {
    namedTypes += NamedType(name, irType)
    declaredNames += name

    UserDefinedType(name)
  }

  def defineMetadata(metadataDef : MetadataDef) {
    metadataDefs += metadataDef
  }

  /** Assigns metadata a new name and returns the NamedMetadata referencing it */
  def nameMetadataNode(metadataNode : MetadataNode) : NamedMetadata = {
    val newIndex = metadataNameSource.allocate()
    val metadataDef = MetadataDef(newIndex, metadataNode)

    defineMetadata(metadataDef)

    metadataDef.namedMetadata
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
      metadataDefs.toList ++
      globalVariableDefs.toList ++
      functionDecls.toList ++
      functionDefs.toList ++
      aliasDefs.toList
    
    allIr.map(_.toIr).mkString("\n")
  }
}
