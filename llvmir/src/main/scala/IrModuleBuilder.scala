package io.llambda.llvmir

import collection.mutable.ListBuffer

class IrModuleBuilder extends Irable {
  private case class NamedType(name : String, irType : IrType) extends Irable {
    private val escapedName = EscapeIdentifier(name)

    def toIr = s"%${escapedName} = type ${irType.toIr}"
  }

  private case class NamedMetadata(name : String, members : Seq[Metadata]) extends Irable {
    // Note the lack of types here - named metadata can only refer to other metadata
    def toIr =
      s"!${name} = !{" + members.map(_.toIr).mkString(", ") + "}" 
  }

  private val globalVariableDefs = new ListBuffer[IrGlobalVariableDef]
  private val functionDecls = new ListBuffer[IrFunctionDecl]
  private val functionDefs = new ListBuffer[IrFunctionBuilder]
  private val aliasDefs = new ListBuffer[IrAliasDef]
  private val namedTypes = new ListBuffer[NamedType]
  private val numberedMetadataDefs = new ListBuffer[NumberedMetadataDef]
  private val namedMetadata = new ListBuffer[NamedMetadata]

  private val declaredNames = collection.mutable.Set[String]()

  // This generates global names
  val nameSource = new GlobalNameSource

  val metadataIndexSource = new MetadataIndexSource

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

  def defineNumberedMetadata(numberedMetadataDef : NumberedMetadataDef) {
    numberedMetadataDefs += numberedMetadataDef
  }

  /** Assigns metadata a number and returns the NumberedMetadata referencing it */
  def numberMetadataNode(metadataNode : MetadataNode) : NumberedMetadata = {
    val newIndex = metadataIndexSource.allocate()
    val metadataDef = NumberedMetadataDef(newIndex, metadataNode)

    defineNumberedMetadata(metadataDef)

    metadataDef.numberedMetadata
  }

  def nameMetadata(name : String, members : Seq[Metadata]) {
    namedMetadata += NamedMetadata(name, members)
  }

  /** Constructs metadata to identify the compiler
    *
    * @param  identifier  Human readable identifier string for the compiler
    */
  def identifyCompiler(identifier : String) {
    val numberedMetadata = numberMetadataNode(
      UserDefinedMetadataNode(List(Some(
        MetadataString.fromUtf8String(identifier)
     )))
    )

    nameMetadata("llvm.ident", List(numberedMetadata))
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
      numberedMetadataDefs.toList ++
      namedMetadata.toList ++
      globalVariableDefs.toList ++
      functionDecls.toList ++
      functionDefs.toList ++
      aliasDefs.toList
    
    allIr.map(_.toIr).mkString("\n")
  }
}
