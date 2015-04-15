package io.llambda.llvmir.debug

import io.llambda.llvmir._

case class SubprogramMetadata(
    sourcePath : Metadata,
    contextDescriptor : Metadata,
    name : String,
    displayName : String,
    mipsLinkageName : String = "",
    definitionLine : Int,
    typeDescriptor : Metadata,
    compileUnitLocal : Boolean,
    definedInCompileUnit : Boolean = true,
    virtuality : Int = 0,
    virtualIndex : Int = 0,
    baseTypeOpt : Option[Metadata] = None,
    flags : Int = 0,
    optimised : Boolean = false,
    llvmFunctionOpt : Option[IrConstant] = None,
    functionTemplateParametersOpt : Option[Metadata] = None,
    functionDeclDescriptorOpt : Option[Metadata] = None,
    functionVariables : List[Metadata] = Nil,
    scopeStartLine : Int

) extends MetadataNode {
  val operandOpts = List(
    Some(IntegerConstant(IntegerType(32), 786478)), // DW_TAG_subprogram
    Some(sourcePath),
    Some(contextDescriptor),
    Some(MetadataString.fromUtf8String(name)),
    Some(MetadataString.fromUtf8String(displayName)),
    Some(MetadataString.fromUtf8String(mipsLinkageName)),
    Some(IntegerConstant(IntegerType(32), definitionLine)),
    Some(typeDescriptor),
    Some(IntegerConstant(IntegerType(1), if (compileUnitLocal) 1 else 0)),
    Some(IntegerConstant(IntegerType(1), if (definedInCompileUnit) 1 else 0)),
    Some(IntegerConstant(IntegerType(32), virtuality)),
    Some(IntegerConstant(IntegerType(32), virtualIndex)),
    baseTypeOpt,
    Some(IntegerConstant(IntegerType(32), flags)),
    Some(IntegerConstant(IntegerType(1), if (optimised) 1 else 0)),
    llvmFunctionOpt,
    functionTemplateParametersOpt,
    functionDeclDescriptorOpt,
    Some(listToNotNullMetadata(functionVariables)),
    Some(IntegerConstant(IntegerType(32), scopeStartLine))
  )
}
