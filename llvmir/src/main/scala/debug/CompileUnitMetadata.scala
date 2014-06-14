package io.llambda.llvmir.debug

import io.llambda.llvmir._

case class CompileUnitMetadata(
    sourcePath : Metadata,
    dwarfLanguage : Int,
    producer : String,
    optimised : Boolean = false,
    flags : String = "",
    runtimeVersion : Int = 0,
    enums : List[Metadata] = Nil,
    retainedTypes : List[Metadata] = Nil,
    subprograms : List[Metadata] = Nil,
    globalVariables : List[Metadata] = Nil,
    importedEntities : List[Metadata] = Nil,
    splitDebugFilename : String = ""
) extends MetadataNode {
  def listToMetadata(metadataList : List[Metadata]) : Metadata = {
    if (metadataList.isEmpty) {
      // Not sure why this is a thing
      UserDefinedMetadataNode(List(
        Some(IntegerConstant(IntegerType(32), 0))
      ))
    }
    else {
      UserDefinedMetadataNode(metadataList.map(Some(_)))
    }
  }

  val memberOpts = List(
    IntegerConstant(IntegerType(32), 786449), // DW_TAG_compile_unit
    sourcePath,
    IntegerConstant(IntegerType(32), dwarfLanguage),
    MetadataString.fromUtf8String(producer),
    IntegerConstant(IntegerType(1), if (optimised) 1 else 0),
    MetadataString.fromUtf8String(flags),
    IntegerConstant(IntegerType(32), runtimeVersion),
    listToMetadata(enums),
    listToMetadata(retainedTypes),
    listToMetadata(subprograms),
    listToMetadata(globalVariables),
    listToMetadata(importedEntities),
    MetadataString.fromUtf8String(splitDebugFilename)
  ).map(Some(_))
}
