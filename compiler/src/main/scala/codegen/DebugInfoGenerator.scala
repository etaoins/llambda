package io.llambda.compiler.codegen
import io.llambda

import java.io.File

import llambda.compiler.CompileConfig
import llambda.llvmir

class DebugInfoGenerator(module : llvmir.IrModuleBuilder, compileConfig : CompileConfig, compilerIdentifier : String, entryFilenameOpt : Option[String]) {
  val definedFilePathMetadata = new collection.mutable.HashMap[String, llvmir.NumberedMetadata]

  def metadataForFilePath(filePath : String) : llvmir.NumberedMetadata =
    // Re-use an existing metadata node if we've already defined one
    definedFilePathMetadata.getOrElseUpdate(filePath, { 
      // Nothing existing, create a new one
      module.numberMetadataNode(
        llvmir.debug.FilePathMetadata.fromFile(new File(filePath))
      )
    })

  def finish() {
    // Add our module flags
    val moduleFlags = List(
      module.numberMetadataNode(
        llvmir.debug.DwarfVersionMetadata(2)
      ),
      module.numberMetadataNode(
        llvmir.debug.DebugInfoVersionMetadata(1)
      )
    )

    module.nameMetadata("llvm.module.flags", moduleFlags)

    // Add the CU metadata
    val numberedCuMetadata = module.numberMetadataNode(
      llvmir.debug.CompileUnitMetadata(
        sourcePath=metadataForFilePath(entryFilenameOpt.getOrElse("(unnamed)")),
        dwarfLanguage=0x9393, // This is in the DWARF user range. It's ~'ll'
        producer=compilerIdentifier,
        optimised=(compileConfig.optimizeLevel > 0)
      )
    )

    module.nameMetadata("llvm.dbg.cu", List(numberedCuMetadata))
  }
}
