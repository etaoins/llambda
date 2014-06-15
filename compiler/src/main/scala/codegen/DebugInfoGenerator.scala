package io.llambda.compiler.codegen
import io.llambda

import java.io.File

import llambda.compiler.debug
import llambda.compiler.{ContextLocated, CompileConfig}
import llambda.llvmir

class DebugInfoGenerator(module : llvmir.IrModuleBuilder, compileConfig : CompileConfig, compilerIdentifier : String, entryFilenameOpt : Option[String]) {
  private val definedFilePathMetadata = new collection.mutable.HashMap[String, llvmir.NumberedMetadata]
  private val sourceContextMetadata = new collection.mutable.HashMap[debug.SourceContext, Option[llvmir.NumberedMetadata]]
  private lazy val subroutineTypeMetadata : llvmir.NumberedMetadata = {
    val subroutineTypeNode = llvmir.debug.SubroutineTypeMetadata
    module.numberMetadataNode(subroutineTypeNode)
  }

  def metadataForFilePath(filePath : String) : llvmir.NumberedMetadata =
    // Re-use an existing metadata node if we've already defined one
    definedFilePathMetadata.getOrElseUpdate(filePath, { 
      // Nothing existing, create a new one
      module.numberMetadataNode(
        llvmir.debug.FilePathMetadata.fromFile(new File(filePath))
      )
    })

  private def contextMetadataForSourceContext(sourceContext : debug.SourceContext) : Option[llvmir.NumberedMetadata] = 
    sourceContextMetadata.getOrElseUpdate(sourceContext, {
      sourceContext match {
        case debug.FileContext(filename) =>
          val fileContextNode = llvmir.debug.FileContextMetadata(
            metadataForFilePath(filename)
          )

          Some(module.numberMetadataNode(fileContextNode))
          
        case subprogram : debug.SubprogramContext => 
          val subprogramNodeOpt =
            for(parentMetadata <- contextMetadataForSourceContext(subprogram.parentContext);
                filename <- subprogram.filenameOpt)
            yield llvmir.debug.SubprogramMetadata(
              sourcePath=metadataForFilePath(filename),
              contextDescriptor=parentMetadata,
              name=subprogram.sourceNameOpt.getOrElse(""),
              displayName=subprogram.sourceNameOpt.getOrElse(""),
              definitionLine=subprogram.startLine,
              typeDescriptor=subroutineTypeMetadata,
              compileUnitLocal=true,
              definedInCompileUnit=true,
              optimised=(compileConfig.optimizeLevel > 0),
              scopeStartLine=subprogram.startLine
            )
        
          subprogramNodeOpt.map(module.numberMetadataNode)

        case debug.UnknownContext =>
          None
      }
    })

  def metadataForContextLocated(located : ContextLocated) = {
    val locationMetadataOpt = 
      for(location <- located.locationOpt;
          sourceContext <- located.contextOpt;
          contextMetadata <- contextMetadataForSourceContext(sourceContext))
      yield llvmir.debug.LocationMetadata(location.line, location.column, contextMetadata, None)

    Map(locationMetadataOpt.toSeq.map("dbg" -> _) : _*)
  }

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

    // Make a list of subprograms
    val allSubprograms = (sourceContextMetadata.flatMap {
      case (_ : debug.SubprogramContext, Some(numberedMetadata)) =>
        Some(numberedMetadata)

      case _ =>
        None
    }).toList

    // Add the CU metadata
    val numberedCuMetadata = module.numberMetadataNode(
      llvmir.debug.CompileUnitMetadata(
        sourcePath=metadataForFilePath(entryFilenameOpt.getOrElse("(unnamed)")),
        dwarfLanguage=0x9393, // This is in the DWARF user range. It's ~'ll'
        producer=compilerIdentifier,
        optimised=(compileConfig.optimizeLevel > 0),
        subprograms=allSubprograms
      )
    )

    module.nameMetadata("llvm.dbg.cu", List(numberedCuMetadata))
  }
}
