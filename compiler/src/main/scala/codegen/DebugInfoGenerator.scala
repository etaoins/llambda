package io.llambda.compiler.codegen
import io.llambda

import java.io.File

import llambda.compiler.debug
import llambda.compiler.planner
import llambda.compiler.{ContextLocated, InlinePathEntry, SourceLocation, CompileConfig}
import llambda.llvmir

class DebugInfoGenerator(module : llvmir.IrModuleBuilder, functions : Map[String, planner.PlannedFunction], compileConfig : CompileConfig, compilerIdentifier : String, entryFilenameOpt : Option[String]) {
  private val definedFilePathMetadata = new collection.mutable.HashMap[String, llvmir.NumberedMetadata]
  private val sourceContextMetadata = new collection.mutable.HashMap[debug.SourceContext, Option[llvmir.NumberedMetadata]]
  private val locationMetadata = new collection.mutable.HashMap[llvmir.debug.LocationMetadata, llvmir.NumberedMetadata]

  private lazy val subroutineTypeMetadata : llvmir.NumberedMetadata = {
    val subroutineTypeNode = llvmir.debug.SubroutineTypeMetadata
    module.numberMetadataNode(subroutineTypeNode)
  }

  // Build a map of SubprogramContexts to their LLVM values
  val subprogramToLlvmFunction = (functions.flatMap { case (symbol, plannedFunction) =>
    // Find the IR type of the procedure
    val irType = ProcedureSignatureToIr(plannedFunction.signature).irType

    // Turn it in to a global variable
    val globalVariable = llvmir.GlobalVariable(symbol, llvmir.PointerType(irType))

    plannedFunction.debugContextOpt.map(_ -> globalVariable)
  }) : Map[debug.SubprogramContext, llvmir.GlobalVariable]

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
              definitionLine=subprogram.startLocation.line,
              typeDescriptor=subroutineTypeMetadata,
              compileUnitLocal=true,
              definedInCompileUnit=true,
              optimised=(compileConfig.optimizeLevel > 0),
              llvmFunctionOpt=subprogramToLlvmFunction.get(subprogram),
              scopeStartLine=subprogram.startLocation.line
            )
        
          subprogramNodeOpt.map(module.numberMetadataNode)

        case debug.UnknownContext =>
          None
      }
    })

  private def dbgMetadataWithInlinePath(locationOpt : Option[SourceLocation], contextOpt : Option[debug.SourceContext], inlinePath : List[InlinePathEntry]) : Option[llvmir.Metadata] = {
    val originalScopeOpt = inlinePath match {
      case pathEntry :: tail =>
        // Recursively build our inline path metadata
        dbgMetadataWithInlinePath(pathEntry.locationOpt, pathEntry.contextOpt, tail)

      case _ =>
        // No more inline path
        None
    }

    val metadataNodeOpt = 
      for(location <- locationOpt;
          sourceContext <- contextOpt;
          metadataNode <- contextMetadataForSourceContext(sourceContext))
      yield llvmir.debug.LocationMetadata(
        line=location.line,
        column=location.column,
        scope=metadataNode,
        originalScopeOpt=originalScopeOpt
      )

    metadataNodeOpt map { metadataNode =>
      // Cache the numbered metadata node for this
      // This is especially helpful to reduce the duplication during inlining where many expressions can be inlined
      // from the same location
      locationMetadata.getOrElseUpdate(metadataNode, {
        module.numberMetadataNode(metadataNode)
      })
    }
  }
  
  private def dbgMetadataForContextLocated(located : ContextLocated) : Option[llvmir.Metadata] = {
    dbgMetadataWithInlinePath(located.locationOpt, located.contextOpt, located.inlinePath)
  }

  def metadataForContextLocated(located : ContextLocated) : Map[String, llvmir.Metadata] = {
    val contextMetdataOpt = dbgMetadataForContextLocated(located)

    Map(contextMetdataOpt.toSeq.map("dbg" -> _) : _*)
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
