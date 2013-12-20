package io.llambda.typegen.writer.compiler

import io.llambda.typegen._

object WriteIntrinsicCellTypes extends writer.OutputWriter {
  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    val scalaBuilder = new ScalaBuilder
    
    scalaBuilder.appendRaw(writer.GeneratedClikeFileComment)

    scalaBuilder += "package io.llambda.compiler.frontend"
    scalaBuilder += "import io.llambda"
    scalaBuilder.sep()
    scalaBuilder += "import llambda.compiler.{celltype => ct}"
    scalaBuilder.sep()
    scalaBuilder += "object IntrinsicCellTypes"
    scalaBuilder.block {
      scalaBuilder += "def apply() : Map[String, ct.CellType] = Map("
      scalaBuilder.indented {
        val mapLines = for(cellClass <- processedTypes.cellClasses.values if !cellClass.internal) yield 
          s"""("${cellClass.names.schemeName}" -> ct.${cellClass.names.scalaObjectName})"""

        scalaBuilder.appendLines(mapLines, ",")
      }
      scalaBuilder += ")"
    }

    Map("compiler/src/main/scala/frontend/generated/IntrinsicCellTypes.scala" -> scalaBuilder.toString)
  }
}

