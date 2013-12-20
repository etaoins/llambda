package io.llambda.typegen.writer.compiler

import io.llambda.typegen._

/** Builds idented Scala source */
class ScalaBuilder extends writer.SourceBuilder {
  protected val indentString = "  "

  protected def buildBlockStart() {
    sourceString = new StringBuilder(sourceString.stripLineEnd)
    sourceString ++= " {\n"
  }

  protected def buildBlockEnd() {
    this += "}"
  }
}

