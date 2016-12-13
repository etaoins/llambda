package io.llambda.typegen.writer.runtime

import io.llambda.typegen._

/** Builds indented C++ source strings */
class CppBuilder extends writer.SourceBuilder {
  protected val indentString = "\t"

  protected def buildBlockStart() {
    // Braces are on their own line
    this += "{"
  }

  protected def buildBlockEnd() {
    this += "}"
  }
}

/** Builds indented C++ header source with include guards */
class CppIncludeBuilder(guardName: String) extends CppBuilder {
  override def toString :String = {
    s"#ifndef ${guardName}\n" +
    s"#define ${guardName}\n" +
    "\n" +
    super.toString +
    s"#endif\n"
  }
}
