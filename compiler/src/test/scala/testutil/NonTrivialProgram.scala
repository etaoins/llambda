package io.llambda.compiler.testutil
import io.llambda
import scala.io.Source

import llambda.compiler._


/** Provides data and an include path for a generic "non-trivial" program
  *
  * This is used by tests that want to check for certain high-level behaviours such as stable IR generation or
  * consistent source location without attempting to exhaustively check all possible program structures. Currently
  * this is using the "Life" example from R7RS
  */
object NonTrivialProgram {
  private val lifeBaseDir = "life-example/"
  private val lifeBaseUrl = getClass.getClassLoader.getResource(lifeBaseDir)

  lazy val data: List[ast.Datum] = {
    val lifeProgramPath = s"${lifeBaseDir}life.scm"

    val stream = getClass.getClassLoader.getResourceAsStream(lifeProgramPath)

    if (stream == null) {
      throw new Exception(s"Unable to load Scheme test source from ${lifeProgramPath}")
    }

    val lifeProgramSource = Source.fromInputStream(stream, "UTF-8").mkString
    SchemeParser.parseStringAsData(lifeProgramSource, Some(s":/${lifeProgramPath}"))
  }

  lazy val includePath = frontend.IncludePath(List(lifeBaseUrl))
}
