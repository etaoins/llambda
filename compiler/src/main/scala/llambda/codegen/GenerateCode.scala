package llambda.codegen

import llambda._
import scala.io.Source

object GenerateCode {
  def resourceAsString(resourcePath : String) : String = {
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    io.Source.fromInputStream(stream).mkString
  }

  def preludeIr : String = {
    List(
      resourceAsString("generated/boxedTypes.ll"),
      resourceAsString("defines.ll")
    ) mkString "\n"
  }

  def apply(expressions : List[et.Expression]) : String = {
    preludeIr
  }
}

