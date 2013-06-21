package llambda.codegen

import llambda._
import scala.io.Source

object GenerateCode {
  def preludeIr : String = {
    val stream = getClass.getClassLoader.getResourceAsStream("prelude.ll")
    io.Source.fromInputStream(stream).mkString
  }

  def apply(expressions : List[et.Expression]) : String = {
    preludeIr
  }
}

