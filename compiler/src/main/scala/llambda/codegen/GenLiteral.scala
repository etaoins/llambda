package llambda.codegen

import llambda.{ast, nfi}

object GenLiteral {
  def apply(datum : ast.Datum) : LiveValue = {
    datum match {
      case ast.StringLiteral(content) =>
        LiveString(
          constantValue=Some(content),
          boxedValue=None,
          utf8Pointer=None)
    }
  }
}
