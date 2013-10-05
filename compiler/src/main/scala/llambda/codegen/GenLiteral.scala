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
      
      case ast.Symbol(content) =>
        LiveSymbol(
          constantValue=Some(content),
          boxedValue=None,
          utf8Pointer=None)

      case ast.IntegerLiteral(value) =>
        LiveExactInteger(
          constantValue=Some(value),
          boxedValue=None)
      
      case ast.RationalLiteral(value) =>
        LiveInexactRational(
          constantValue=Some(value),
          boxedValue=None)
      
      case ast.BooleanLiteral(value) =>
        LiveBoolean(
          constantValue=Some(value),
          boxedValue=None)
      
      case ast.CharLiteral(value) =>
        LiveCharacter(
          constantValue=Some(value),
          boxedValue=None)
      
      case ast.Pair(car, cdr) =>
        LivePair(
          constantValue=Some((GenLiteral(car), GenLiteral(cdr))),
          boxedValue=None)
      
      case ast.VectorLiteral(elements) =>
        LiveVector(
          constantValue=Some(elements.map(GenLiteral.apply)),
          boxedValue=None)
      
      case ast.Bytevector(elements) =>
        LiveBytevector(
          constantValue=Some(elements),
          boxedValue=None)

      case ast.UnspecificValue =>
        LiveUnspecific
      
      case ast.EmptyList =>
        LiveEmptyList
    }
  }
}
