package llambda.codegen

import llambda.{ast, nfi}

object GenLiteral {
  def apply(datum : ast.Datum) : ConstantLiveValue = {
    datum match {
      case ast.StringLiteral(content) =>
        LiveString.fromConstant(content)
      
      case ast.Symbol(content) =>
        LiveSymbol.fromConstant(content)

      case ast.IntegerLiteral(value) =>
        LiveExactInteger.fromConstant(value)
      
      case ast.RationalLiteral(value) =>
        LiveInexactRational.fromConstant(value)
      
      case ast.BooleanLiteral(value) =>
        LiveBoolean.fromConstant(value)
      
      case ast.CharLiteral(value) =>
        LiveCharacter.fromConstant(value)
      
      case ast.Pair(car, cdr) =>
        LivePair.fromConstant(GenLiteral(car), GenLiteral(cdr))
      
      case ast.VectorLiteral(elements) =>
        LiveVector.fromConstant(elements.map(GenLiteral.apply))
      
      case ast.Bytevector(elements) =>
        LiveBytevector.fromConstant(elements)

      case ast.UnspecificValue =>
        LiveUnspecific
      
      case ast.EmptyList =>
        LiveEmptyList
    }
  }
}
