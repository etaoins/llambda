package llambda.codegen

import llambda.{ast, nfi}

object GenLiteral {
  def apply(module : llvmir.IrModuleBuilder)(datum : ast.Datum) : ConstantLiveValue = {
    datum match {
      case ast.StringLiteral(content) =>
        LiveString.fromConstant(module)(content)
      
      case ast.Symbol(content) =>
        LiveSymbol.fromConstant(module)(content)

      case ast.IntegerLiteral(value) =>
        LiveExactInteger.fromConstant(module)(value)
      
      case ast.RationalLiteral(value) =>
        LiveInexactRational.fromConstant(module)(value)
      
      case ast.BooleanLiteral(value) =>
        LiveBoolean.fromConstant(module)(value)
      
      case ast.CharLiteral(value) =>
        LiveCharacter.fromConstant(module)(value)
      
      case ast.Pair(car, cdr) =>
        LivePair.fromConstant(module)(GenLiteral(module)(car), GenLiteral(module)(cdr))
      
      case ast.VectorLiteral(elements) =>
        LiveVector.fromConstant(module)(elements.map(GenLiteral(module)_))
      
      case ast.Bytevector(elements) =>
        LiveBytevector.fromConstant(module)(elements)

      case ast.UnspecificValue =>
        LiveUnspecific
      
      case ast.EmptyList =>
        LiveEmptyList
    }
  }
}
