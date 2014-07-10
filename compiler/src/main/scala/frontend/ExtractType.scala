package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler._

object ExtractType {
  private def resolveTypeConstructor(scopedSymbol : sst.ScopedSymbol) : PrimitiveTypeConstructor = scopedSymbol.resolve match {
    case typeConstructor : PrimitiveTypeConstructor =>
      typeConstructor

    case _ =>
      throw new MalformedExprException(scopedSymbol, "Syntax cannot be used as an expression")
  }

  private def applyTypeConstructor(constructorName : sst.ScopedSymbol, operands : List[sst.ScopedDatum]) = {
    resolveTypeConstructor(constructorName) match {
      case Primitives.TypeUnion =>
        val schemeTypeOperands = operands.map(extractSchemeType)
        vt.SchemeType.fromTypeUnion(schemeTypeOperands)

      case _ =>
        throw new BadSpecialFormException(constructorName, "Invalid type constructor syntax")
    }
  }

  def extractSchemeType(datum : sst.ScopedDatum) : vt.SchemeType = extractValueType(datum) match {
    case schemeType : vt.SchemeType =>
      schemeType

    case nonCellValue =>
      throw new BadSpecialFormException(datum, "Native type used where Scheme type expected")
  }

  def extractValueType(datum : sst.ScopedDatum) : vt.ValueType = datum match { 
    case symbol : sst.ScopedSymbol =>
      symbol.resolve match {
        case BoundType(schemeType) => schemeType

        case _ =>
          throw new BadSpecialFormException(symbol, "Non-type value used as type")
      }
    
    case sst.ScopedProperList((constructorName : sst.ScopedSymbol) :: operandData) =>
      applyTypeConstructor(constructorName, operandData)

    case sst.NonSymbolLeaf(ast.BooleanLiteral(value)) =>
      vt.ConstantBooleanType(value)

    case nonsymbol => 
      throw new BadSpecialFormException(nonsymbol, "Excepted type name to be symbol or type constructor application")
  }
}
