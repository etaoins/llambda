package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.{intermediatevalue => iv}

object DatumToConstantValue {
  def apply(datum : ast.Datum) : iv.ConstantValue = {
    datum match {
      case ast.StringLiteral(content) =>
        new iv.ConstantStringValue(content)

      case ast.Symbol(content) =>
        new iv.ConstantSymbolValue(content)

      case ast.IntegerLiteral(value) =>
        new iv.ConstantExactIntegerValue(value)

      case ast.FlonumLiteral(value) =>
        new iv.ConstantFlonumValue(value)

      case ast.BooleanLiteral(value) =>
        new iv.ConstantBooleanValue(value)
      
      case ast.CharLiteral(value) =>
        new iv.ConstantCharValue(value)
      
      case ast.Pair(car, cdr) =>
        // Recurse down the car and cdr
        new iv.ConstantPairValue(apply(car), apply(cdr))

      case ast.VectorLiteral(elements) =>
        new iv.ConstantVectorValue(elements.map(apply))
      
      case ast.Bytevector(elements) =>
        new iv.ConstantBytevectorValue(elements)

      case ast.UnitValue() =>
        iv.UnitValue
      
      case ast.EmptyList() =>
        iv.EmptyListValue
    }
  }
}

