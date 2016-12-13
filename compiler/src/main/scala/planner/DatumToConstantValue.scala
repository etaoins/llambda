package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.{intermediatevalue => iv}

object DatumToConstantValue {
  def apply(datum: ast.Datum): iv.ConstantValue = {
    datum match {
      case ast.StringLiteral(content) =>
        iv.ConstantStringValue(content)

      case ast.Symbol(content) =>
        iv.ConstantSymbolValue(content)

      case ast.IntegerLiteral(value) =>
        iv.ConstantIntegerValue(value)

      case ast.FlonumLiteral(value) =>
        iv.ConstantFlonumValue(value)

      case ast.BooleanLiteral(value) =>
        iv.ConstantBooleanValue(value)

      case ast.CharLiteral(value) =>
        iv.ConstantCharValue(value)

      case ast.Pair(car, cdr) =>
        // Recurse down the car and cdr
        iv.ConstantPairValue(apply(car), apply(cdr))

      case ast.VectorLiteral(elements) =>
        iv.ConstantVectorValue(elements.map(apply))

      case ast.Bytevector(elements) =>
        iv.ConstantBytevectorValue(elements)

      case ast.UnitValue() =>
        iv.UnitValue

      case ast.EmptyList() =>
        iv.EmptyListValue
    }
  }
}

