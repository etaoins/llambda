package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.{intermediatevalue => iv}

object DatumToConstantValue {
  def apply(datum: ast.Datum): iv.ConstantValue = {
    datum match {
      case ast.String(content) =>      iv.ConstantStringValue(content)
      case ast.Symbol(content) =>      iv.ConstantSymbolValue(content)
      case ast.Integer(value) =>       iv.ConstantIntegerValue(value)
      case ast.Flonum(value) =>        iv.ConstantFlonumValue(value)
      case ast.Boolean(value) =>       iv.ConstantBooleanValue(value)
      case ast.Char(value) =>          iv.ConstantCharValue(value)
      case ast.Pair(car, cdr) =>       iv.ConstantPairValue(apply(car), apply(cdr))
      case ast.Vector(elements) =>     iv.ConstantVectorValue(elements.map(apply))
      case ast.Bytevector(elements) => iv.ConstantBytevectorValue(elements)
      case ast.Unit() =>               iv.UnitValue
      case ast.EmptyList() =>          iv.EmptyListValue
    }
  }
}

