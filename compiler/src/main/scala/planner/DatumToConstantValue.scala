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

      case ast.RationalLiteral(value) =>
        new iv.ConstantInexactRationalValue(value)

      case ast.BooleanLiteral(value) =>
        new iv.ConstantBooleanValue(value)
      
      case ast.CharLiteral(value) =>
        new iv.ConstantCharacterValue(value)
      
      case ast.Pair(car, cdr) =>
        // Recurse down the car and cdr
        val carConstant = apply(car)
        val cdrConstant = apply(cdr)

        val listMetricsOpt = cdrConstant match {
          case pairValue : iv.ConstantPairValue =>
            pairValue.listMetricsOpt.map { cdrListMetrics =>
              // Only keep our member type hint if we have the same type as the tail of the list
              val newMemberType = cdrListMetrics.memberType.filter(_ == carConstant.cellType)

              iv.ConstantListMetrics(
                length=cdrListMetrics.length + 1,
                memberType=newMemberType
              )
            }

          case iv.EmptyListValue =>
            Some(
              iv.ConstantListMetrics(
                length=1,
                memberType=Some(carConstant.cellType)
              )
            )

          case _ =>
            // Not a proper list
            None
        }

        new iv.ConstantPairValue(carConstant, cdrConstant, listMetricsOpt)

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

