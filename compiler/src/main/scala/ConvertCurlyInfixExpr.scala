package io.llambda.compiler
import io.llambda

object ConvertCurlyInfixExpr {
  /** Converts a C-expression to an S-expression */
  def apply(data : Seq[ast.Datum]) : ast.Datum = data.toList match {
    case oddList if (oddList.length >= 3) && (oddList.length % 2 == 1) =>
      val (operators, operands) = oddList.zipWithIndex.partition(_._2 % 2 == 1) match {
        case (operatorsWithIndex, operandsWithIndex) =>
          (operatorsWithIndex.map(_._1), operandsWithIndex.map(_._1))
      }

      val operator = operators.reduce { (prev, current) =>
        if (prev != current) {
          throw new InvalidCurlyInfixExprException(
            located=current,
            message=s"Mismatched even data in curly infix expression: ${prev}, ${current}"
          )
        }

        current
      }

      ast.ProperList(operator :: operands)

    case List(unaryOperator, unaryOperand) =>
      ast.ProperList(List(unaryOperator, unaryOperand))

    case List(single) =>
      single

    case Nil =>
      ast.EmptyList()

    case other :: _ =>
      throw new InvalidCurlyInfixExprException(
        located=other,
        message="Invalid curly infix expression"
      )
  }
}
