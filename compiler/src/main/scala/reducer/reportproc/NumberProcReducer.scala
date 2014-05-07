package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object NumberProcReducer extends ReportProcReducer {
  private abstract class MathOperandProcessor(unitValue : Long, initialExactValue : Long, initialInexactValueOpt : Option[Double] = None) {
    def accumulateExact(acc : Long, newValue : Long) : Long
    def accumulateInexact(acc : Double, newValue : Double) : Double
    def reduceExactAndInexact(exact : Long, inexact : Double) : Double

    def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : et.Expression = {
      case class MathAcc(
        exactValue : Long,
        inexactValueOpt : Option[Double] = initialInexactValueOpt,
        nonLiterals : List[et.Expression] = Nil
      )

      val initialAcc = MathAcc(
        exactValue=initialExactValue,
        inexactValueOpt=initialInexactValueOpt
      )

      val finalAcc = operands.foldLeft(initialAcc) { case (acc, operandExpr) =>
        LiteralValue(operandExpr) match {
          case Some(ast.IntegerLiteral(exactValue)) =>
            acc.copy(
              exactValue=accumulateExact(acc.exactValue, exactValue)
            )
          
          case Some(ast.RationalLiteral(inexactValue)) =>
            val newInexactValue = accumulateInexact(acc.inexactValueOpt.getOrElse(unitValue), inexactValue)
            acc.copy(
              inexactValueOpt=Some(newInexactValue)
            )

          case _ =>
            acc.copy(
              nonLiterals=acc.nonLiterals :+ operandExpr
            )
        }
      }

      val constantValueOpt = finalAcc match {
        case MathAcc(exactValue, None, _) if exactValue != unitValue =>
          Some(ast.IntegerLiteral(exactValue))

        case MathAcc(exactValue, Some(inexactValue), _) =>
          val finalValue = reduceExactAndInexact(exactValue, inexactValue)
          Some(ast.RationalLiteral(finalValue))

        case _ =>
          None
      }

      if (finalAcc.nonLiterals.isEmpty) {
        // We reduce down to a constant value
        et.Literal(
          constantValueOpt.getOrElse(ast.IntegerLiteral(unitValue))
        )
      }
      else {
        // We partially reduced but still have non-literal operands
        et.Apply(
          et.VarRef(appliedVar),
          constantValueOpt.map(et.Literal(_)).toList ++ finalAcc.nonLiterals
        )
      }
    }
  }

  abstract class ComparisonProcessor {
    def compareExact(left : Long, right : Long) : Boolean
    def compareInexact(left : Double, right : Double) : Boolean
    
    def apply(operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = {
      val literalOperands = operands.map(LiteralValue(_))

      val exactOperandValues = literalOperands.collect { 
        case Some(ast.IntegerLiteral(exactValue)) =>
          exactValue
      }

      if (exactOperandValues.length == literalOperands.length) {
        // We have all exact operands
        Some(et.Literal(ast.BooleanLiteral(
          exactOperandValues.sliding(2).forall { case List(left, right) =>
            compareExact(left, right)
          }
        )))
      }
      else {
        val inexactOperandValues = literalOperands.collect { 
          case Some(ast.RationalLiteral(inexactValue)) =>
            inexactValue
        }

        if (inexactOperandValues.length == literalOperands.length) {
          // We have all inexact operands
          Some(et.Literal(ast.BooleanLiteral(
            inexactOperandValues.sliding(2).forall { case List(left, right) =>
              compareInexact(left, right)
            }
          )))
        }
        else {
          // Don't handle non-literals or mixed numerics yet
          None
        }
      }
    }
  }

  private def numericValueOfExpr(expr : et.Expression)(implicit reduceConfig : ReduceConfig) : Option[Double] = {
    LiteralValue(expr) match {
      case Some(ast.IntegerLiteral(exactValue)) =>
        Some(exactValue.toLong)

      case Some(ast.RationalLiteral(inexactValue)) =>
        Some(inexactValue)

      case _ =>
        None
    }
  }

  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case ("number?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.IntegerLiteral] || literal.isInstanceOf[ast.RationalLiteral]
      })
    
    case ("integer?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.IntegerLiteral]
      })
    
    case ("zero?", List(singleExpr)) =>
      LiteralValue(singleExpr) match {
        case Some(ast.IntegerLiteral(exactValue)) =>
          Some(et.Literal(
            ast.BooleanLiteral(exactValue == 0)
          ))
        
        case Some(ast.RationalLiteral(inexactValue)) =>
          Some(et.Literal(
            ast.BooleanLiteral(inexactValue == 0.0)
          ))

        case _ =>
          None
      }
    
    case ("+", _) =>
      Some((new MathOperandProcessor(0, 0) {
        def accumulateExact(acc : Long, newValue : Long) =
          acc + newValue
        
        def accumulateInexact(acc : Double, newValue : Double) =
          acc + newValue
        
        def reduceExactAndInexact(val1 : Long, val2 : Double) =
          val1 + val2
      })(appliedVar, operands))
    
    case ("*", _) =>
      Some((new MathOperandProcessor(1, 1) {
        def accumulateExact(acc : Long, newValue : Long) =
          acc * newValue
        
        def accumulateInexact(acc : Double, newValue : Double) =
          acc * newValue
        
        def reduceExactAndInexact(val1 : Long, val2 : Double) =
          val1 * val2
      })(appliedVar, operands))
    
    case ("-", Nil) =>
      None

    case ("-", List(singleOperand)) =>
      LiteralValue(singleOperand) match {
        case Some(ast.IntegerLiteral(exactValue)) =>
          Some(et.Literal(ast.IntegerLiteral(-exactValue)))
        
        case Some(ast.RationalLiteral(inexactValue)) =>
          Some(et.Literal(ast.RationalLiteral(-inexactValue)))

        case _ =>
          None
      }
    
    case ("-", startValue :: restOperands) =>
      class SubtractOperandProcessor(initialExactValue : Long, initialInexactValueOpt : Option[Double]) extends MathOperandProcessor(0, initialExactValue, initialInexactValueOpt) {
        def accumulateExact(acc : Long, newValue : Long) =
          acc - newValue
        
        def accumulateInexact(acc : Double, newValue : Double) =
          acc - newValue
        
        def reduceExactAndInexact(val1 : Long, val2 : Double) =
          val1 + val2
      }
      
      LiteralValue(startValue) match {
        case Some(ast.IntegerLiteral(exactValue)) =>
          Some((new SubtractOperandProcessor(exactValue, None))(appliedVar, restOperands))

        case Some(ast.RationalLiteral(inexactValue)) =>
          Some((new SubtractOperandProcessor(0, Some(inexactValue)))(appliedVar, restOperands))

        case _ =>
          None
      }
    
    case ("/", Nil) =>
      None

    case ("/", List(singleOperand)) =>
      numericValueOfExpr(singleOperand) match {
        case Some(inexactValue) if inexactValue != 0.0 =>
          Some(et.Literal(ast.RationalLiteral(1.0 / inexactValue)))

        case _ =>
          None
      }
    
    case ("/", startValue :: restOperands) =>
      // We'll throw this if we encounter something weird
      class UnhandledDivisionException extends Exception

      val startNumericOpt = numericValueOfExpr(startValue)

      try {
        val finalNumericValueOpt = startNumericOpt.map { startNumericValue =>
          restOperands.foldLeft(startNumericValue) { case (accValue, operandExpr) =>
            numericValueOfExpr(operandExpr) match {
              case Some(inexactValue) if inexactValue != 0.0 =>
                accValue / inexactValue

              case _ =>
                throw new UnhandledDivisionException
            }
          }
        }

        finalNumericValueOpt.map { finalNumericValue =>
          et.Literal(ast.RationalLiteral(finalNumericValue))
        }
      }
      catch {
        case _ : UnhandledDivisionException =>
          None
      }
    
    case ("=", _) if operands.length >= 2 =>
      (new ComparisonProcessor {
        def compareExact(left : Long, right : Long) =
          left == right

        def compareInexact(left : Double, right : Double) =
          left == right
      })(operands)

    case ("<", _) if operands.length >= 2 =>
      (new ComparisonProcessor {
        def compareExact(left : Long, right : Long) =
          left < right

        def compareInexact(left : Double, right : Double) =
          left < right
      })(operands)
    
    case ("<=", _) if operands.length >= 2 =>
      (new ComparisonProcessor {
        def compareExact(left : Long, right : Long) =
          left <= right

        def compareInexact(left : Double, right : Double) =
          left <= right
      })(operands)

    case (">", _) if operands.length >= 2 =>
      (new ComparisonProcessor {
        def compareExact(left : Long, right : Long) =
          left > right

        def compareInexact(left : Double, right : Double) =
          left > right
      })(operands)
    
    case (">=", _) if operands.length >= 2 =>
      (new ComparisonProcessor {
        def compareExact(left : Long, right : Long) =
          left >= right

        def compareInexact(left : Double, right : Double) =
          left >= right
      })(operands)

    case _ =>
      None
  }
}
