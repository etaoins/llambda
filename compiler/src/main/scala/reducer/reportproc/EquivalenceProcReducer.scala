package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._

object EquivalenceProcReducer extends ReportProcReducer {
  private def literalsAreEqv(val1 : ast.Datum, val2 : ast.Datum) : Option[Boolean] = val1 match {
    case (_ : ast.BooleanLiteral | _ : ast.IntegerLiteral | _ : ast.RationalLiteral | _ : ast.StringLiteral |
          _ : ast.Symbol | _ : ast.StringLiteral | _ : ast.CharLiteral | _ : ast.UnitValue | _ : ast.EmptyList) =>
      Some(val1 == val2)

    case _ : ast.Pair =>
      if (val2.isInstanceOf[ast.Pair]) {
        // Could be - we don't know if codegen will place these in the same storage location
        None
      }
      else {
        Some(false)
      }
    
    case _ : ast.VectorLiteral =>
      if (val2.isInstanceOf[ast.VectorLiteral]) {
        None
      }
      else {
        Some(false)
      }
    
    case _ : ast.Bytevector =>
      if (val2.isInstanceOf[ast.Bytevector]) {
        None
      }
      else {
        Some(false)
      }
  }

  private def elementsAreEqual(elems1 : Seq[ast.Datum], elems2 : Seq[ast.Datum]) : Option[Boolean] = {
    if (elems1.length != elems2.length) {
      // Nope
      return Some(false)
    }

    val equalityResult = elems1.zip(elems2).map { case (elem1, elem2) =>
      literalsAreEqual(elem1, elem2)
    }

    if (equalityResult.contains(Some(false))) {
      // Definitely inequal
      Some(false)
    }
    else if (equalityResult.forall(_ == Some(true))) {
      // Definitely equal
      Some(true)
    }
    else {
      // Unknown
      None
    }
  }

  private def literalsAreEqual(val1 : ast.Datum, val2 : ast.Datum) : Option[Boolean] = (val1, val2) match {
    case (ast.Pair(car1, cdr1), ast.Pair(car2, cdr2)) =>
      elementsAreEqual(List(car1, cdr1), List(car2, cdr2))

    case (ast.VectorLiteral(elems1), ast.VectorLiteral(elems2)) =>
      elementsAreEqual(elems1, elems2)
    
    case (ast.Bytevector(elems1), ast.Bytevector(elems2)) =>
      Some(elems1 == elems2)

    case _ =>
      literalsAreEqv(val1, val2)
  }

  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case (reportName, List(val1Expr, val2Expr)) if List("eqv?", "eq?").contains(reportName) =>
      for(val1 <- LiteralForExpression(val1Expr);
          val2 <- LiteralForExpression(val2Expr)) {
        return literalsAreEqv(val1, val2).map { isEqv =>
          et.Literal(ast.BooleanLiteral(isEqv))
        }
      }

      None
    
    case ("equal?", List(val1Expr, val2Expr)) =>
      for(val1 <- LiteralForExpression(val1Expr);
          val2 <- LiteralForExpression(val2Expr)) {
        return literalsAreEqual(val1, val2).map { isEqv =>
          et.Literal(ast.BooleanLiteral(isEqv))
        }
      }

      None

    case _ =>
      None
  }
}
