package io.llambda.compiler.reducer.reportproc
import io.llambda

import io.llambda.compiler._
import io.llambda.compiler.reducer._
import io.llambda.compiler.reducer.{partialvalue => pv}

import annotation.tailrec

object ListProcReducer extends ReportProcReducer {
  @tailrec
  private def findMember(needle : ast.Datum, listHead : ast.Datum, comparator : (ast.Datum, ast.Datum) => Option[Boolean]) : Option[et.Literal] = listHead match {
    case ast.Pair(car, cdr) =>
      comparator(needle, car) match {
        case Some(true) =>
          // Definitely found it - return!
          Some(et.Literal(listHead))

        case None =>
          // Can't compare - terminate with an indefinite result
          None

        case Some(false) =>
          // Didn't match - continue
          findMember(needle, cdr, comparator)
      }

    case ast.EmptyList() =>
      // Did not find it
      Some(et.Literal(ast.BooleanLiteral(false)))

    case _ =>
      // Not a proper list
      // Let this fail at runtime
      None
  }

  def apply(appliedVar : ReportProcedure, operands : List[et.Expression])(implicit reduceConfig : ReduceConfig) : Option[et.Expression] = (appliedVar.reportName, operands) match {
    case ("null?", List(singleExpr)) =>
      literalPredicate(singleExpr, { literal =>
        literal.isInstanceOf[ast.EmptyList]
      })
    
    case ("pair?", List(singleExpr)) =>
      PartialValueForExpression(singleExpr).flatMap {
        case _ : pv.PartialPair => 
          Some(et.Literal(ast.BooleanLiteral(true)))

        case _ : pv.PartialVector => 
          Some(et.Literal(ast.BooleanLiteral(false)))
        
        case _ : pv.LiteralLeaf => 
          Some(et.Literal(ast.BooleanLiteral(false)))

        case _ : pv.ReducedExpression =>
          // This could be anything
          None
      }
    
    case ("length", List(singleExpr)) =>
      PartialValueForExpression(singleExpr) flatMap {
        case pv.ProperList(elements) =>
          Some(et.Literal(ast.IntegerLiteral(elements.length)))

        case _ =>
          None
      }
    
    // Make sure we only include literal lists and don't deref variables
    // Otherwise we could duplicate the tail of the list which would make them falsely not eqv?
    case (reportName, List(needle, et.Literal(listHead))) if List("memq", "memv").contains(reportName) =>
      LiteralForExpression(needle) flatMap { datum =>
        findMember(datum, listHead, LiteralEqv.literalsAreEqv)
      }
    
    case ("member", List(needle, et.Literal(listHead))) =>
      LiteralForExpression(needle) flatMap { datum =>
        findMember(datum, listHead, LiteralEqv.literalsAreEqual)
      }
    
    case _ => 
      None
  }
}
