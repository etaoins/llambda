package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class BooleanProcReducerSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  implicit val scope = schemeBaseScope

  test("type predicates") {
    assert(reductionFor("(boolean? '())") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(boolean? #t)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
  }  
  
  test("static (not)") {
    assert(reductionFor("(not #t)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(not (not #t))") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(not 'truthy)") === 
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(not #f)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
  }

  test("static (boolean=?)") {
    assert(reductionFor("(boolean=? #t #t)") === 
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(boolean=? #f #f)") === 
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(boolean=? #t #f)") === 
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(boolean=? #t #t #f)") === 
      et.Literal(ast.BooleanLiteral(false))
    )

    inside(reductionFor("(boolean=? #t 'symbol)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.BooleanLiteral(true)),
        et.Literal(ast.Symbol("symbol"))
      )) =>
        Unit
    }
  }
}
