package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class SymbolProcReducerSuite extends FunSuite with Inside with testutil.ExprHelpers {
  implicit val scope = schemeBaseScope

  test("type predicates") {
    assert(reductionFor("(symbol? 'hello)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(symbol? #f)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("static (symbol=?)") {
    assert(reductionFor("(symbol=? 'one 'one)") === 
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(symbol=? 'one 'two)") === 
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(symbol=? 'one 'one 'two)") === 
      et.Literal(ast.BooleanLiteral(false))
    )

    inside(reductionFor("(symbol=? 'symbol #t)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.Symbol("symbol")),
        et.Literal(ast.BooleanLiteral(true))
      )) =>
        Unit
    }
  }
}
