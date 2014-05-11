package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class NumberProcReducerSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  implicit val scope = schemeBaseScope

  test("predicates") {
    assert(reductionFor("(number? 4)") ===
      et.Literal(ast.BooleanLiteral(true))
    )

    assert(reductionFor("(number? 4.7)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(number? #t)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(integer? 4)") ===
      et.Literal(ast.BooleanLiteral(true))
    )

    assert(reductionFor("(integer? 4.7)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(integer? #t)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }

  test("static (zero?)") {
    assert(reductionFor("(zero? 0)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(zero? 5)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(zero? 0.0)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(zero? -6.1)") ===
      et.Literal(ast.BooleanLiteral(false))
    )

    // Let this fail at runtime
    inside(reductionFor("(zero? 'hello)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.Symbol("hello"))
      )) =>
        Unit
    }
  }

  test("static (+)") {
    assert(reductionFor("(+)") ===
      et.Literal(ast.IntegerLiteral(0))
    )
    
    assert(reductionFor("(+ 56)") ===
      et.Literal(ast.IntegerLiteral(56))
    )
    
    assert(reductionFor("(+ 56 -6)") ===
      et.Literal(ast.IntegerLiteral(50))
    )

    assert(reductionFor("(+ 56 -6 55.5)") ===
      et.Literal(ast.RationalLiteral(105.5))
    )
    
    assert(reductionFor("(+ 56 -6 55.5 -5.5)") ===
      et.Literal(ast.RationalLiteral(100.0))
    )
    
    inside(reductionFor("(+ 56 -6 55.5 -5.5 #t #f)")) {
      case et.Apply(et.VarRef(_), List( 
        et.Literal(ast.RationalLiteral(100.0)),
        et.Literal(ast.BooleanLiteral(true)),
        et.Literal(ast.BooleanLiteral(false))
      )) =>
        Unit
    }
  }
  
  test("static (*)") {
    assert(reductionFor("(*)") ===
      et.Literal(ast.IntegerLiteral(1))
    )
    
    assert(reductionFor("(* 56)") ===
      et.Literal(ast.IntegerLiteral(56))
    )
    
    assert(reductionFor("(* 5 -6)") ===
      et.Literal(ast.IntegerLiteral(-30))
    )

    assert(reductionFor("(* 56 -0.5)") ===
      et.Literal(ast.RationalLiteral(-28.0))
    )
    
    assert(reductionFor("(* -200.0 -0.5)") ===
      et.Literal(ast.RationalLiteral(100.0))
    )
    
    inside(reductionFor("(* 100 0.25 #t #f)")) {
      case et.Apply(et.VarRef(_), List( 
        et.Literal(ast.RationalLiteral(25.0)),
        et.Literal(ast.BooleanLiteral(true)),
        et.Literal(ast.BooleanLiteral(false))
      )) =>
        Unit
    }
  }
  
  test("static (-)") {
    assert(reductionFor("(- 5)") ===
      et.Literal(ast.IntegerLiteral(-5))
    )
    
    assert(reductionFor("(- -14.5)") ===
      et.Literal(ast.RationalLiteral(14.5))
    )
    
    assert(reductionFor("(- 56 -0.5)") ===
      et.Literal(ast.RationalLiteral(56.5))
    )
    
    assert(reductionFor("(- 200 50)") ===
      et.Literal(ast.IntegerLiteral(150))
    )
    
    inside(reductionFor("(- 100 0.25 #t #f)")) {
      case et.Apply(et.VarRef(_), List( 
        et.Literal(ast.RationalLiteral(99.75)),
        et.Literal(ast.BooleanLiteral(true)),
        et.Literal(ast.BooleanLiteral(false))
      )) =>
        Unit
    }
  }
  
  test("static (/)") {
    assert(reductionFor("(/ 5)") ===
      et.Literal(ast.RationalLiteral(0.2))
    )
    
    assert(reductionFor("(/ 10 2.0)") ===
      et.Literal(ast.RationalLiteral(5.0))
    )
    
    assert(reductionFor("(/ -20 0.5)") ===
      et.Literal(ast.RationalLiteral(-40.0))
    )
    
    assert(reductionFor("(/ 200 50)") ===
      et.Literal(ast.RationalLiteral(4.0))
    )
    
    // We should refuse to do this at compile time
    // We can't error out because the code may unreachable
    inside(reductionFor("(/ 0)")) {
      case et.Apply(et.VarRef(_), List( 
        et.Literal(ast.IntegerLiteral(0))
      )) =>
        Unit
    }

    // Same as above with two args
    inside(reductionFor("(/ 200 0)")) {
      case et.Apply(et.VarRef(_), List( 
        et.Literal(ast.IntegerLiteral(200)),
        et.Literal(ast.IntegerLiteral(0))
      )) =>
        Unit
    }
    
    // Unlike the other math operators we don't do partial reduction at the moment
    inside(reductionFor("(/ 100 0.25 #t #f)")) {
      case et.Apply(et.VarRef(_), List( 
        et.Literal(ast.IntegerLiteral(100)),
        et.Literal(ast.RationalLiteral(0.25)),
        et.Literal(ast.BooleanLiteral(true)),
        et.Literal(ast.BooleanLiteral(false))
      )) =>
        Unit
    }
  }
  
  test("static (=)") {
    // Let this fail at runtime
    inside(reductionFor("(= 40)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.IntegerLiteral(40))
      )) =>
        Unit
    }

    assert(reductionFor("(= 50 50)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(= 50 200)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(= 50 500 200)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(= -125.5 -125.5)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(= -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(= -125.5 -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(= +nan.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(= 0.0 0.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("static (<)") {
    // Let this fail at runtime
    inside(reductionFor("(< 40)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.IntegerLiteral(40))
      )) =>
        Unit
    }

    assert(reductionFor("(< 50 50)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(< 50 200)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(< 50 500 200)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(< -125.5 -125.5)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(< -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(< -125.5 -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(< +nan.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(< 0.0 0.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("static (<=)") {
    // Let this fail at runtime
    inside(reductionFor("(<= 40)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.IntegerLiteral(40))
      )) =>
        Unit
    }

    assert(reductionFor("(<= 50 50)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(<= 50 200)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(<= 50 500 200)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(<= -125.5 -125.5)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(<= -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(<= -125.5 -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(<= +nan.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(<= 0.0 0.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("static (>)") {
    // Let this fail at runtime
    inside(reductionFor("(> 40)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.IntegerLiteral(40))
      )) =>
        Unit
    }

    assert(reductionFor("(> 50 50)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(> 200 50)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(> 50 500 200)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(> -125.5 -125.5)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(> -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(> -70.0 -125.5)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(> -125.5 -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(> +nan.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(> 0.0 0.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
  
  test("static (>=)") {
    // Let this fail at runtime
    inside(reductionFor("(>= 40)")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.IntegerLiteral(40))
      )) =>
        Unit
    }

    assert(reductionFor("(>= 50 50)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(>= 200 50)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(>= 50 500 200)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(>= -125.5 -125.5)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(>= -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(>= -70.0 -125.5)") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("(>= -125.5 -125.5 -70.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(>= +nan.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
    
    assert(reductionFor("(>= 0.0 0.0 +nan.0)") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }
}
