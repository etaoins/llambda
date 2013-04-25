package test.scala

import org.scalatest.{FunSuite,Inside}
import llambda._

class PrimitiveExpressionsSuite extends FunSuite with Inside {
  implicit val primitiveScope = new Scope(SchemePrimitives.bindings)
  
  private def expressionFor(scheme : String)(implicit scope : Scope)  = {
    SchemeParser(scheme) match {
      case SchemeParser.Success(datum :: Nil, _) =>
        ExtractExpressions(datum)(scope)
      case err =>
        fail(err.toString)
    }
  }

  test("variable reference") {
    // "a" isn't a binding in the primitive expressions
    intercept[UnboundVariableException] {
      expressionFor("a")
    }

    assert(expressionFor("set!") === et.VarReference(SchemePrimitives.Set))
  }

  test("literals") {
    assert(expressionFor("'a") === et.Literal(ast.Symbol("a")))

    assert(expressionFor("'#(a b c)") === et.Literal(
      ast.Vector(ast.Symbol("a"), ast.Symbol("b"), ast.Symbol("c"))
    ))
    
    assert(expressionFor("'()") === et.Literal(ast.EmptyList))
    
    assert(expressionFor("'(+ 1 2)") === et.Literal(
      ast.ProperList(ast.Symbol("+"), ast.IntegerLiteral(1), ast.IntegerLiteral(2))
    ))
    
    assert(expressionFor("'(quote a)") === et.Literal(
      ast.ProperList(ast.Symbol("quote"), ast.Symbol("a"))
    ))
    
    assert(expressionFor("''a") === et.Literal(
      ast.ProperList(ast.Symbol("quote"), ast.Symbol("a"))
    ))

    assert(expressionFor("'145932") === et.Literal(
      ast.IntegerLiteral(145932)
    ))
    
    assert(expressionFor("145932") === et.Literal(
      ast.IntegerLiteral(145932)
    ))
    
    assert(expressionFor("'\"" + "abc" + "\"") === et.Literal(
      ast.StringLiteral("abc")
    ))
    
    assert(expressionFor("\"" + "abc" + "\"") === et.Literal(
      ast.StringLiteral("abc")
    ))
    
    assert(expressionFor("'#(a 10)") === et.Literal(
      ast.Vector(ast.Symbol("a"), ast.IntegerLiteral(10))
    ))
    
    assert(expressionFor("#(a 10)") === et.Literal(
      ast.Vector(ast.Symbol("a"), ast.IntegerLiteral(10))
    ))
    
    assert(expressionFor("'#u8(64 65)") === et.Literal(
      ast.ByteVector(64, 65)
    ))
    
    assert(expressionFor("#u8(64 65)") === et.Literal(
      ast.ByteVector(64, 65)
    ))
    
    assert(expressionFor("'#t") === et.Literal(
      ast.TrueLiteral
    ))
    
    assert(expressionFor("#t") === et.Literal(
      ast.TrueLiteral
    ))
  }

  test("application") {
    // Make a storage location for +
    val plusLoc = new StorageLocation
    val plusScope = new Scope(Map("+" -> plusLoc), Some(primitiveScope))

    assert(expressionFor("(+ 3 4)")(plusScope) === et.ProcedureCall(
      plusLoc,
      List(
        et.Literal(ast.IntegerLiteral(3)),
        et.Literal(ast.IntegerLiteral(4))
      )
    ))

    intercept[UnboundVariableException] {
      expressionFor("(/ 1 2)")
    }

    // R7RS explicitly calls this an error
    intercept[MalformedExpressionException] {
      expressionFor("()")
    }
  }

  test("set!") {
    // Make a storage location for a
    val storageLoc = new StorageLocation
    val varScope = new Scope(Map("a" -> storageLoc), Some(primitiveScope))

    assert(expressionFor("(set! a 1)")(varScope) === et.SetVar(
      storageLoc,
      et.Literal(ast.IntegerLiteral(1))
    ))

    intercept[UnboundVariableException] {
      expressionFor("(set! b 1)")
    }
  }

  /*
  test("lambdas") {
    assert(expressionFor("(lambda (x) (+ x x))") === et.Procedure(
      List(et.UnresolvedVar("x")), None,
      List(
        et.ProcedureCall(
          et.UnresolvedVar("+"), 
          List(
            et.VarReference(et.UnresolvedVar("x")),
            et.VarReference(et.UnresolvedVar("x"))
          )
        )
      )
    ))

    assert(expressionFor("(lambda x x)") === et.Procedure(
      Nil, Some(et.UnresolvedVar("x")),
      List(et.VarReference(et.UnresolvedVar("x")))
    ))

    assert(expressionFor("(lambda (x y . z) z)") === et.Procedure(
      List(et.UnresolvedVar("x"), et.UnresolvedVar("y")), Some(et.UnresolvedVar("z")),
      List(et.VarReference(et.UnresolvedVar("z")))
    ))
  }*/

  test("conditionals") {
    assert(expressionFor("(if #t 'yes 'no)") === et.Conditional(
      et.Literal(ast.TrueLiteral),
      et.Literal(ast.Symbol("yes")),
      Some(et.Literal(ast.Symbol("no")))
    ))
    
    assert(expressionFor("(if #f 'yes)") === et.Conditional(
      et.Literal(ast.FalseLiteral),
      et.Literal(ast.Symbol("yes")),
      None
    ))
  }
}
