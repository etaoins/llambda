package llambda.optimizer

import llambda._
import org.scalatest.FunSuite

class IsPureExpressionSuite extends FunSuite with testutil.ExpressionHelpers {
  val pureExpr = et.Literal(ast.StringLiteral("PURE"))

  val impureExpr = {
    et.Apply(
      et.NativeFunction(
        fixedArgs=Nil,
        hasRestArg=false,
        returnType=None,
        nativeSymbol="impure"
      ),
      Nil
    )
  }

  // Test lambda that returns a constant
  val pureLambda = {
    val arg = new StorageLocation("arg")
    et.Lambda(
      fixedArgs=List(arg),
      restArg=None,
      expressions=List(pureExpr)
    )
  }
  
  // Test lambda that does impure things
  val impureLambda = {
    val arg = new StorageLocation("arg")

    et.Lambda(
      fixedArgs=List(arg),
      restArg=None,
      expressions=List(impureExpr)
    )
  }

  test("literals are pure") {
    val expression = et.Literal(ast.StringLiteral("Hello, world!"))
    assert(IsPureExpression(expression) === true)
  }

  test("lambda expression for a pure lambda is pure") {
    assert(IsPureExpression(pureLambda) === true)
  }
  
  test("lambda expression for an impure lambda is pure") {
    assert(IsPureExpression(impureLambda) === true)
  }

  test("native function expressions are pure") {
    val expression = {
      et.NativeFunction(
        fixedArgs=Nil,
        hasRestArg=false,
        returnType=None,
        nativeSymbol="impure"
      )
    }

    assert(IsPureExpression(expression) === true)
  }

  test("native function applications are impure") {
    val expression = {
      et.Apply(
        et.NativeFunction(
          fixedArgs=Nil,
          hasRestArg=false,
          returnType=None,
          nativeSymbol="impure"
        ),
        Nil
      )
    }

    assert(IsPureExpression(expression) === false)
  }

  test("applying a pure lambda with a pure arg is pure") {
    val expression = et.Apply(pureLambda, List(pureExpr))
    assert(IsPureExpression(expression) === true)
  }

  test("applying a pure lambda with an impure arg is impure") {
    val expression = et.Apply(pureLambda, List(impureExpr))
    assert(IsPureExpression(expression) === false)
  }
  
  test("applying a impure lambda with an pure arg is impure") {
    val expression = et.Apply(impureLambda, List(pureExpr))
    assert(IsPureExpression(expression) === false)
  }
  
  test("applying a impure lambda with an impure arg is impure") {
    val expression = et.Apply(impureLambda, List(impureExpr))
    assert(IsPureExpression(expression) === false)
  }

  test("applying a variable reference is impure") {
    val storageLoc = new StorageLocation("test")

    val expression = {
      et.Apply(
        et.VarRef(storageLoc),
        Nil)
    }

    assert(IsPureExpression(expression) === false)
  }

  test("cond with pure test and pure expressions is pure") {
    val expression = et.Cond(pureExpr, pureExpr, pureExpr)
    assert(IsPureExpression(expression) === true)
  }
  
  test("cond with impure test and pure expressions is impure") {
    val expression = et.Cond(impureExpr, pureExpr, pureExpr)
    assert(IsPureExpression(expression) === false)
  }
  
  test("cond with pure test and impure true expression is impure") {
    val expression = et.Cond(pureExpr, impureExpr, pureExpr)
    assert(IsPureExpression(expression) === false)
  }
  
  test("cond with pure test and impure false expression is impure") {
    val expression = et.Cond(pureExpr, pureExpr, impureExpr)
    assert(IsPureExpression(expression) === false)
  }

  test("variable references are impure") {
    val storageLoc = new StorageLocation("test")
    val expression = et.VarRef(storageLoc)

    assert(IsPureExpression(expression) === false)
  }

  test("variable mutations are impure") {
    val storageLoc = new StorageLocation("test")
    val expression = et.MutateVar(storageLoc, pureExpr)
    
    assert(IsPureExpression(expression) === false)
  }

  test("letting pure bindings to a pure expression is pure") {
    val storageLoc = new StorageLocation("test")

    val expression = {
      et.Let(
        bindings=List((storageLoc, pureExpr)),
        innerExprs=List(pureExpr, pureExpr)
      )
    }

    assert(IsPureExpression(expression) === true)
  }
  
  test("letting impure bindings to a pure expression is impure") {
    val storageLoc = new StorageLocation("test")

    val expression = {
      et.Let(
        bindings=List((storageLoc, impureExpr)),
        innerExprs=List(pureExpr, pureExpr)
      )
    }

    assert(IsPureExpression(expression) === false)
  }
  
  test("letting pure bindings to an impure expression is impure") {
    val storageLoc = new StorageLocation("test")

    val expression = {
      et.Let(
        bindings=List((storageLoc, pureExpr)),
        innerExprs=List(pureExpr, impureExpr)
      )
    }

    assert(IsPureExpression(expression) === false)
  }
}
