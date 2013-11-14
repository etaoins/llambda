package llambda.analyzer

import llambda.{et, ast, StorageLocation}
import org.scalatest.FunSuite

class FindMutableVarsSuite extends FunSuite {
  test("literals have no mutable vars") {
    val testExpr = et.Literal(ast.StringLiteral("Hello, world"))

    assert(FindMutableVars(testExpr) === Set())
  }

  test("let doesn't make a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExpr = et.Bind(
      List(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    )
    
    assert(FindMutableVars(testExpr) === Set())
  }
  
  test("direct mutate var makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.Bind(
        List(
          testLocA -> et.Literal(ast.BooleanLiteral(true)),
          testLocB -> et.Literal(ast.BooleanLiteral(false))
        )
      ),
      et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
    )

    val mutables = testExprs.flatMap(FindMutableVars.apply).toSet
    assert(mutables === Set(testLocA))
  }
  
  test("mutate var in one side of conditional makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.Bind(
        List(
          testLocA -> et.Literal(ast.BooleanLiteral(true)),
          testLocB -> et.Literal(ast.BooleanLiteral(false))
        )
      ),
      et.Cond(
        et.Literal(ast.BooleanLiteral(false)),
        et.MutateVar(testLocA, et.Literal(ast.EmptyList())),
        et.Literal(ast.UnspecificValue())
      ))

    val mutables = testExprs.flatMap(FindMutableVars.apply).toSet
    assert(mutables === Set(testLocA))
  }
  
  test("mutate var inside lambda parameters make a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List( 
      et.Bind(
        List(
          testLocA -> et.Literal(ast.BooleanLiteral(true)),
          testLocB -> et.Literal(ast.BooleanLiteral(false))
        )
      ),
      et.Apply(
        et.Lambda(
          fixedArgs=Nil,
          restArg=None,
          body=et.Begin(Nil)
        ),
        List(
          et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
        )
      )
    )
    
    val mutables = testExprs.flatMap(FindMutableVars.apply).toSet
    assert(mutables === Set(testLocA))
  }
  
  test("mutate var inside self-executing lambda makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.Bind(
        List(
          testLocA -> et.Literal(ast.BooleanLiteral(true)),
          testLocB -> et.Literal(ast.BooleanLiteral(false))
        )
      ),
      et.Apply(
        et.Lambda(
          fixedArgs=Nil,
          restArg=None,
          body=et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
        ),
        Nil
      )
    )
    
    val mutables = testExprs.flatMap(FindMutableVars.apply).toSet
    assert(mutables === Set(testLocA))
  }
  
  test("mutate var captured inside lambda makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.Bind(
        List(
          testLocA -> et.Literal(ast.BooleanLiteral(true)),
          testLocB -> et.Literal(ast.BooleanLiteral(false))
        )
      ),
      et.Lambda(
        fixedArgs=Nil,
        restArg=None,
        body=et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
      )
    )
    
    val mutables = testExprs.flatMap(FindMutableVars.apply).toSet
    assert(mutables === Set(testLocA))
  }

  test("mutate var inside other let makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")
    val testLocC = new StorageLocation("testLocC")

    val testExprs = List(
      et.Bind(
        List(
          testLocA -> et.Literal(ast.BooleanLiteral(true)),
          testLocB -> et.Literal(ast.BooleanLiteral(false))
        )
      ),
      et.Bind(
        List(
          testLocC -> et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
        )
      )
    )
    
    val mutables = testExprs.flatMap(FindMutableVars.apply).toSet
    assert(mutables === Set(testLocA))
  }

  test("mutate var inside other mutate var RHS makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.Bind(
        List(
          testLocA -> et.Literal(ast.BooleanLiteral(true)),
          testLocB -> et.Literal(ast.BooleanLiteral(false))
        )
      ),
      et.MutateVar(testLocA, et.MutateVar(testLocB, et.Literal(ast.BooleanLiteral(true))))
    )
    
    val mutables = testExprs.flatMap(FindMutableVars.apply).toSet
    assert(mutables === Set(testLocA, testLocB))
  }
}
