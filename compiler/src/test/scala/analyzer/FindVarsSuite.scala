package io.llambda.compiler.analyzer
import io.llambda

import llambda.compiler.{et, ast, StorageLocation}
import org.scalatest.FunSuite

class FindVarsSuite extends FunSuite {
  test("literals have no vars") {
    val testExpr = et.Literal(ast.StringLiteral("Hello, world"))

    assert(FindVars(testExpr) === FoundVars())
  }

  test("immutable let") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExpr = et.Bind(
      List(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    )
    
    assert(FindVars(testExpr) === FoundVars(
      mutableVars=Set(),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    ))
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

    val foundVars = FindVars(et.Begin(testExprs))

    assert(foundVars === FoundVars(
      mutableVars=Set(testLocA),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    ))
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
        et.Literal(ast.UnitValue())
      ))

    val foundVars = FindVars(et.Begin(testExprs))

    assert(foundVars === FoundVars(
      mutableVars=Set(testLocA),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    ))
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
    
    val foundVars = FindVars(et.Begin(testExprs))

    assert(foundVars === FoundVars(
      mutableVars=Set(testLocA),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    ))
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
    
    val foundVars = FindVars(et.Begin(testExprs))

    assert(foundVars === FoundVars(
      mutableVars=Set(testLocA),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    ))
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
    
    val foundVars = FindVars(et.Begin(testExprs))

    assert(foundVars === FoundVars(
      mutableVars=Set(testLocA),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    ))
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
    
    val foundVars = FindVars(et.Begin(testExprs))

    assert(foundVars === FoundVars(
      mutableVars=Set(testLocA),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false)),
        testLocC -> et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
      )
    ))
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
    
    val foundVars = FindVars(et.Begin(testExprs))
    
    assert(foundVars === FoundVars(
      mutableVars=Set(testLocA, testLocB),
      initializers=Map(
        testLocA -> et.Literal(ast.BooleanLiteral(true)),
        testLocB -> et.Literal(ast.BooleanLiteral(false))
      )
    ))
  }
}
