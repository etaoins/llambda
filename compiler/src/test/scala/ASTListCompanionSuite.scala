package llambda

import org.scalatest.FunSuite

class ASTListCompanionSuite extends FunSuite {
  test("proper list creation") {
    assert(ast.ProperList(List()) === ast.EmptyList)
    
    assert(ast.ProperList(List(ast.Symbol("a"))) === 
      ast.Pair(ast.Symbol("a"), ast.EmptyList))

    assert(ast.ProperList(List(ast.Symbol("a"), ast.Symbol("b"))) === 
      ast.Pair(ast.Symbol("a"),
        ast.Pair(ast.Symbol("b"),
          ast.EmptyList
        )
      )
    )
  }
  
  test("improper list creation") {
    assert(ast.ImproperList(List(ast.Symbol("a")), ast.Symbol("b")) === 
      ast.Pair(ast.Symbol("a"), ast.Symbol("b"))
    )
    
    assert(ast.ImproperList(List(ast.Symbol("a"), ast.Symbol("b")), ast.Symbol("c")) === 
      ast.Pair(ast.Symbol("a"), 
        ast.Pair(ast.Symbol("b"), ast.Symbol("c")))
    )
  }

  test("proper list extraction") {
    expectResult(Some(Nil)) {
      ast.ProperList.unapply(ast.EmptyList)
    }

    expectResult(Some(List(ast.Symbol("a")))) {
      ast.ProperList.unapply(
        ast.Pair(ast.Symbol("a"), ast.EmptyList)
      )
    }
    
    expectResult(Some(List(ast.Symbol("a"), ast.Symbol("b")))) {
      ast.ProperList.unapply(
        ast.Pair(ast.Symbol("a"), 
          ast.Pair(ast.Symbol("b"), ast.EmptyList)
        )
      )
    }

    expectResult(None) {
      ast.ProperList.unapply(ast.Symbol("a"))
    }
    
    expectResult(None) {
      ast.ProperList.unapply(
        ast.Pair(
          ast.Symbol("a"), ast.Symbol("b")
        )
      )
    }
  }

  test("improper list extraction") {
    expectResult(Some((List(ast.Symbol("a")), ast.Symbol("b")))) {
      ast.ImproperList.unapply(
        ast.Pair(
          ast.Symbol("a"), ast.Symbol("b")
        )
      )
    }
    
    expectResult(Some((List(ast.Symbol("a"), ast.Symbol("b")), ast.Symbol("c")))) {
      ast.ImproperList.unapply(
        ast.Pair(
          ast.Symbol("a"), ast.Pair(
            ast.Symbol("b"), ast.Symbol("c")
          )
        )
      )
    }
    
    expectResult(None) {
      ast.ImproperList.unapply(ast.Symbol("a"))
    }
    
    expectResult(None) {
      ast.ImproperList.unapply(
        ast.Pair(ast.Symbol("a"), ast.EmptyList)
      )
    }
  }
}

