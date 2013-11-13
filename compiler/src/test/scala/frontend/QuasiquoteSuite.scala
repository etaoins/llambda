package llambda.frontend

import org.scalatest.FunSuite
import llambda._

class QuasiquoteSuite extends FunSuite with testutil.ExpressionHelpers {
  // We need (scheme base) to look for (list) and (vector)
  // which aren't primitives
  val schemeBaseBindings = libraryLoader.loadSchemeBase
  implicit val baseScope = new ImmutableScope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))

  def schemeBaseProcedure(name : String) : et.Expression =  
    schemeBaseBindings(name) match {
      case storageLoc : StorageLocation =>
        et.VarRef(storageLoc)

      case _ =>
        throw new Exception("Attempted to get reference to non-storage loc")
    }

  val plusProc = schemeBaseProcedure("+")

  val listProc = schemeBaseProcedure("list")
  val appendProc = schemeBaseProcedure("append")

  val vectorProc = schemeBaseProcedure("vector")
  val listToVectorProc = schemeBaseProcedure("list->vector")

  test("unquoting outside of a quasiquote fails") {
    intercept[BadSpecialFormException] {
      expressionFor(",4")
    }
  }
  
  test("splicing unquoting outside of a quasiquote fails") {
    intercept[BadSpecialFormException] {
      expressionFor(",@4")
    }
  }
  
  test("empty quasiquote list") {
    assert(expressionFor("`()") === 
      et.Literal(ast.ProperList(Nil))
    )
  }

  test("quasiquote list without unquoting") {
    assert(expressionFor("`(1 2 3)") === 
      et.Literal(
        ast.ProperList(List(
          ast.IntegerLiteral(1),
          ast.IntegerLiteral(2),
          ast.IntegerLiteral(3)
        ))
      )
    )
  }
  
  test("quasiquote list with non-splicing unquoting") {
    assert(expressionFor("`(1 ,(+ 2 3) 4)") === 
      et.Apply(listProc, List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Apply(plusProc, List(
          et.Literal(ast.IntegerLiteral(2)),
          et.Literal(ast.IntegerLiteral(3))
        )),
        et.Literal(ast.IntegerLiteral(4))
      ))
    )
  }
  
  test("quasiquote list with splicing unquoting mid-list") {
    assert(expressionFor("`(1 ,@(list 2 3) 4)") === 
      et.Apply(appendProc, List(
        et.Literal(ast.ProperList(List(
          ast.IntegerLiteral(1)
        ))),
        et.Apply(listProc, List(
          et.Literal(ast.IntegerLiteral(2)),
          et.Literal(ast.IntegerLiteral(3))
        )),
        et.Literal(ast.ProperList(List(
          ast.IntegerLiteral(4)
        )))
      ))
    )
  }
  
  test("quasiquote list with splicing unquoting at end") {
    assert(expressionFor("`(1 ,@(list 2 3))") === 
      et.Apply(appendProc, List(
        et.Literal(ast.ProperList(List(
          ast.IntegerLiteral(1)
        ))),
        et.Apply(listProc, List(
          et.Literal(ast.IntegerLiteral(2)),
          et.Literal(ast.IntegerLiteral(3))
        ))
      ))
    )
  }
  
  test("complex quasiquote list") {
    assert(expressionFor("`(1 2 ,@(list 3 4) ,(+ 2 3) 6)") === 
      et.Apply(appendProc, List(
        et.Literal(ast.ProperList(List(
          ast.IntegerLiteral(1),
          ast.IntegerLiteral(2)
        ))),
        et.Apply(listProc, List(
          et.Literal(ast.IntegerLiteral(3)),
          et.Literal(ast.IntegerLiteral(4))
        )),
        et.Apply(listProc, List(
          et.Apply(plusProc, List(
            et.Literal(ast.IntegerLiteral(2)),
            et.Literal(ast.IntegerLiteral(3))
          )),
          et.Literal(ast.IntegerLiteral(6))
        ))
      ))
    )
  }
  
  test("empty quasiquote vector") {
    assert(expressionFor("`#()") === 
      et.Literal(ast.VectorLiteral(Vector()))
    )
  }
  
  test("quasiquote vector without unquoting") {
    assert(expressionFor("`#(1 2 3)") === 
      et.Literal(ast.VectorLiteral(Vector(
        ast.IntegerLiteral(1),
        ast.IntegerLiteral(2),
        ast.IntegerLiteral(3)
      )))
    )
  }
  
  test("quasiquote vector with non-splicing unquoting") {
    assert(expressionFor("`#(1 ,(+ 2 3) 4)") === 
      et.Apply(vectorProc, List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Apply(plusProc, List(
          et.Literal(ast.IntegerLiteral(2)),
          et.Literal(ast.IntegerLiteral(3))
        )),
        et.Literal(ast.IntegerLiteral(4))
      ))
    )
  }
  
  test("quasiquote vector with splicing unquoting mid-vector") {
    assert(expressionFor("`#(1 ,@(list 2 3) 4)") === 
      et.Apply(listToVectorProc, List(
        et.Apply(appendProc, List(
          et.Literal(ast.ProperList(List(
            ast.IntegerLiteral(1)
          ))),
          et.Apply(listProc, List(
            et.Literal(ast.IntegerLiteral(2)),
            et.Literal(ast.IntegerLiteral(3))
          )),
          et.Literal(ast.ProperList(List(
            ast.IntegerLiteral(4)
          )))
        ))
      ))
    )
  }
  
  test("quasiquote vector with splicing unquoting at end") {
    assert(expressionFor("`#(1 ,@(list 2 3))") === 
      et.Apply(listToVectorProc, List(
        et.Apply(appendProc, List(
          et.Literal(ast.ProperList(List(
            ast.IntegerLiteral(1)
          ))),
          et.Apply(listProc, List(
            et.Literal(ast.IntegerLiteral(2)),
            et.Literal(ast.IntegerLiteral(3))
          ))
        ))
      ))
    )
  }
  
  test("complex quasiquote vector") {
    assert(expressionFor("`#(1 2 ,@(list 3 4) ,(+ 2 3) 6)") === 
      et.Apply(listToVectorProc, List(
        et.Apply(appendProc, List(
          et.Literal(ast.ProperList(List(
            ast.IntegerLiteral(1),
            ast.IntegerLiteral(2)
          ))),
          et.Apply(listProc, List(
            et.Literal(ast.IntegerLiteral(3)),
            et.Literal(ast.IntegerLiteral(4))
          )),
          et.Apply(listProc, List(
            et.Apply(plusProc, List(
              et.Literal(ast.IntegerLiteral(2)),
              et.Literal(ast.IntegerLiteral(3))
            )),
            et.Literal(ast.IntegerLiteral(6))
          ))
        ))
      ))
    )
  }
  
  
}
