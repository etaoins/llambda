package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite
import llambda.compiler._

class QuasiquoteSuite extends FunSuite with testutil.ExprHelpers {
  implicit val baseScope = new ImmutableScope(collection.mutable.Map(llambdaBaseBindings.toSeq: _*))

  def llambdaBaseProcedure(name: String): et.Expr =
    llambdaBaseBindings(name) match {
      case storageLoc: StorageLocation =>
        et.VarRef(storageLoc)

      case _ =>
        throw new Exception("Attempted to get reference to non-storage loc")
    }

  val plusProc = llambdaBaseProcedure("+")

  val listProc = llambdaBaseProcedure("list")
  val appendProc = llambdaBaseProcedure("append")

  val vectorProc = llambdaBaseProcedure("vector")
  val listToVectorProc = llambdaBaseProcedure("list->vector")

  test("unquoting outside of a quasiquote fails") {
    intercept[BadSpecialFormException] {
      exprFor(",4")
    }
  }

  test("splicing unquoting outside of a quasiquote fails") {
    intercept[BadSpecialFormException] {
      exprFor(",@4")
    }
  }

  test("empty quasiquote list") {
    assert(exprFor("`()") ===
      et.Literal(ast.ProperList(Nil))
    )
  }

  test("quasiquote list without unquoting") {
    assert(exprFor("`(1 2 3)") ===
      et.Literal(
        ast.ProperList(List(
          ast.Integer(1),
          ast.Integer(2),
          ast.Integer(3)
        ))
      )
    )
  }

  test("quasiquote list with non-splicing unquoting") {
    assert(exprFor("`(1 ,(+ 2 3) 4)") ===
      et.Apply(listProc, List(
        et.Literal(ast.Integer(1)),
        et.Apply(plusProc, List(
          et.Literal(ast.Integer(2)),
          et.Literal(ast.Integer(3))
        )),
        et.Literal(ast.Integer(4))
      ))
    )
  }

  test("quasiquote list with splicing unquoting mid-list") {
    assert(exprFor("`(1 ,@(list 2 3) 4)") ===
      et.Apply(appendProc, List(
        et.Literal(ast.ProperList(List(
          ast.Integer(1)
        ))),
        et.Apply(listProc, List(
          et.Literal(ast.Integer(2)),
          et.Literal(ast.Integer(3))
        )),
        et.Literal(ast.ProperList(List(
          ast.Integer(4)
        )))
      ))
    )
  }

  test("quasiquote list with splicing unquoting at end") {
    assert(exprFor("`(1 ,@(list 2 3))") ===
      et.Apply(appendProc, List(
        et.Literal(ast.ProperList(List(
          ast.Integer(1)
        ))),
        et.Apply(listProc, List(
          et.Literal(ast.Integer(2)),
          et.Literal(ast.Integer(3))
        ))
      ))
    )
  }

  test("complex quasiquote list") {
    assert(exprFor("`(1 2 ,@(list 3 4) ,(+ 2 3) 6)") ===
      et.Apply(appendProc, List(
        et.Literal(ast.ProperList(List(
          ast.Integer(1),
          ast.Integer(2)
        ))),
        et.Apply(listProc, List(
          et.Literal(ast.Integer(3)),
          et.Literal(ast.Integer(4))
        )),
        et.Apply(listProc, List(
          et.Apply(plusProc, List(
            et.Literal(ast.Integer(2)),
            et.Literal(ast.Integer(3))
          )),
          et.Literal(ast.Integer(6))
        ))
      ))
    )
  }

  test("empty quasiquote vector") {
    assert(exprFor("`#()") ===
      et.Literal(ast.Vector(Vector()))
    )
  }

  test("quasiquote vector without unquoting") {
    assert(exprFor("`#(1 2 3)") ===
      et.Literal(ast.Vector(Vector(
        ast.Integer(1),
        ast.Integer(2),
        ast.Integer(3)
      )))
    )
  }

  test("quasiquote vector with non-splicing unquoting") {
    assert(exprFor("`#(1 ,(+ 2 3) 4)") ===
      et.Apply(vectorProc, List(
        et.Literal(ast.Integer(1)),
        et.Apply(plusProc, List(
          et.Literal(ast.Integer(2)),
          et.Literal(ast.Integer(3))
        )),
        et.Literal(ast.Integer(4))
      ))
    )
  }

  test("quasiquote vector with splicing unquoting mid-vector") {
    assert(exprFor("`#(1 ,@(list 2 3) 4)") ===
      et.Apply(listToVectorProc, List(
        et.Apply(appendProc, List(
          et.Literal(ast.ProperList(List(
            ast.Integer(1)
          ))),
          et.Apply(listProc, List(
            et.Literal(ast.Integer(2)),
            et.Literal(ast.Integer(3))
          )),
          et.Literal(ast.ProperList(List(
            ast.Integer(4)
          )))
        ))
      ))
    )
  }

  test("quasiquote vector with splicing unquoting at end") {
    assert(exprFor("`#(1 ,@(list 2 3))") ===
      et.Apply(listToVectorProc, List(
        et.Apply(appendProc, List(
          et.Literal(ast.ProperList(List(
            ast.Integer(1)
          ))),
          et.Apply(listProc, List(
            et.Literal(ast.Integer(2)),
            et.Literal(ast.Integer(3))
          ))
        ))
      ))
    )
  }

  test("complex quasiquote vector") {
    assert(exprFor("`#(1 2 ,@(list 3 4) ,(+ 2 3) 6)") ===
      et.Apply(listToVectorProc, List(
        et.Apply(appendProc, List(
          et.Literal(ast.ProperList(List(
            ast.Integer(1),
            ast.Integer(2)
          ))),
          et.Apply(listProc, List(
            et.Literal(ast.Integer(3)),
            et.Literal(ast.Integer(4))
          )),
          et.Apply(listProc, List(
            et.Apply(plusProc, List(
              et.Literal(ast.Integer(2)),
              et.Literal(ast.Integer(3))
            )),
            et.Literal(ast.Integer(6))
          ))
        ))
      ))
    )
  }


}
