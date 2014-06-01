package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite
import scala.util.parsing.input.NoPosition

import llambda.compiler._

class ExprLocatingSuite extends FunSuite with testutil.ExprHelpers {
  val primitiveScope = new ImmutableScope(collection.mutable.Map(PrimitiveExprs.bindings.toSeq : _*))
  
  val plusLoc = new StorageLocation("+")
  implicit val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))

  private def assertLocated(expr : et.Expr) {
    assert(expr.locationOpt.isDefined, "Expr is unlocated")
  }

  test("variable references are located") {
    assertLocated(exprFor("+"))
  }
  
  test("applications are located") {
    assertLocated(exprFor("(+ 1 2)"))
  }

  test("literals are located") {
    assertLocated(exprFor("#t"))
  }

  test("conditions are located") {
    assertLocated(exprFor("(if #t 1 2)"))
  }
}

