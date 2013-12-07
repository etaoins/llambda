package llambda.frontend

import org.scalatest.FunSuite
import scala.util.parsing.input.NoPosition

import llambda._

class ExpressionLocatingSuite extends FunSuite with testutil.ExpressionHelpers {
  val primitiveScope = new ImmutableScope(collection.mutable.Map(PrimitiveExpressions.bindings.toSeq : _*))
  
  val plusLoc = new StorageLocation("+")
  implicit val plusScope = new Scope(collection.mutable.Map("+" -> plusLoc), Some(primitiveScope))

  private def assertLocated(expr : et.Expression) {
    assert(expr.pos ne NoPosition, "Expression is unlocated")
  }

  test("variable references are located") {
    assertLocated(expressionFor("+"))
  }
  
  test("applications are located") {
    assertLocated(expressionFor("(+ 1 2)"))
  }

  test("literals are located") {
    assertLocated(expressionFor("#t"))
  }

  test("conditions are located") {
    assertLocated(expressionFor("(if #t 1 2)"))
  }
}

