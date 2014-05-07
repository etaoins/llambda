package io.llambda.compiler.reducer.reportproc
import io.llambda

import llambda.compiler._
import org.scalatest.{FunSuite, Inside}

class VectorProcReducerSuite extends FunSuite with Inside with testutil.ExpressionHelpers {
  val schemeBaseBindings = libraryLoader.loadSchemeBase(frontendConfig)
  implicit val scope = new Scope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))

  test("type predicates") {
    assert(reductionFor("""(vector? #(0 1 2))""") ===
      et.Literal(ast.BooleanLiteral(true))
    )
    
    assert(reductionFor("""(vector? #f)""") ===
      et.Literal(ast.BooleanLiteral(false))
    )
  }

  test("static (vector-length)") {
    assert(reductionFor("""(vector-length #(1 2 3 4 5))""") ===
      et.Literal(ast.IntegerLiteral(5))
    )

    assert(reductionFor("""(vector-length #())""") ===
      et.Literal(ast.IntegerLiteral(0))
    )
    
    inside(reductionFor("""(vector-length 5)""")) {
      case et.Apply(et.VarRef(_), List(
        et.Literal(ast.IntegerLiteral(5))
      )) =>
        Unit
    }
  }
}
