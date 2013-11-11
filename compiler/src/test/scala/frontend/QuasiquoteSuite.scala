package llambda.frontend

import org.scalatest.FunSuite
import llambda._

class QuasiquoteSuite extends FunSuite with testutil.ExpressionHelpers {
  // We need (scheme base) to look for (list) and (vector)
  // which aren't primitives
  val schemeBaseBindings = libraryLoader.loadSchemeBase
  implicit val baseScope = new ImmutableScope(collection.mutable.Map(schemeBaseBindings.toSeq : _*))

  def schemeBaseStorageLoc(name : String) : StorageLocation =  
    schemeBaseBindings(name) match {
      case storageLoc : StorageLocation =>
        storageLoc

      case _ =>
        throw new Exception("Attempted to get reference to non-storage loc")
    }

  val listLoc = schemeBaseStorageLoc("list")
  val plusLoc = schemeBaseStorageLoc("+")
  val vectorLoc = schemeBaseStorageLoc("vector")

  test("unquoting outside of a quasiquote fails") {
    intercept[BadSpecialFormException] {
      expressionFor(",4")
    }
  }

  test("quasiquote list without unquoting") {
    assert(expressionFor("`(1 2 3)") === 
      et.Apply(et.VarRef(listLoc), List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Literal(ast.IntegerLiteral(2)),
        et.Literal(ast.IntegerLiteral(3))
      ))
    )
  }
  
  test("quasiquote list with non-splicing unquoting") {
    assert(expressionFor("`(1 ,(+ 2 3) 4)") === 
      et.Apply(et.VarRef(listLoc), List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Apply(et.VarRef(plusLoc), List(
          et.Literal(ast.IntegerLiteral(2)),
          et.Literal(ast.IntegerLiteral(3))
        )),
        et.Literal(ast.IntegerLiteral(4))
      ))
    )
  }
  
  test("quasiquote vector without unquoting") {
    assert(expressionFor("`#(1 2 3)") === 
      et.Apply(et.VarRef(vectorLoc), List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Literal(ast.IntegerLiteral(2)),
        et.Literal(ast.IntegerLiteral(3))
      ))
    )
  }
  
  test("quasiquote vector with non-splicing unquoting") {
    assert(expressionFor("`#(1 ,(+ 2 3) 4)") === 
      et.Apply(et.VarRef(vectorLoc), List(
        et.Literal(ast.IntegerLiteral(1)),
        et.Apply(et.VarRef(plusLoc), List(
          et.Literal(ast.IntegerLiteral(2)),
          et.Literal(ast.IntegerLiteral(3))
        )),
        et.Literal(ast.IntegerLiteral(4))
      ))
    )
  }
}
