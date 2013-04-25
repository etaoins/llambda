package llambda

sealed abstract class BoundValue

// These are normal bindings
class BoundExpression(expression : et.Expression) extends BoundValue

// These are arguments to a procedure
class ProcedureArg extends BoundValue

// These are the primitive expression types in R7Rs
sealed abstract class PrimitiveExpression extends BoundValue

// These are what (define-syntax) creates
case class SyntaxRule(pattern : List[ast.Datum], template : ast.Datum)
case class SyntaxBinding(literals : List[String], rules : List[SyntaxRule], scope : Scope)  extends BoundValue

class Scope(bindings : Map[String, BoundValue], parent : Option[Scope] = None) {
  def get(name : String) : Option[BoundValue] = 
    bindings.get(name).orElse {
      parent match {
        case Some(parentScope) => parentScope.get(name)
        case None => None
      }
    }
}

/** Bindings for the primitive expressions defined in (scheme core) */
object SchemePrimitives {
  object Lambda extends PrimitiveExpression
  object Quote extends PrimitiveExpression
  object If extends PrimitiveExpression
  object Set extends PrimitiveExpression

  val bindings = {
    Map(
      "lambda" -> Lambda,
      "quote" -> Quote,
      "if" -> If,
      "set!" -> Set
    )
  }
}
