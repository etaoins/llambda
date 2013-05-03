package llambda

sealed abstract class BoundValue

// These are normal bindings
class StorageLocation extends BoundValue

// These are arguments to a procedure
class ProcedureArg extends BoundValue

// These are the primitive expression types in R7Rs
sealed abstract class PrimitiveExpression extends BoundValue

// These are what (define-syntax) creates
case class SyntaxRule(pattern : List[sst.ScopedDatum], template : sst.ScopedDatum)
case class BoundSyntax(literals : List[String], rules : List[SyntaxRule])  extends BoundValue

/** BindingSets can look up bindings by name and return a list of all identifiers  */
final class Scope(val bindings : collection.mutable.Map[String, BoundValue], parent : Option[Scope] = None) {
  def get(name : String) : Option[BoundValue] = 
    bindings.get(name).orElse {
      parent match {
        case Some(resolver) => resolver.get(name)
        case None => None
      }
    }

  def +(kv : (String, BoundValue)) : Scope = {
    new Scope(bindings + kv, parent)
  }

  def ++(values : Map[String, BoundValue]) : Scope = {
    new Scope(bindings ++ values, parent)
  }

  // This is a hack for define-syntax to inject the new syntax definition in to its scope
  // Otherwise recursive macros don't work correctly
  def +=(kv : (String, BoundValue)) {
    bindings += kv
  }
}

/** Bindings for the primitive expressions defined in (scheme core) */
object SchemePrimitives {
  object Lambda extends PrimitiveExpression
  object Quote extends PrimitiveExpression
  object If extends PrimitiveExpression
  object Set extends PrimitiveExpression
  object SyntaxError extends PrimitiveExpression

  val bindings = {
    Map[String, BoundValue](
      "lambda" -> Lambda,
      "quote" -> Quote,
      "if" -> If,
      "set!" -> Set,
      "syntax-error" -> SyntaxError
    )
  }
}
