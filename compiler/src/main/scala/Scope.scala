package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}

sealed abstract class BoundValue

/** Normal Scheme binding of a value
  *
  * @param  sourceName   Name of the variable in the Scheme source. This is used for error reporting and chosing
  *                      names in the generated IR
  * @param  schemeType   Type of the storage location. Normal Scheme values are untyped which is represented by
  *                      DatumCell. Typed Scheme may explicitly restrict values further. 
  */
class StorageLocation(
    val sourceName : String,
    val schemeType : vt.SchemeType = vt.AnySchemeType
) extends BoundValue {
  override def toString = "$" + sourceName

  def hasTypeConstraints =
    schemeType != vt.AnySchemeType
}

// These are procedure with the semantics of the same procedure defined in R7RS
// This allows the compiler to optimize or REPL emulate them based on their documented semantics
class ReportProcedure(val reportName : String) extends StorageLocation(reportName) {
  override def toString = "&" + reportName
}

// These are primitive expressions treated specially by the frontend
abstract class PrimitiveExpr extends BoundValue

// These are similar to primitive expressions but they can only appear when creating types
abstract class PrimitiveTypeConstructor extends BoundValue

// These are what (define-syntax) creates

/** Represents a placeholder in a macro pattern */
sealed abstract class SyntaxVariable extends SourceLocated

/** Represents a placeholder in a macro pattern that resolved to a bound symbol
  *
  * This will only match other symbols bound to the same value regardless of the symbol's name
  */
case class BoundSyntaxVariable(boundValue : BoundValue) extends SyntaxVariable

/** Represents a placeholder in a macro pattern that resolved to an unbound symbol
  *
  * This will only match other unbound symbols with the same name
  */
case class UnboundSyntaxVariable(identifier : String) extends SyntaxVariable

/** Encapsulates information about pattern variables in a transformer
  *
  * This is kept only to prevent repeatedly re-parsing the pattern. It does not contain any information that could not
  * be regenerated from the Transformer alone
  */
case class PatternVariables(
    variables : List[SyntaxVariable] = Nil,
    subpatterns : Vector[PatternVariables] = Vector()
) {
  def ++(other : PatternVariables) : PatternVariables = 
    PatternVariables(
      variables=(variables ++ other.variables),
      subpatterns=(subpatterns ++ other.subpatterns)
    )
}

/** Represents a macro transformer
  *
  * Transformers are a pattern and an  associated expansion template. A macro is made of one or more transformers that
  * are sequentially tried until a matching transformer is found.
  */
case class Transformer(pattern : sst.ScopedDatum, patternVariables : PatternVariables, template : sst.ScopedDatum)

case class BoundSyntax(
    ellipsisIdentifier : String,
    literals : Set[SyntaxVariable],
    transformers : List[Transformer],
    debugContext : debug.SubprogramContext
) extends BoundValue

object SyntaxVariable {
  def fromSymbol(scopedSymbol : sst.ScopedSymbol) : SyntaxVariable = {
    scopedSymbol.resolveOpt match {
      case Some(boundValue) =>
        BoundSyntaxVariable(boundValue).assignLocationFrom(scopedSymbol)
      case None =>
        UnboundSyntaxVariable(scopedSymbol.name).assignLocationFrom(scopedSymbol)
    }
  }
}

case class BoundType(valueType : vt.ValueType) extends BoundValue

/** Scope can look up bindings by name and return a list of all identifiers  */
sealed class Scope(val bindings : collection.mutable.Map[String, BoundValue], parent : Option[Scope] = None) {
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

  def ++=(values : Map[String, BoundValue]) = {
    bindings ++= values
  }
    
  def +=(kv : (String, BoundValue)) {
    bindings += kv
  }

  def apply(name : String) =
    get(name).get
}

final class ImmutableScope(binding : collection.mutable.Map[String, BoundValue], parent : Option[Scope] = None) extends Scope(binding, parent) {
  override def ++=(values : Map[String, BoundValue]) = {
    throw new InternalCompilerErrorException("Attempted to mutate an immutable scope")
  }
  
  override def +=(kv : (String, BoundValue)) {
    throw new InternalCompilerErrorException("Attempted to mutate an immutable scope")
  }
}

