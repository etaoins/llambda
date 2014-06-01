package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}

sealed abstract class BoundValue

// These are normal bindings
class StorageLocation(val sourceName : String) extends BoundValue {
  override def toString = "$" + sourceName
}

// These are procedure with the semantics of the same procedure defined in R7RS
// This allows the compiler to optimize or REPL emulate them based on their documented semantics
class ReportProcedure(val reportName : String) extends StorageLocation(reportName) {
  override def toString = "&" + reportName
}

// These are primitive expressions treated specially by the frontend
abstract class PrimitiveExpr extends BoundValue

// These are what (define-syntax) creates
sealed abstract class SyntaxVariable extends SourceLocated
case class BoundSyntaxVariable(boundValue : BoundValue) extends SyntaxVariable
case class UnboundSyntaxVariable(identifier : String) extends SyntaxVariable

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

case class Transformer(pattern : sst.ScopedDatum, patternVariables : PatternVariables, template : sst.ScopedDatum)
case class BoundSyntax(ellipsisIdentifier : String, literals : Set[SyntaxVariable], transformers : List[Transformer])  extends BoundValue

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

// These are either intrinsic types or ones introduced by (define-record-type)
// or (define-native-type)
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

