package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}

sealed abstract class BoundValue

/** Normal Scheme binding of a value
  *
  * @param  sourceName   Name of the variable in the Scheme source. This is used for error reporting and chosing
  *                      names in the generated IR
  * @param  schemeType   Type of the storage location. Normal Scheme values are untyped which is represented by
  *                      AnySchemeType. Typed Scheme may explicitly restrict values further.
  */
class StorageLocation(
    val sourceName: String,
    val schemeType: vt.SchemeType = vt.AnySchemeType
) extends BoundValue {
  override def toString = "$" + sourceName

  /** Indicates if mutations of this location should be forbidden */
  val forceImmutable: Boolean = false
}

/** Record type constructor binding
  *
  * This is used to distinguish record type constructor for unapplication in pattern matching
  *
  * @param  constructor  Record type constructor for this binding
  */
class BoundRecordConstructor(
    val constructor: et.RecordConstructor,
    sourceName: String,
    schemeType: vt.SchemeType = vt.AnySchemeType
) extends StorageLocation(sourceName, schemeType) {
  // If we allow mutation then pattern matching can be "tricked" in to matching a variable initialised with a record
  // type constructor and then rebound to another value
  override val forceImmutable = true
}

// These are procedure with the semantics of the same procedure defined in our standard library
// This allows the compiler to optimise them based on their known semantics
class StdlibProcedure(
    val stdlibName: String,
    schemeType: vt.SchemeType = vt.AnySchemeType
) extends StorageLocation(stdlibName, schemeType) {
  override def toString = "&" + stdlibName

  // R7RS doesn't allow mutating imported definition and (define-stdlib-procedure) should only be used as library
  // exports. This prevents us from treating a variable initialised as a stdlib procedure and rebound to another value
  // as a stdlib procedure with the original stdlibName
  override val forceImmutable = true
}

// These are primitive expressions treated specially by the frontend
abstract class PrimitiveExpr extends BoundValue

/** Type constructors
  *
  * Type constructors build types based off of type arguments passed to them. These types aren't necessarily distinct
  * from existing types; typically a type constuctor with the same arguments will produce an identical type.
  */
sealed abstract class TypeConstructor extends BoundValue

/** Intrinsic type constructors
  *
  * These are analogous to primitive expressions in that they're implemented in the compiler frontend and provide a
  * basis for more complex type constructors to be built on.
  */
abstract class PrimitiveTypeConstructor extends TypeConstructor

/** User defined type constructors
  *
  * These allow the construction of types based off type arguments
  */
case class UserDefinedTypeConstructor(vars: List[pm.TypeVar], definition: vt.SchemeType) extends TypeConstructor

/** Type constructor for literal types such as booleans or symbols */
case object LiteralTypeConstructor extends TypeConstructor

// These are what (define-syntax) creates

/** Represents a placeholder in a macro pattern */
sealed abstract class SyntaxVariable extends SourceLocated

/** Represents a placeholder in a macro pattern that resolved to a bound symbol
  *
  * This will only match other symbols bound to the same value regardless of the symbol's name
  */
case class BoundSyntaxVariable(boundValue: BoundValue) extends SyntaxVariable

/** Represents a placeholder in a macro pattern that resolved to an unbound symbol
  *
  * This will only match other unbound symbols with the same name
  */
case class UnboundSyntaxVariable(identifier: String) extends SyntaxVariable

/** Represents a native library as a source of NFI functions */
sealed abstract class NativeLibrary extends BoundValue

/** Represents the libraries implicitly linked with all Llambda programs
  *
  * This includes the Llambda core runtime and the system C library
  */
object NativeSystemLibrary extends NativeLibrary

/** Represents a static native library */
case class NativeStaticLibrary(libraryName: String) extends NativeLibrary

/** Encapsulates information about pattern variables in a transformer
  *
  * This is kept only to prevent repeatedly re-parsing the pattern. It does not contain any information that could not
  * be regenerated from the Transformer alone
  */
case class PatternVariables(
    variables: List[SyntaxVariable] = Nil,
    subpatterns: Vector[PatternVariables] = Vector()
) {
  def ++(other: PatternVariables): PatternVariables =
    PatternVariables(
      variables=(variables ++ other.variables),
      subpatterns=(subpatterns ++ other.subpatterns)
    )
}

/** Represents a macro transformer
  *
  * Transformers are a pattern and an associated expansion template. A macro is made of one or more transformers that
  * are sequentially tried until a matching transformer is found.
  */
case class Transformer(pattern: sst.ScopedDatum, patternVariables: PatternVariables, template: sst.ScopedDatum)

case class BoundSyntax(
    ellipsisVariable: SyntaxVariable,
    literals: Set[SyntaxVariable],
    transformers: List[Transformer],
    debugContext: debug.SubprogramContext
) extends BoundValue

object SyntaxVariable {
  def fromSymbol(scopedSymbol: sst.Symbol): SyntaxVariable = {
    scopedSymbol.resolveOpt match {
      case Some(boundValue) =>
        BoundSyntaxVariable(boundValue).assignLocationFrom(scopedSymbol)
      case None =>
        UnboundSyntaxVariable(scopedSymbol.name).assignLocationFrom(scopedSymbol)
    }
  }
}

case class BoundType(valueType: vt.ValueType) extends BoundValue

/** Scope can look up bindings by name and return a list of all identifiers  */
sealed class Scope(val bindings: collection.mutable.Map[String, BoundValue], parent: Option[Scope] = None) {
  val typeDeclarations = new collection.mutable.HashMap[sst.Symbol, LocTypeDeclaration]

  def get(name: String): Option[BoundValue] =
    bindings.get(name).orElse {
      parent.flatMap(_.get(name))
    }

  def ++=(values: Map[String, BoundValue]) = {
    bindings ++= values
  }

  def +=(kv: (String, BoundValue)) {
    bindings += kv
  }

  def apply(name: String) =
    get(name).get
}

final class ImmutableScope(binding: collection.mutable.Map[String, BoundValue], parent: Option[Scope] = None) extends Scope(binding, parent) {
  override def ++=(values: Map[String, BoundValue]) = {
    throw new InternalCompilerErrorException("Attempted to mutate an immutable scope")
  }

  override def +=(kv: (String, BoundValue)) {
    throw new InternalCompilerErrorException("Attempted to mutate an immutable scope")
  }
}

object Scope {
  /** Generates a scope mapping to introduce the passed bound values
    *
    * For every distinct scope in the bound values a new child scope is created containg the bound values for that
    * scope. The mapping of parent scope to child scope is then returned. This can be passed to
    * [[sst.ScopedDatum.rescope]] to moved scoped data in to the new scopes.
    */
  def mappingForBoundValues(boundValues: Seq[(sst.Symbol, BoundValue)]): Map[Scope, Scope] = {
    val varsForScope = boundValues groupBy(_._1.scope)

    varsForScope map { case (oldScope, scopeVars) =>
      val bindings = collection.mutable.Map(scopeVars.map { case (varSymbol, boundValue) =>
        varSymbol.name -> boundValue
      }: _*)

      (oldScope -> new Scope(bindings, Some(oldScope)))
    }
  }
}
