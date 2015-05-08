package io.llambda.compiler.dialect
import io.llambda

/** Represents a Scheme language dialect */
sealed abstract class Dialect {
  /** Human-readable name of the dialect */
  val name : String

  /** List of feature identifiers this dialect defines */
  val dialectFeatures : Set[String]

  /** Indicates if pairs are immutable in this dialect */
  val pairsAreImmutable : Boolean

  /** Indicates if top-level redefinitions are allowed */
  val allowTopLevelRedefinition : Boolean

  /** Indicates if the root program file should be case folded
    *
    * This does not apply to included files or libraries; (include-ci) can be used to explicitly include files case
    * insensitively
    */
  def caseFoldPrograms : Boolean

  /** List of libraries to implicitly load in the main program */
  def implicitLibraryNames : List[Seq[String]]
}

/** R5RS compatibility mode */
object R5RS extends Dialect {
  val name = "r5rs"
  val dialectFeatures = Set[String]()
  val pairsAreImmutable = false
  val allowTopLevelRedefinition = true
  val caseFoldPrograms = true
  val implicitLibraryNames = List(
    List("scheme", "r5rs")
  )
}

/** R7RS as defined by scheme-reports.org */
object R7RS extends Dialect {
  val name = "r7rs"
  val dialectFeatures = Set("r7rs")
  val pairsAreImmutable = false
  val allowTopLevelRedefinition = true
  val caseFoldPrograms = false
  val implicitLibraryNames = Nil
}

object Llambda extends Dialect {
  val name = "llambda"
  val dialectFeatures = Set("immutable-pairs")
  val pairsAreImmutable = true
  val allowTopLevelRedefinition = false
  val caseFoldPrograms = false
  val implicitLibraryNames = Nil
}

object Dialect {
  val default = Llambda

  val dialects = Map(
    R5RS.name -> R5RS,
    R7RS.name -> R7RS,
    Llambda.name -> Llambda
  )
}
