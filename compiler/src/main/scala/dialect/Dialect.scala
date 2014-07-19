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
}

/** R7RS as defined by scheme-reports.org */
object R7RS extends Dialect {
  val name = "r7rs"
  val dialectFeatures = Set("r7rs")
  val pairsAreImmutable = false
}

object Llambda extends Dialect {
  val name = "llambda"
  val dialectFeatures = Set("immutable-pairs")
  val pairsAreImmutable = true
}

object Dialect {
  val default = Llambda

  val dialects = Map(
    R7RS.name -> R7RS,
    Llambda.name -> Llambda
  )
}
