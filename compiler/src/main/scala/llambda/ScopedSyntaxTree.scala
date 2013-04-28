package llambda.sst

import llambda._

sealed abstract class ScopedDatum {
  val scope : Scope

  def unscope : ast.Datum
}

case class ScopedPair(scope : Scope, car : ScopedDatum, cdr : ScopedDatum) extends ScopedDatum {
  def unscope = ast.Pair(car.unscope, cdr.unscope)
}

case class ScopedAtom(scope : Scope, atom : ast.Atom) extends ScopedDatum {
  def unscope = atom
}

// The following two objects are essential copied from AbstractSyntaxTree
// TODO: Find a way to share code that actually creates less code
object ScopedImproperList {
  def unapply(datum : ScopedDatum) : Option[(List[ScopedDatum], ScopedDatum)] = datum match {
    case ScopedPair(_, _, ScopedAtom(_, ast.EmptyList)) => None // This is a proper list
    case ScopedPair(_, car, tail : ScopedPair)  => 
      ScopedImproperList.unapply(tail).map { case (head, terminator) =>
        (car :: head, terminator)
      }
    case ScopedPair(_, car, cdr) => Some(List(car), cdr)
    case _ => None
  }
}

object ScopedProperList {
  def unapply(datum : ScopedDatum) : Option[List[ScopedDatum]] = datum match {
    case ScopedAtom(_, ast.EmptyList) => Some(Nil)
    case ScopedPair(_, car, cdr) => ScopedProperList.unapply(cdr).map(car :: _)
    case _ => None
  }
}

// ExtractBody cares a lot about symbols. Make its life easier
object ScopedSymbol {
  def apply(scope : Scope, name : String) = sst.ScopedAtom(scope, ast.Symbol(name))

  def unapply(datum : ScopedDatum) : Option[(Scope, String)] = datum match {
    case ScopedAtom(scope, ast.Symbol(name)) => Some((scope, name))
    case _ => None
  }
}

object ScopedDatum {
  def apply(scope : Scope, datum : ast.Datum) : ScopedDatum = datum match {
    case ast.Pair(car, cdr) => ScopedPair(scope, ScopedDatum(scope, car), ScopedDatum(scope, cdr))
    case atom : ast.Atom  => ScopedAtom(scope, atom)
  }
}
