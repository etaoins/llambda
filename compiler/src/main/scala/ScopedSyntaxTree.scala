package io.llambda.compiler.sst
import io.llambda

import scala.collection.immutable.{Vector => ScalaVector}

import llambda.compiler._


sealed abstract class ScopedDatum extends SourceLocated {
  protected def unlocatedUnscope: ast.Datum

  def unscope: ast.Datum = {
    val unlocated = unlocatedUnscope
    // Preserve our source location
    assignLocationTo(unlocated)

    unlocated
  }

  def rescoped(mapping: Map[Scope, Scope]): ScopedDatum
}

case class Pair(car: ScopedDatum, cdr: ScopedDatum) extends ScopedDatum {
  def unlocatedUnscope: ast.Pair = ast.Pair(car.unscope, cdr.unscope)
  override def toString = "(" + car + " . " + cdr + ")"

  def rescoped(mapping: Map[Scope, Scope]): Pair =
    Pair(car.rescoped(mapping), cdr.rescoped(mapping)).assignLocationFrom(this)
}

case class Vector(elements: ScalaVector[ScopedDatum]) extends ScopedDatum {
  def unlocatedUnscope = ast.Vector(elements.map(_.unscope))
  override def toString =
    "#(" + elements.map(_.toString).mkString(" ") + ")"

  def rescoped(mapping: Map[Scope, Scope]): Vector =
    Vector(elements.map(_.rescoped(mapping))).assignLocationFrom(this)
}

case class NonSymbolLeaf(atom: ast.NonSymbolLeaf) extends ScopedDatum {
  def unlocatedUnscope = atom
  override def toString = atom.toString

  def rescoped(mapping: Map[Scope, Scope]): NonSymbolLeaf =
    this
}

case class Symbol(scope: Scope, name: String) extends ScopedDatum {
  def unlocatedUnscope = ast.Symbol(name)
  override def toString = name + "@" + scope.hashCode.toHexString

  def resolveOpt: Option[BoundValue] = scope.get(name)

  def resolve: BoundValue = resolveOpt getOrElse {
    throw new UnboundVariableException(this, name)
  }

  def rescoped(mapping: Map[Scope, Scope]): Symbol =
    mapping.get(scope) match {
      case Some(newScope) =>
        Symbol(newScope, name).assignLocationFrom(this)

      case _ =>
        this
    }
}

// The following two objects are essential copied from AbstractSyntaxTree
// TODO: Find a way to share code that actually creates less code
object AnyList {
  def unapply(datum: ScopedDatum): Option[(List[ScopedDatum], ScopedDatum)] = datum match {
    case Pair(car, tail: Pair)  =>
      AnyList.unapply(tail).map { case (head, terminator) =>
        (car :: head, terminator)
      }
    case Pair(car, cdr) => Some((List(car), cdr))
    case _ => None
  }

  def apply(head: Seq[ScopedDatum], terminator: ScopedDatum) = {
    head.foldRight(terminator: ScopedDatum) { (car, cdr) =>
      Pair(car, cdr)
    }
  }
}

object ImproperList {
  def unapply(datum: ScopedDatum): Option[(List[ScopedDatum], ScopedDatum)] = datum match {
    case Pair(_, NonSymbolLeaf(ast.EmptyList())) => None // This is a proper list
    case Pair(car, tail: Pair)  =>
      ImproperList.unapply(tail).map { case (head, terminator) =>
        (car :: head, terminator)
      }
    case Pair(car, cdr) => Some((List(car), cdr))
    case _ => None
  }
}

object ProperList {
  def apply(data: Seq[ScopedDatum]): ScopedDatum =
    data.foldRight(NonSymbolLeaf(ast.EmptyList()): sst.ScopedDatum) { (car, cdr) =>
      Pair(car, cdr)
    }

  def unapply(datum: ScopedDatum): Option[List[ScopedDatum]] = datum match {
    case NonSymbolLeaf(ast.EmptyList()) => Some(Nil)
    case Pair(car, cdr) => ProperList.unapply(cdr).map(car :: _)
    case _ => None
  }
}

object ScopedDatum {
  def apply(scope: Scope, datum: ast.Datum): ScopedDatum = {
    val scopedDatum = datum match {
      case ast.Pair(car, cdr) =>           Pair(ScopedDatum(scope, car), ScopedDatum(scope, cdr))
      case ast.Vector(elements) =>         Vector(elements.map(ScopedDatum.apply(scope, _)))
      case ast.Symbol(name) =>             Symbol(scope, name)
      case nonSymbol: ast.NonSymbolLeaf => NonSymbolLeaf(nonSymbol)
    }

    // Preserve our source location
    datum.assignLocationTo(scopedDatum)

    scopedDatum
  }
}

/** Matches symbols that resolve to a specific bound value
  *
  * This performs a hard resolve on any symbol it encounters; an UnboundVariableException will be raised for unbound
  * symbols.
  */
object ResolvedSymbol {
  def unapply(datum: ScopedDatum): Option[BoundValue] = datum match {
    case scopedSymbol: sst.Symbol =>
      Some(scopedSymbol.resolve)

    case _ =>
      None
  }
}

/** Matches either a list or a single datum
  *
  * A single datum is treated as the terminator of an otherwise empty improper list
  */
object ListOrDatum {
  def unapply(datum: ScopedDatum): Option[(List[ScopedDatum], ScopedDatum)] = datum match {
    case Pair(car, tail)  =>
      ListOrDatum.unapply(tail).map { case (head, terminator) =>
        (car :: head, terminator)
      }
    case nonPair => Some((Nil, nonPair))
  }
}

