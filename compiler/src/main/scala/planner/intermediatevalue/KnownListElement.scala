package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}

trait KnownListElement extends IntermediateValue {
  /** Returns the length of the proper list this element is a part of
    *
    * If this element isn't part of a proper list or the length of the list can't be statically determined then this
    * will return None
    */
  def listLengthOpt : Option[Long]

  /** Returns the members of this list as IntermediateValues
    *
    * This will only be defined for lists that have statically known members at compile time
    */
  def toValueListOpt : Option[List[IntermediateValue]]
}

trait KnownPair extends KnownListElement {
  val car : IntermediateValue
  val cdr : IntermediateValue
  
  lazy val listLengthOpt : Option[Long] =
    cdr match {
      case knownListElement : KnownListElement =>
        knownListElement.listLengthOpt.map(_ + 1)

      case _ =>
        None
    }

  def toValueListOpt : Option[List[IntermediateValue]] = {
    cdr match {
      case knownListElement : KnownListElement =>
        knownListElement.toValueListOpt.map(car :: _)

      case _ =>
        None
    }
  }
}
