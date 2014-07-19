package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}

trait KnownPair extends IntermediateValue {
  val car : IntermediateValue
  val cdr : IntermediateValue
  
  override lazy val isDefiniteProperList = listLengthOpt.isDefined

  /** Returns the length of the proper list this pair is a part of
    *
    * If this pair isn't part of a proper list or the length of the list can't be statically determined then this
    * will return None
    */
  lazy val listLengthOpt : Option[Long] =
    cdr match {
      case emptyList if emptyList.schemeType.satisfiesType(vt.EmptyListType) == Some(true) =>
        Some(1)

      case knownPair : KnownPair =>
        knownPair.listLengthOpt.map(_ + 1)

      case _ =>
        None
    }

}
