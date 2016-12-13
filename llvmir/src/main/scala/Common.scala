package io.llambda.llvmir

abstract trait Irable {
  /** Returns the IR representation of this domain object */
  def toIr: String

  /** Returns the IR representation of this domain object if it's a non-default
   * value or attribute */
  def toOptIr: Option[String] = Some(toIr)

  override def toString = toIr
}

sealed abstract class Visibility(val toIr: String) extends Irable
object Visibility {
  case object Hidden extends Visibility("hidden")
  case object Protected extends Visibility("protected")
  case object Default extends Visibility("default") {
    override def toOptIr = None
  }
}

sealed abstract class Linkage(val toIr: String) extends Irable
object Linkage {
  case object Private extends Linkage("private")
  case object Internal extends Linkage("internal")
  case object ExternallyAvailable extends Linkage("externally_available")
  case object External extends Linkage("external") {
    override def toOptIr = None
  }

  val Default = External
}

sealed abstract class CallingConv(val toIr: String) extends Irable
object CallingConv {
  case object FastCC extends CallingConv("fastcc")
  case object ColdCC extends CallingConv("coldcc")
  case object CCC extends CallingConv("ccc") {
    override def toOptIr = None
  }

  val Default = CCC
}
