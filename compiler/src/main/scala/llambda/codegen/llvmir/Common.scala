package llambda.codegen.llvmir

abstract trait Irable {
  /** Returns the IR representation of this domain object */
  def toIr : String

  /** Returns the IR representation of this domain object if it's a non-default
   * value or attribute */
  def toOptIr : Option[String] = Some(toIr)

  override def toString = toIr
}

object Visibility {
  sealed abstract class Visibility(val toIr : String) extends Irable

  case object Hidden extends Visibility("hidden")
  case object Protected extends Visibility("protected")
  case object Default extends Visibility("default") {
    override def toOptIr = None
  }
}

object Linkage {
  sealed abstract class Linkage(val toIr : String) extends Irable

  case object Private extends Linkage("private")
  case object ExternallyAvailable extends Linkage("externally_available")
  case object External extends Linkage("external") {
    override def toOptIr = None
  }

  val Default = External
}

object CallingConv {
  sealed abstract class CallingConv(val toIr : String) extends Irable

  case object FastCC extends CallingConv("fastcc")
  case object ColdCC extends CallingConv("coldcc")
  case object CCC extends CallingConv("ccc") {
    override def toOptIr = None
  }

  val Default = CCC
}
