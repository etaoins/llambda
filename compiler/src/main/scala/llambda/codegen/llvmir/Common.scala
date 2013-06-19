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
  sealed abstract class Visibility extends Irable

  case object Default extends Visibility {
    def toIr = "default"
    override def toOptIr = None
  }

  case object Hidden extends Visibility {
    def toIr = "hidden"
  }

  case object Protected extends Visibility {
    def toIr = "protected"
  }
}

object Linkage {
  sealed abstract class Linkage extends Irable

  case object Private extends Linkage {
    def toIr = "private"
  }

  case object ExternallyAvailable extends Linkage {
    def toIr = "externally_available"
  }

  case object External extends Linkage {
    def toIr = "external"
    override def toOptIr = None
  }

  val Default = External
}

object CallingConv {
  sealed abstract class CallingConv extends Irable

  case object CCC extends CallingConv {
    def toIr = "ccc"
    override def toOptIr = None
  }

  case object FastCC extends CallingConv {
    def toIr = "fastcc"
  }

  case object ColdCC extends CallingConv {
    def toIr = "coldcc"
  }

  val Default = CCC
}
