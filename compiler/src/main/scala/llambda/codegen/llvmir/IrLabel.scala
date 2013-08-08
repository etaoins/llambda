package llambda.codegen.llvmir

private[llvmir] case class IrLabel(name : String) extends Irable {
  def toIr = "%" + name
}
