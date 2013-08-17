package llambda.codegen.llvmir

private[llvmir] case class IrLabel(val name : String) extends Irable {
  def toIr = "%" + name
}
