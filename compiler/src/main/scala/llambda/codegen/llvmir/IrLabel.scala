package llambda.codegen.llvmir

case class IrLabel(name : String) extends Irable {
  def toIr = "%" + name
}
