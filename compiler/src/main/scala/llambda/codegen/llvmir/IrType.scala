package llambda.codegen.llvmir

sealed abstract class IrType {
  def toIr : String
  override def toString = toIr
}

sealed trait ReturnableType

// This seems to be everything but void
sealed abstract class FirstClassType extends IrType with ReturnableType

sealed abstract class FloatingPointType extends FirstClassType

case class IntegerType(bits : Integer) extends FirstClassType {
  def toIr = s"i${bits}"
}

case object SingleType extends FloatingPointType {
  def toIr = "float"
}

case object DoubleType extends FloatingPointType {
  def toIr = "double"
}

case object VoidType extends IrType with ReturnableType {
  def toIr = "void"
}

case class ArrayType(elements : Integer, innerType : IrType) extends FirstClassType {
  def toIr = s"[$elements x $innerType]"
}

case class FunctionType(returnType : ReturnableType, parameterTypes : List[FirstClassType]) extends IrType {
  def toIr = returnType + " (" + parameterTypes.map(_.toIr).mkString(", ") + ")"
}

case class StructureType(memberTypes : List[FirstClassType]) extends FirstClassType {
  def toIr = "{" + memberTypes.mkString(", ") + "}"
}

case class PointerType(pointeeType : IrType) extends FirstClassType {
  def toIr = pointeeType match {
    // Leaving the space after the parameter list looks nicer
    case _ : FunctionType => pointeeType.toIr + " *"
    case _ => pointeeType.toIr + "*"
  }
}
