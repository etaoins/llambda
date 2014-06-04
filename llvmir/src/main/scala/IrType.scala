package io.llambda.llvmir

sealed abstract class IrType extends Irable

sealed trait ReturnableType extends IrType

// This seems to be everything but void
sealed abstract class FirstClassType extends IrType with ReturnableType

sealed abstract class FloatingPointType(val bits : Int) extends FirstClassType

case class UserDefinedType(name : String) extends FirstClassType {
  private val escapedName = EscapeIdentifier(name)

  def toIr = s"%${escapedName}"
}

case class IntegerType(bits : Int) extends FirstClassType {
  def toIr = s"i${bits}"
}

case object FloatType extends FloatingPointType(32) {
  def toIr = "float"
}

case object DoubleType extends FloatingPointType(64) {
  def toIr = "double"
}

case object VoidType extends IrType with ReturnableType {
  def toIr = "void"
}

case class ArrayType(elements : Int, innerType : FirstClassType) extends FirstClassType {
  def toIr = s"[$elements x $innerType]"
}

case class FunctionType(returnType : ReturnableType, parameterTypes : Seq[FirstClassType], hasVararg : Boolean = false) extends IrType {
  def toIr : String = {
    val parameterIrParts = parameterTypes.map(_.toIr) ++ (if (hasVararg) {
      List("...")
    }
    else {
      Nil
    })

    returnType + " (" + parameterIrParts.mkString(", ") + ")"
  }
}

case class StructureType(memberTypes : Seq[FirstClassType]) extends FirstClassType {
  def toIr = "{" + memberTypes.mkString(", ") + "}"
}

case class PointerType(pointeeType : IrType) extends FirstClassType {
  def toIr = pointeeType.toIr + "*"
}

case object MetadataType extends FirstClassType {
  def toIr = "metadata"
}
