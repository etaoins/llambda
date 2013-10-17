package llambda.codegen.llvmir
import llambda.InternalCompilerErrorException

sealed abstract class IrValue extends Irable {
  def irType : FirstClassType

  def toIrWithType = irType.toIr + " " + toIr
}

sealed abstract class IrConstant extends IrValue

case class LocalVariable(name : String, irType : FirstClassType) extends IrValue {
  def toIr = "%" + name
}

// Global variables are constants because they're simply pointers to a
// possibly mutable value. The pointer itself is a link-time constant
case class GlobalVariable(name : String, irType : PointerType) extends IrConstant {
  def toIr = "@" + name
}

sealed abstract class BoolConstant extends IrConstant {
  def irType = IntegerType(1)
}

sealed abstract class FloatingPointConstant extends IrConstant {
  def IrType : FloatingPointType
}

sealed abstract class ArrayLikeConstant extends IrConstant {
  val innerType : FirstClassType
  val length : Int
    
  def irType = ArrayType(length, innerType)
}

case object TrueConstant extends BoolConstant {
  def toIr = "true"
}

case object FalseConstant extends BoolConstant {
  def toIr = "false"
}

case class IntegerConstant(irType : IntegerType, value : Long) extends IrConstant {
  def toIr = value.toString
}

case class FloatConstant(value : Float) extends IrConstant {
  def irType = FloatType

  def toIr = {
    if (value.isInfinity || value.isNaN) {
      "0x" + java.lang.Float.floatToIntBits(value).toHexString
    }
    else {
      value.toString
    }
  }
}

case class DoubleConstant(value : Double) extends IrConstant {
  def irType = DoubleType

  def toIr = {
    if (value.isInfinity || value.isNaN) {
      "0x" + java.lang.Double.doubleToLongBits(value).toHexString
    }
    else {
      value.toString
    }
  }
}

case class NullPointerConstant(irType : PointerType) extends IrConstant {
  def toIr = "null"
}

case class StructureConstant(members : List[IrConstant], userDefinedType : Option[UserDefinedType] = None) extends IrConstant {
  def irType = userDefinedType getOrElse StructureType(members.map(_.irType))

  def toIr = "{" + members.map(_.toIrWithType).mkString(", ") + "}"
}

case class ArrayConstant(innerType : FirstClassType , members : List[IrConstant]) extends ArrayLikeConstant {
  val length = members.length

  def toIr = "[" + members.map(_.toIrWithType).mkString(", ") + "]"
}

case class StringConstant(str : String) extends ArrayLikeConstant {
  // Always NULL terminate when passing to NFI
  private val utf8Bytes = io.Codec.toUTF8(str) :+ 0.toByte

  val innerType = IntegerType(8) 
  val length = utf8Bytes.length

  // String without "
  private def innerString : String = (utf8Bytes flatMap {
    case backslash if backslash == 92 =>
      """\\"""
    case doubleQuote if doubleQuote == 34 =>
      "\\\""
    case printable if ((printable >= 32) && (printable <= 126)) =>
      printable.toChar.toString
    case unprintable =>
      f"\\$unprintable%02X"
  }).mkString

  def toIr = "c\"" + innerString + "\"" 
}

case class ElementPointerConstant(elementType : FirstClassType, basePointer : GlobalVariable, indices : Seq[Integer], inbounds : Boolean = false) extends IrConstant {
  def irType = PointerType(elementType)

  def toIr : String = {
    val inboundsIr = if (inbounds) {
      " inbounds"
    }
    else {
      ""
    }

    val paramParts = basePointer.toIrWithType :: indices.toList.map("i32 " + _.toString)
    val paramIr = paramParts.mkString(", ")

    s"getelementptr${inboundsIr} (${paramIr})"
  }
}

case class BitcastToConstant(value : IrConstant, toType : FirstClassType) extends IrConstant {
  def irType = toType

  def toIr : String = {
    s"bitcast (${value.toIrWithType} to ${toType.toIr})"
  }
}
