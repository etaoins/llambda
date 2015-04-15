package io.llambda.llvmir

import scala.io.Codec

sealed abstract class IrValue extends Irable {
  def irType : FirstClassType

  def toIrWithType = irType.toIr + " " + toIr
}

sealed abstract class IrConstant extends IrValue with MetadataOperand {
  def toMetadataIr = toIrWithType
}

case class LocalVariable(name : String, irType : FirstClassType) extends IrValue {
  def toIr = "%" + EscapeIdentifier(name)
}

// Global variables are constants because they're simply pointers to a
// possibly mutable value. The pointer itself is a link-time constant
case class GlobalVariable(name : String, irType : PointerType) extends IrConstant {
  def toIr = "@" + EscapeIdentifier(name)
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

case class StructureConstant(members : Seq[IrConstant], userDefinedType : Option[UserDefinedType] = None) extends IrConstant {
  def irType = userDefinedType getOrElse StructureType(members.map(_.irType))

  def toIr = "{" + members.map(_.toIrWithType).mkString(", ") + "}"
}

case class ArrayConstant(innerType : FirstClassType, members : Seq[IrConstant]) extends ArrayLikeConstant {
  val length = members.length

  def toIr = "[" + members.map(_.toIrWithType).mkString(", ") + "]"
}

case class StringConstant(stringBytes : Seq[Byte]) extends ArrayLikeConstant {
  val innerType = IntegerType(8) 
  val length = stringBytes.length

  def toIr = "c" + BytesToIrString(stringBytes) + "" 
}

object StringConstant {
  def fromUtf8String(str : String) : StringConstant = {
    // Convert to NULL terminated UTF-8
    StringConstant(Codec.toUTF8(str) :+ 0.toByte)
  }
}

case class ElementPointerConstant(elementType : FirstClassType, basePointer : IrConstant, indices : Seq[Int], inbounds : Boolean = false) extends IrConstant {
  if (!basePointer.irType.isInstanceOf[PointerType]) {
    throw new InconsistentIrException("Attempted to create a getelementptr constant from non-pointer")
  }

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

case class PtrToIntConstant(value : IrConstant, toType : IntegerType) extends IrConstant {
  if (!value.irType.isInstanceOf[PointerType]) {
    throw new InconsistentIrException("Attempted to create a ptrtoint constant from non-pointer")
  }

  def irType = toType

  def toIr : String = {
    s"ptrtoint (${value.toIrWithType} to ${toType.toIr})"
  }
}

case class IntToPtrConstant(value : IrConstant, toType : PointerType) extends IrConstant {
  if (!value.irType.isInstanceOf[IntegerType]) {
    throw new InconsistentIrException("Attempted to create a inttoptr constant from non-integer")
  }

  def irType = toType

  def toIr : String = {
    s"inttoptr (${value.toIrWithType} to ${toType.toIr})"
  }
}
