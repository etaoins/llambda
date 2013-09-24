/*****************************************************************
 * This file is generated by gen-types.py. Do not edit manually. *
 *****************************************************************/

package llambda.codegen.boxedtype

import llambda.codegen.llvmir.{UserDefinedType, PointerType}

sealed abstract class BoxedType {
  val irType : PointerType
}

object BoxedDatum extends BoxedType {
  val irType = PointerType(UserDefinedType("boxedDatum"))
}

object UnspecificValue extends BoxedType {
  val irType = PointerType(UserDefinedType("unspecific"))
}

object PairValue extends BoxedType {
  val irType = PointerType(UserDefinedType("pair"))
}

object EmptyListValue extends BoxedType {
  val irType = PointerType(UserDefinedType("emptyList"))
}

object StringLikeValue extends BoxedType {
  val irType = PointerType(UserDefinedType("stringLike"))
}

object StringValue extends BoxedType {
  val irType = PointerType(UserDefinedType("string"))
}

object SymbolValue extends BoxedType {
  val irType = PointerType(UserDefinedType("symbol"))
}

object BooleanValue extends BoxedType {
  val irType = PointerType(UserDefinedType("boolean"))
}

object NumericValue extends BoxedType {
  val irType = PointerType(UserDefinedType("numeric"))
}

object ExactIntegerValue extends BoxedType {
  val irType = PointerType(UserDefinedType("exactInteger"))
}

object InexactRationalValue extends BoxedType {
  val irType = PointerType(UserDefinedType("inexactRational"))
}

object CharacterValue extends BoxedType {
  val irType = PointerType(UserDefinedType("character"))
}

object ByteVectorValue extends BoxedType {
  val irType = PointerType(UserDefinedType("byteVector"))
}

object ProcedureValue extends BoxedType {
  val irType = PointerType(UserDefinedType("procedure"))
}

object VectorLikeValue extends BoxedType {
  val irType = PointerType(UserDefinedType("vectorLike"))
}

object VectorValue extends BoxedType {
  val irType = PointerType(UserDefinedType("vector"))
}

object ClosureValue extends BoxedType {
  val irType = PointerType(UserDefinedType("closure"))
}

