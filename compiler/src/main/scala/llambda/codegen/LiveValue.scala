package llambda.codegen

import llambda.{ast, nfi}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

sealed trait LiveValue {
  val possibleTypes : Set[bt.BoxedType]
  val boxedValue : Option[IrValue]
}

case class LiveString(constantValue : Option[String], boxedValue : Option[IrValue], utf8Pointer : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.BoxedType](bt.BoxedString)
}

case class LiveProcedure(signature : nfi.NativeSignature, boxedValue : Option[IrValue], functionPointer : IrValue) extends LiveValue {
  val possibleTypes = Set[bt.BoxedType](bt.BoxedProcedure)
}

case object LiveUnspecific extends LiveValue {
  val possibleTypes = Set[bt.BoxedType](bt.BoxedUnspecific)
  val singletonValue = GlobalVariable("lliby_unspecific_value", PointerType(bt.BoxedUnspecific.irType))
  val boxedValue = Some(singletonValue)
}
