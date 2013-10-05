package llambda.codegen

import llambda.{ast, nfi}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

sealed trait LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType]
  val boxedValue : Option[IrValue]
}

case class LiveString(constantValue : Option[String], boxedValue : Option[IrValue], utf8Pointer : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedString)
}

case class LiveSymbol(constantValue : Option[String], boxedValue : Option[IrValue], utf8Pointer : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedSymbol)
}

case class LiveExactInteger(constantValue : Option[Int], boxedValue : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedExactInteger)
}

case class LiveInexactRational(constantValue : Option[Double], boxedValue : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedInexactRational)
}

case class LiveBoolean(constantValue : Option[Boolean], boxedValue : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedBoolean)
}

case class LiveProcedure(signature : nfi.NativeSignature, boxedValue : Option[IrValue], functionPointer : IrValue) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedProcedure)
}

case class LiveCharacter(constantValue : Option[Char], boxedValue : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedCharacter)
}

case class LivePair(constantValue : Option[(LiveValue, LiveValue)], boxedValue : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedPair)
}

case class LiveVector(constantValue : Option[Vector[LiveValue]], boxedValue : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedVector)
}

case class LiveBytevector(constantValue : Option[Vector[Int]], boxedValue : Option[IrValue]) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedBytevector)
}

case object LiveUnspecific extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedUnspecific)
  val singletonValue = GlobalVariable("lliby_unspecific_value", PointerType(bt.BoxedUnspecific.irType))
  val boxedValue = Some(singletonValue)
}

case object LiveEmptyList extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedEmptyList)
  val singletonValue = GlobalVariable("lliby_empty_list_value", PointerType(bt.BoxedEmptyList.irType))
  val boxedValue = Some(singletonValue)
}

