package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

class LiveBoxedValue(boxedType : bt.BoxedType, boxedValue : IrValue) extends LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType] =
    (boxedType :: boxedType.subtypes).collect({
        case concrete : bt.ConcreteBoxedType => concrete
    }).toSet
  
  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)] = {
    targetType match {
      case nfi.BoxedValue(expectedType) =>
        if (expectedType.isTypeOrSupertypeOf(boxedType)) {
          // This doesn't require any casting
          val castValue = expectedType.genPointerBitcast(state.currentBlock)(boxedValue)

          Some((state, castValue))
        }
        else {
          None
        }
    }
  }
}
