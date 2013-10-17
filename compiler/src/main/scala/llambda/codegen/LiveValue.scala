package llambda.codegen

import llambda.{nfi, ImpossibleTypeConversionException}
import llambda.codegen.{boxedtype => bt}
import llambda.codegen.llvmir._

abstract class LiveValue {
  val possibleTypes : Set[bt.ConcreteBoxedType]

  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)]

  def genTruthyPredicate(state : GenerationState) : IrValue

  def toRequiredNativeType(state : GenerationState)(targetType : nfi.NativeType) : (GenerationState, IrValue) = {
    toNativeType(state)(targetType) getOrElse {
      throw new ImpossibleTypeConversionException("Unable to convert " + this.toString + " to " + targetType)
    }
  }
}

