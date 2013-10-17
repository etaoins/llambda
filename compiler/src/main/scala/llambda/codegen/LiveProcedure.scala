package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.nfi
import llambda.codegen.llvmir._

class LiveProcedure(val signature : nfi.NativeSignature, val functionPointer : IrValue) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedProcedure)
  
  def genTruthyPredicate(state : GenerationState) : IrValue =
    IntegerConstant(IntegerType(1), 1)
  
  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)] =
    // Nothing supported yet
    None
}
