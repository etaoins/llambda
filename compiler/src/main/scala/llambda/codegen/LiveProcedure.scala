package llambda.codegen

import llambda.codegen.{boxedtype => bt}
import llambda.nfi
import llambda.codegen.llvmir._

class LiveProcedure(val signature : nfi.NativeSignature, val functionPointer : IrValue) extends LiveValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedProcedure)
  
  def toNativeType(state : GenerationState)(targetType : nfi.NativeType) : Option[(GenerationState, IrValue)] =
    // Nothing supported yet
    None
}
