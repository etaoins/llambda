package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{StepBuffer, InvokableProcedure}

class KnownProcedure(val signature : nfi.NativeSignature, nativeSymbol : String) extends IntermediateValue with InvokableProcedure {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedProcedure) 
  
  def toInvokableProcedure()(implicit planSteps : StepBuffer) : Option[InvokableProcedure] = 
    Some(this)

  def toBoxedTempValue(boxedType : bt.BoxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    None
  
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit planSteps : StepBuffer) : Option[ps.TempValue] =
    None
  
  def planEntryPoint()(implicit planSteps : StepBuffer) : ps.TempValue = {
    val entryPointTemp = new ps.TempValue
    planSteps += ps.StoreKnownEntryPoint(entryPointTemp, signature, nativeSymbol)

    entryPointTemp
  }
}

