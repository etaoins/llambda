package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, InvokableProcedure}

class KnownProcedure(val signature : nfi.NativeSignature, nativeSymbol : String) extends IntermediateValue with InvokableProcedure {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedProcedure) 
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] = 
    Some(this)

  def toBoxedTempValue(boxedType : bt.BoxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    None
  
  def toUnboxedTempValue(unboxedType : nfi.UnboxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    None
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = new ps.TempValue
    plan.steps += ps.StoreKnownEntryPoint(entryPointTemp, signature, nativeSymbol)

    entryPointTemp
  }
}

