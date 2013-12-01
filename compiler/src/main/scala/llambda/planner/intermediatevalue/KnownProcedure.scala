package llambda.planner.intermediatevalue

import llambda.nfi
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, InvokableProcedure}
import llambda.codegen.BoxedProcedureSignature
import llambda.NotImplementedException

class KnownProcedure(val signature : nfi.NativeSignature, val nativeSymbol : String, val reportName : Option[String] = None) extends IntermediateValue with InvokableProcedure with NonRecordValue {
  val possibleTypes = Set[bt.ConcreteBoxedType](bt.BoxedProcedure) 
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] = 
    Some(this)

  def toBoxedTempValue(targetType : bt.BoxedType)(implicit plan : PlanWriter) : Option[ps.TempValue] = {
    if (targetType.isTypeOrSupertypeOf(bt.BoxedProcedure)) {
      val entryPointTemp = if (signature == BoxedProcedureSignature) {
        // We can load this directly
        planEntryPoint()
      }
      else {
        val trampolineSymbol = "__llambda_" + nativeSymbol + "_trampoline"

        if (!plan.plannedFunctions.contains(trampolineSymbol)) {
          // Plan the trampoline
          plan.plannedFunctions += (trampolineSymbol -> PlanProcedureTrampoline(signature, nativeSymbol))
        }

        // Load the trampoline's entry point
        val trampEntryPointTemp = new ps.TempValue
        plan.steps += ps.StoreNamedEntryPoint(trampEntryPointTemp, BoxedProcedureSignature, trampolineSymbol) 

        trampEntryPointTemp
      }

      // Box the whole thing
      val tempAllocation = new ps.TempAllocation
      plan.steps += ps.AllocateCons(tempAllocation, 1)

      val boxedTemp = new ps.TempValue
      plan.steps += ps.BoxProcedure(boxedTemp, tempAllocation, 0, entryPointTemp)

      if (targetType != bt.BoxedProcedure) {
        // Cast this to super
        val castTemp = new ps.TempValue
        plan.steps += ps.CastBoxedToTypeUnchecked(castTemp, boxedTemp, targetType)

        Some(castTemp)
      }
      else {
        Some(boxedTemp)
      }
    }
    else {
      None
    }
  }
  
  def toScalarTempValue(unboxedType : nfi.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
    // Procedures have no unboxed representation
    None

  def withReportName(newReportName : String) : KnownProcedure = {
    new KnownProcedure(signature, nativeSymbol, Some(newReportName))
  }
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = new ps.TempValue
    plan.steps += ps.StoreNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

    entryPointTemp
  }

  def planClosure()(implicit plan : PlanWriter) : ps.TempValue = 
    throw new NotImplementedException("Closures not implemented")
}

