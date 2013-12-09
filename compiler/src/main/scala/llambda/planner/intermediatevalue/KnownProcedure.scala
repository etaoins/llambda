package llambda.planner.intermediatevalue

import llambda.ProcedureSignature
import llambda.{celltype => ct}
import llambda.{valuetype => vt}
import llambda.planner.{step => ps}
import llambda.planner.{PlanWriter, InvokableProcedure}
import llambda.codegen.AdaptedProcedureSignature
import llambda.NotImplementedException

class KnownProcedure(val signature : ProcedureSignature, val nativeSymbol : String, val reportName : Option[String] = None) extends IntermediateValue with InvokableProcedure with NonRecordValue {
  val possibleTypes = Set[ct.ConcreteCellType](ct.ProcedureCell) 
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : Option[InvokableProcedure] = 
    Some(this)

  def toCellTempValue(targetType : ct.CellType)(implicit plan : PlanWriter) : Option[ps.TempValue] = {
    if (targetType.isTypeOrSupertypeOf(ct.ProcedureCell)) {
      val entryPointTemp = if (signature == AdaptedProcedureSignature) {
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
        plan.steps += ps.StoreNamedEntryPoint(trampEntryPointTemp, AdaptedProcedureSignature, trampolineSymbol) 

        trampEntryPointTemp
      }

      // Box the whole thing
      val tempAllocation = new ps.TempAllocation
      plan.steps += ps.AllocateCells(tempAllocation, 1)

      val boxedTemp = new ps.TempValue
      plan.steps += ps.BoxProcedure(boxedTemp, tempAllocation, 0, entryPointTemp)

      if (targetType != ct.ProcedureCell) {
        // Cast this to super
        val castTemp = new ps.TempValue
        plan.steps += ps.CastCellToTypeUnchecked(castTemp, boxedTemp, targetType)

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
  
  def toNativeTempValue(nativeType : vt.NativeType)(implicit plan : PlanWriter) : Option[ps.TempValue] =
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
  
  def preferredRepresentation : vt.ValueType =
    vt.IntrinsicCellType(ct.ProcedureCell)
}

