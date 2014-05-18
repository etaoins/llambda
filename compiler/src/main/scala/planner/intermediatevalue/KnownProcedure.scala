package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.ProcedureSignature
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter, InvokableProcedure}
import llambda.compiler.codegen.AdaptedProcedureSignature
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.RuntimeErrorMessage

/** Represents a procedure with a known signature and direct entry point
  *
  * These procedures can be called directly without going through a costly trampoline. If this is converted to a
  * ct.ProcedureCell a trampoline will be dynamically built to give it an adapted signature. Adapted signatures are the
  * same for all procedures so they can be called without specific knowledge of the backing procedure. These adapted
  * procedure values are represented by InvokableProcedureCell
  *
  * @param signature     Signature of the procedure
  * @param symbolBlock   Function returning the native symbol of the direct entry point to the procedure
  * @param selfTempOpt   For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                      point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                      if this value is explicitly converted to a ct.ProcedureCell
  * @param reportName    Name of this procedure in R7RS. This is used as a tag to implement certain optimizations
  *                      elsewhere in the planner. It is not directly used by this class
  */
class KnownProcedure(val signature : ProcedureSignature, symbolBlock : () => String, selfTempOpt : Option[ps.TempValue], val reportName : Option[String] = None) extends IntermediateValue with InvokableProcedure with NonRecordValue {
  val possibleTypes = Set[ct.ConcreteCellType](ct.ProcedureCell) 

  // Only ask for this once
  lazy val nativeSymbol = symbolBlock()
  
  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[InvokableProcedure] = 
    Some(this)

  def toCellTempValue(targetType : ct.CellType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    if (targetType.isTypeOrSupertypeOf(ct.ProcedureCell)) {
      // Store an entry point with an adapted signature
      val entryPointTemp = if (signature == AdaptedProcedureSignature) {
        // The procedure already has the correct signature
        // This is unlikely but worth checking
        val entryPointTemp = ps.EntryPointTemp()
        plan.steps += ps.StoreNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

        entryPointTemp
      }
      else {
        // Give the trampoline a sufficently scary looking symbol
        val trampolineSymbol = "__llambda_" + nativeSymbol + "_trampoline"

        // Ensure this already hasn't been planned
        if (!plan.plannedFunctions.contains(trampolineSymbol)) {
          // Plan the trampoline
          plan.plannedFunctions += (trampolineSymbol -> PlanProcedureTrampoline(signature, nativeSymbol))
        }

        // Load the trampoline's entry point
        val trampEntryPointTemp = ps.EntryPointTemp()
        plan.steps += ps.StoreNamedEntryPoint(trampEntryPointTemp, AdaptedProcedureSignature, trampolineSymbol) 

        trampEntryPointTemp
      }
      
      val cellTemp = selfTempOpt match {
        case Some(selfTemp) =>
          // Store the entry point in the procedure cell containing our closure
          // data
          plan.steps += ps.SetProcedureEntryPoint(selfTemp, entryPointTemp)

          selfTemp

        case None =>
          // This must have an empty closure
          // If we had a closure selfTempOpt would have been defined to contain it
          // This means we have to create a new closureless procedure cell to
          // contain the entry point
          val cellTemp = ps.CellTemp(ct.ProcedureCell)

          plan.steps += ps.StoreEmptyClosure(cellTemp, entryPointTemp)

          cellTemp
      }
    
      cellTempToSupertype(cellTemp, ct.ProcedureCell, targetType)
    }
    else {
      impossibleConversion(s"Cannot convert procedure to non-procedure type ${targetType.schemeName}")
    }
  }
  
  def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue =
    // Procedures have no unboxed representation
    impossibleConversion(s"Cannot convert procedure to requested type ${nativeType.schemeName} or any other native type")

  def withReportName(newReportName : String) : KnownProcedure = {
    new KnownProcedure(signature, symbolBlock, selfTempOpt, Some(newReportName))
  }
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.StoreNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

    entryPointTemp
  }

  def planSelf()(implicit plan : PlanWriter) : ps.TempValue = selfTempOpt getOrElse {
    throw new InternalCompilerErrorException("Attempted to get self value of a procedure without a closure")
  }
  
  def preferredRepresentation : vt.ValueType =
    vt.IntrinsicCellType(ct.ProcedureCell)

  def closureRepresentation : Option[vt.ValueType] = 
    // We only need a closure if we have a closure ourselves (i.e. a self temp)
    selfTempOpt map { _ =>
      preferredRepresentation
    }
  
  override def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue) : IntermediateValue = {
    new KnownProcedure(signature, symbolBlock, Some(varTemp), reportName)
  }
}

