package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
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
  * @param selfTempOpt   For procedures with closures a procedure cell containing the procedure's closure. The entry
  *                      point does not have to be initialized; it will be set dynamically to a generated trampoline
  *                      if this value is explicitly converted to a ct.ProcedureCell
  */
abstract class KnownProc(selfTempOpt : Option[ps.TempValue]) extends IntermediateValue with BoxedOnlyValue with InvokableProcedure {
  val schemeType = vt.ProcedureType
  val typeDescription = "procedure"

  /** Signature for the procedure */
  val signature : ProcedureSignature

  /** Returns the native symbol for this function
    *
    * If the procedure is lazily planned it should be planned here
    */
  def nativeSymbol(implicit plan : PlanWriter) : String
  
  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue = {
    // Store an entry point with an adapted signature
    val entryPointTemp = if (signature == AdaptedProcedureSignature) {
      // The procedure already has the correct signature
      // This is unlikely but worth checking
      planEntryPoint()
    }
    else {
      // Give the trampoline a sufficently scary looking symbol
      val trampolineSymbol = "__llambda_" + nativeSymbol + "_trampoline"

      // Ensure this already hasn't been planned
      plan.plannedFunctions.getOrElseUpdate(trampolineSymbol, {
        // Plan the trampoline
        PlanProcedureTrampoline(signature, nativeSymbol)
      })

      // Load the trampoline's entry point
      val trampEntryPointTemp = ps.EntryPointTemp()
      plan.steps += ps.CreateNamedEntryPoint(trampEntryPointTemp, AdaptedProcedureSignature, trampolineSymbol) 

      trampEntryPointTemp
    }
    
    val cellTemp = selfTempOpt match {
      case Some(selfTemp) =>
        // Store the entry point in the procedure cell containing our closure data
        plan.steps += ps.SetProcedureEntryPoint(selfTemp, entryPointTemp)

        selfTemp

      case None =>
        // This must have an empty closure
        // If we had a closure selfTempOpt would have been defined to contain it
        // This means we have to create a new closureless procedure cell to contain the entry point
        val cellTemp = ps.CellTemp(ct.ProcedureCell)
        plan.steps += ps.CreateEmptyClosure(cellTemp, entryPointTemp)

        cellTemp
    }

    BoxedValue(ct.ProcedureCell, cellTemp)
  }
  
  def toSchemeTempValue(targetType : vt.SchemeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    if (hasDefiniteType(targetType)) {
      toBoxedValue().castToCellTempValue(targetType.cellType)
    }
    else {
      impossibleConversion(s"Cannot convert ${typeDescription} to non-procedure type ${targetType.schemeName}")
    }
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[InvokableProcedure] = 
    Some(this)
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

    entryPointTemp
  }

  def planSelf()(implicit plan : PlanWriter) : ps.TempValue = selfTempOpt getOrElse {
    throw new InternalCompilerErrorException("Attempted to get self value of a procedure without a closure")
  }
  
  def preferredRepresentation : vt.ValueType =
    vt.ProcedureType

  def needsClosureRepresentation  = 
    // We only need a closure if we have a closure ourselves (i.e. a self temp)
    selfTempOpt.isDefined
  
  /** Optionally plans an application of this procedure inline at the call site */
  def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] =
    None
}
