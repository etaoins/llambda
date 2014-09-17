package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{ProcedureSignature, ContextLocated}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.InternalCompilerErrorException
import llambda.compiler.{RuntimeErrorMessage, ContextLocated}

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
abstract class KnownProc(val signature : ProcedureSignature, selfTempOpt : Option[ps.TempValue]) extends IntermediateValue with BoxedOnlyValue with InvokableProcedure {
  val typeDescription = "procedure"

  final val schemeType = signature.toSchemeProcedureType

  /** Optional location of this procedure's definition
    *
    * This is used to generate a comment for the procedure's trampoline
    */
  def locationOpt : Option[ContextLocated] =
    None

  /** Returns the native symbol for this function
    *
    * If the procedure is lazily planned it should be planned here
    */
  def nativeSymbol(implicit plan : PlanWriter) : String

  def nativeSymbolOpt(implicit plan : PlanWriter) : Option[String] =
    Some(nativeSymbol)
  
  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue = {
    val procTemp = toProcedureTempValue(vt.TopProcedureType, None)
    BoxedValue(ct.ProcedureCell, procTemp)
  }
  
  def toProcedureTempValue(
      targetType : vt.SchemeType,
      errorMessageOpt : Option[RuntimeErrorMessage],
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    if (vt.SatisfiesType(targetType, schemeType) == Some(false)) {
      val message = s"Unable to convert ${typeDescription} to procedure type ${targetType}"
      impossibleConversion(message)
    }

    val requiredSignature = ProcedureTypeToAdaptedSignature(vt.TopProcedureType)

    // Store an entry point with an adapted signature
    val entryPointTemp = if (signature == requiredSignature) {
      // The procedure already has the correct signature
      // This is unlikely but worth checking
      planEntryPoint()
    }
    else {
      // Give the trampoline a sufficently scary looking symbol
      val trampolineSymbol = nativeSymbol + " Trampoline"

      // Ensure this already hasn't been planned
      plan.plannedFunctions.getOrElseUpdate(trampolineSymbol, {
        // Plan the trampoline
        PlanProcedureTrampoline(this, nativeSymbol, locationOpt)
      })

      // Load the trampoline's entry point
      val trampEntryPointTemp = ps.EntryPointTemp()
      plan.steps += ps.CreateNamedEntryPoint(trampEntryPointTemp, requiredSignature, trampolineSymbol) 

      trampEntryPointTemp
    }
    
    selfTempOpt match {
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
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : InvokableProcedure = 
    this
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

    entryPointTemp
  }

  def planSelf()(implicit plan : PlanWriter) : ps.TempValue = selfTempOpt getOrElse {
    throw new InternalCompilerErrorException("Attempted to get self value of a procedure without a closure")
  }
  
  def preferredRepresentation : vt.ValueType =
    schemeType

  def needsClosureRepresentation  = 
    // We only need a closure if we have a closure ourselves (i.e. a self temp)
    selfTempOpt.isDefined
  
  /** Optionally plans an application of this procedure inline at the call site */
  def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] =
    None

  override def withSchemeType(newType : vt.SchemeType) : KnownProc =
    this
  
  override def castToSchemeType(
      targetType : vt.SchemeType,
      errorMessageOpt : Option[RuntimeErrorMessage] = None,
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : IntermediateValue= {
    if (hasDefiniteType(targetType)) {
      // We don't need to do anything 
      return this
    }

    // Procedures cannot have their types tested at runtime
    impossibleConversion(s"${typeDescription} does not statically satisfy type ${targetType}")
  }
}
