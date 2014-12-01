package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{PolymorphicSignature, ContextLocated}
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
abstract class KnownProc(val polySignature : PolymorphicSignature, val selfTempOpt : Option[ps.TempValue]) extends IntermediateValue with BoxedOnlyValue with InvokableProcedure {
  val typeDescription = "procedure"

  val schemeType : vt.ApplicableType = polySignature.upperBound.toSchemeProcedureType

  /** Optional location of this procedure's definition
    *
    * This is used to generate a comment for the procedure's trampoline
    */
  def locationOpt : Option[ContextLocated] =
    None

  override def procedureSignatureOpt =
    Some(polySignature.upperBound)

  /** Returns the native symbol for this function
    *
    * If the procedure is lazily planned it should be planned here
    */
  def nativeSymbol(implicit plan : PlanWriter) : String

  def nativeSymbolOpt(implicit plan : PlanWriter) : Option[String] =
    Some(nativeSymbol)
  
  def toBoxedValue()(implicit plan : PlanWriter) : BoxedValue =
    BoxedValue(ct.ProcedureCell, planSelf())
  
  def toProcedureTempValue(
      targetType : vt.ApplicableType,
      errorMessageOpt : Option[RuntimeErrorMessage]
  )(implicit plan : PlanWriter) : ps.TempValue = {
    if (vt.SatisfiesType(targetType, schemeType) == Some(false)) {
      val message = s"Unable to convert ${typeDescription} to procedure type ${targetType}"
      impossibleConversion(message)
    }

    val requiredSignature = ApplicableTypeToAdaptedSignature(targetType)

    if (SatisfiesSignature(requiredSignature, polySignature.upperBound)) {
      // The procedure already has the correct signature - return our exisiting cell directly
      return toBoxedValue().tempValue
    }

    // Can we select a more specific polymorph or case lambda clause?
    targetType.signatures match {
      case List(vt.ProcedureType(fixedTypes, None, _)) =>
        val selectedPolymorph = toApplicableValueForOperands(fixedTypes)

        if (!(selectedPolymorph eq this)) {
          return selectedPolymorph.toProcedureTempValue(targetType, errorMessageOpt)
        }

      case _ =>
    }

    // Check if this symbol/signature combination has been planned
    val trampolineKey = (nativeSymbol, requiredSignature)
    val trampolineSymbol = plan.knownProcTrampolines.getOrElseUpdate(trampolineKey, {
      val trampolineSymbol = plan.allocSymbol(s"${nativeSymbol} ${targetType} Trampoline")

      // Plan the trampoline
      val plannedTrampoline = PlanProcedureTrampoline(requiredSignature, this, locationOpt)
      plan.plannedFunctions += trampolineSymbol -> plannedTrampoline

      trampolineSymbol
    })

    // Load the trampoline's entry point
    val trampEntryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.CreateNamedEntryPoint(trampEntryPointTemp, requiredSignature, trampolineSymbol) 

    // Create the adapter procedure cell
    val adapterProcTemp = ps.CellTemp(ct.ProcedureCell)

    selfTempOpt match {
      case Some(selfTemp) =>
        // Create a closure for the adapter pointing to our original closure
        val adapterDataTemp = ps.RecordLikeDataTemp()

        plan.steps += ps.InitRecordLike(adapterProcTemp, adapterDataTemp, AdapterProcType, false)
        plan.steps += ps.SetRecordDataField(adapterDataTemp, AdapterProcType, AdapterProcField, selfTemp)
        plan.steps += ps.SetProcedureEntryPoint(adapterProcTemp, trampEntryPointTemp)

      case None =>
        plan.steps += ps.CreateEmptyClosure(adapterProcTemp, trampEntryPointTemp)
    }

    adapterProcTemp
  }
  
  def toInvokableProcedure()(implicit plan : PlanWriter) : InvokableProcedure = 
    this
  
  def planEntryPoint()(implicit plan : PlanWriter) : ps.TempValue = {
    val entryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, polySignature.upperBound, nativeSymbol)

    entryPointTemp
  }

  def planSelf()(implicit plan : PlanWriter) : ps.TempValue =
    selfTempOpt match {
      case Some(selfTemp) => selfTemp

      case None =>
        val cellTemp = ps.CellTemp(ct.ProcedureCell)
        plan.steps += ps.CreateEmptyClosure(cellTemp, planEntryPoint())
        cellTemp
    }
  
  def preferredRepresentation : vt.ValueType =
    schemeType

  def needsClosureRepresentation  = 
    // We only need a closure if we have a closure ourselves (i.e. a self temp)
    selfTempOpt.isDefined
  
  /** Optionally plans an application of this procedure inline at the call site */
  def attemptInlineApplication(state : PlannerState)(operands : List[(ContextLocated, IntermediateValue)])(implicit plan : PlanWriter) : Option[PlanResult] =
    None

  override def withSchemeType(newType : vt.SchemeType) : KnownProc =
    this
  
  def withSelfTemp(selfValue : ps.TempValue) : KnownProc

  override def restoreFromClosure(
      valueType : vt.ValueType,
      varTemp : ps.TempValue
  )(planConfig : PlanConfig) : IntermediateValue = 
    withSelfTemp(varTemp)
}
