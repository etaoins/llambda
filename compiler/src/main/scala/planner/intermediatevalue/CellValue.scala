package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.typecheck
import llambda.compiler.planner._
import llambda.compiler.{InternalCompilerErrorException, ValueNotApplicableException}
import llambda.compiler.{ErrorCategory, RuntimeErrorMessage}

/** Represents a value boxed in an alloc cell
  *
  * @param  schemeType      Scheme type of the CellValue. For types that are accessible from Scheme code this must be a
  *                         stable type
  * @param  boxedValue      BoxedValue containing the value's TempValue and cell type
  */
class CellValue(
    val schemeType: vt.SchemeType,
    val boxedValue: BoxedValue
) extends IntermediateValue {
  lazy val typeDescription = s"cell of type ${schemeType}"

  override def toTruthyPredicate()(implicit plan: PlanWriter): ps.TempValue = {
    // Find out if we're false
    val isFalseResult = typecheck.PlanTypeCheck(boxedValue, schemeType, vt.LiteralBooleanType(false))
    val isFalsePred = isFalseResult.toNativePred()

    // Invert the result
    val constantZeroPred = ps.Temp(vt.Predicate)
    plan.steps += ps.CreateNativeInteger(constantZeroPred, 0, vt.Predicate.bits)

    val truthyPred = ps.Temp(vt.Predicate)
    plan.steps += ps.IntegerCompare(truthyPred, ps.CompareCond.Equal, None, isFalsePred, constantZeroPred)
    truthyPred
  }

  def toBoxedValue()(implicit plan: PlanWriter): BoxedValue =
    boxedValue

  def toInvokableProc()(implicit plan: PlanWriter): InvokableProc =  {
    schemeType.applicableTypeOpt match {
      case Some(procedureType) =>
        val boxedProcTemp = toProcedureTempValue(procedureType, None)
        new BoxedProcCell(procedureType, boxedProcTemp)

      case None =>
        // This is definitely not applicable
        throw new ValueNotApplicableException(plan.activeContextLocated, typeDescription)
    }
  }

  def toProcedureTempValue(
      targetType: vt.ApplicableType,
      errorMessageOpt: Option[RuntimeErrorMessage]
  )(implicit plan: PlanWriter): ps.TempValue = {
    val applicableType = schemeType.applicableTypeOpt getOrElse {
      val message = errorMessageOpt.map(_.text) getOrElse {
        s"Unable to convert ${typeDescription} to ${targetType}"
      }

      impossibleConversion(message)
    }

    val currentSignature = ApplicableTypeToAdaptedSignature(applicableType)
    val requiredSignature = ApplicableTypeToAdaptedSignature(targetType)

    if (SatisfiesSignature(requiredSignature, currentSignature)) {
      // We already have the correct type
      return toNonProcedureTempValue(vt.SchemeTypeAtom(ct.ProcedureCell), errorMessageOpt)
    }

    // Make sure our types are sane
    if (vt.SatisfiesType(targetType, schemeType) == Some(false)) {
      impossibleConversionToType(targetType, errorMessageOpt, false)
    }

    // Ensure we're a procedure
    val procedureTypeAtom = vt.SchemeTypeAtom(ct.ProcedureCell)

    if (vt.SatisfiesType(procedureTypeAtom, schemeType) != Some(true)) {
      val errorMessage = errorMessageOpt getOrElse {
        RuntimeErrorMessage(
          category=ErrorCategory.Type,
          name=s"subcastTo${vt.NameForType(targetType)}Failed",
          text=s"Union typed value did not have a procedure type while attempting to convert value to procedure"
        )
      }

      val isProcPred = typecheck.PlanTypeCheck(boxedValue, schemeType, procedureTypeAtom).toNativePred()
      plan.steps += ps.AssertPredicate(isProcPred, errorMessage)
    }

    // Prepare a trampoline for this procedure conversion
    val targetProcTemp = boxedValue.castToCellTempValue(ct.ProcedureCell)
    val invokableTarget = new BoxedProcCell(applicableType, targetProcTemp)

    val trampolineKey = (invokableTarget.signature, requiredSignature)
    val trampolineSymbol = plan.adapterProcTrampolines.getOrElseUpdate(trampolineKey, {
      val trampolineSymbol = plan.allocSymbol(applicableType + " to " + targetType + " Adapter")

      // Plan the trampoline
      val plannedTrampoline = PlanProcedureTrampoline(requiredSignature, invokableTarget)
      plan.plannedFunctions += trampolineSymbol -> plannedTrampoline

      trampolineSymbol
    })

    val trampEntryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.CreateNamedEntryPoint(trampEntryPointTemp, requiredSignature, trampolineSymbol)

    // Create the adapter procedure cell
    val adapterProcTemp = ps.CellTemp(ct.ProcedureCell)

    val adapterFields = Map[vt.RecordField, ps.TempValue](AdapterProcField -> targetProcTemp)
    plan.steps += ps.InitProcedure(adapterProcTemp, AdapterProcType, trampEntryPointTemp, adapterFields)

    adapterProcTemp
  }

  def toNativeTempValue(nativeType: vt.NativeType, errorMessageOpt: Option[RuntimeErrorMessage])(implicit plan: PlanWriter): ps.TempValue = nativeType match {
    case vt.UnicodeChar =>
      val boxedChar = toTempValue(vt.CharType)
      val unboxedTemp = ps.Temp(vt.UnicodeChar)
      plan.steps += ps.UnboxChar(unboxedTemp, boxedChar)

      unboxedTemp

    case intType: vt.IntType =>
      val boxedInt = toTempValue(vt.IntegerType)
      val unboxedTemp = ps.Temp(vt.Int64)
      plan.steps += ps.UnboxInteger(unboxedTemp, boxedInt)

      if (intType.bits == 64) {
        // Correct width
        unboxedTemp
      }
      else {
        AssertIntInRange(unboxedTemp, vt.Int64, intType, evidenceOpt=Some(boxedInt))()

        // Convert to the right width
        val convTemp = ps.Temp(intType)
        plan.steps += ps.ConvertNativeInteger(convTemp, unboxedTemp, intType.bits, intType.signed)
        convTemp
      }

    case fpType: vt.FpType =>
      if (vt.SatisfiesType(vt.FlonumType, schemeType) == Some(false)) {
        // Not possible
        impossibleConversion(s"Unable to convert non-flonum ${typeDescription} to ${vt.NameForType(fpType)}")
      }

      // Unbox as flonum
      val boxedFlonum = toTempValue(vt.FlonumType)
      val unboxedTemp = ps.Temp(vt.Double)
      plan.steps += ps.UnboxFlonum(unboxedTemp, boxedFlonum)

      if (fpType == vt.Double) {
        // No conversion needed
        unboxedTemp
      }
      else {
        val convTemp = ps.Temp(fpType)

        plan.steps += ps.ConvertNativeFloat(convTemp, unboxedTemp, fpType)
        convTemp
      }

    case vt.Predicate =>
      throw new InternalCompilerErrorException("Attempt to directly convert to native boolean. Should be caught by toTruthyPredicate.")
  }

  override def withSchemeType(newType: vt.SchemeType): IntermediateValue = newType match {
    case literalType: vt.LiteralValueType =>
      IntermediateValue.fromLiteralType(literalType)

    case _ =>
      new CellValue(newType, boxedValue)
  }

  def preferredRepresentation: vt.ValueType =
    schemeType

  def needsClosureRepresentation =
    true
}

