package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._
import llambda.compiler.{celltype => ct}
import llambda.compiler.RuntimeErrorMessage
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.ProcedureSignature
import llambda.compiler.ImpossibleTypeConversionException
import llambda.compiler.InternalCompilerErrorException

trait IntermediateValueHelpers {
  def typeDescription : String

  /** Helper for signalling impossible conversions */
  protected def impossibleConversion(message : String)(implicit plan : PlanWriter) = { 
    throw new ImpossibleTypeConversionException(plan.activeContextLocated, message)
  }

  protected def impossibleConversionToType(
      targetType : vt.SchemeType,
      errorMessageOpt : Option[RuntimeErrorMessage],
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter) = if (staticCheck) {
    val message = errorMessageOpt.map(_.text) getOrElse {
      s"${typeDescription} does not statically satisfy ${vt.NameForType(targetType)}"
    }

    impossibleConversion(message) 
  }
  else {
    val message = errorMessageOpt.map(_.text) getOrElse {
      s"Unable to convert ${typeDescription} to ${vt.NameForType(targetType)}"
    }

    impossibleConversion(message) 
  }
}

abstract class IntermediateValue extends IntermediateValueHelpers {
  val schemeType : vt.SchemeType
  
  /** Provides a human-readable description of the value's type */
  def typeDescription : String

  /** Returns true is this value definitely has the passed type */
  def hasDefiniteType(otherType : vt.SchemeType) : Boolean =
    vt.SatisfiesType(otherType, schemeType) == Some(true)

  def isDefiniteProperList : Boolean =
    vt.SatisfiesType(vt.UniformProperListType(vt.AnySchemeType), schemeType) == Some(true)

  /** Returns our exact procedure signature */
  def procedureSignatureOpt : Option[ProcedureSignature] = 
    schemeType.applicableTypeOpt.map(ApplicableTypeToAdaptedSignature)

  protected def toNativeTempValue(nativeType : vt.NativeType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue
  protected def toTruthyPredicate()(implicit plan : PlanWriter) : ps.TempValue = {
    val trueTemp = ps.Temp(vt.Predicate)
    plan.steps += ps.CreateNativeInteger(trueTemp, 1, 1) 

   trueTemp
  }
  
  protected def toProcedureTempValue(
      targetType : vt.ApplicableType,
      errorMessageOpt : Option[RuntimeErrorMessage],
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue
  
  private def toProcedureTypeUnionTempValue(
      targetType : vt.SchemeType,
      targetApplicableType : vt.ApplicableType,
      errorMessageOpt : Option[RuntimeErrorMessage],
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    // This is a union containing different procedure types
    val resultCellType = targetType.cellType
    val procTypeAtom = vt.SchemeTypeAtom(ct.ProcedureCell)

    val createProcTemp = { isProcPlan : PlanWriter =>
      val knownType = schemeType & procTypeAtom
      val retypedThis = this.withSchemeType(knownType)

      val procTemp = toProcedureTempValue(
        targetApplicableType,
        errorMessageOpt,
        staticCheck
      )(isProcPlan, worldPtr)

      val castTemp = ps.CellTemp(resultCellType)
      isProcPlan.steps += ps.CastCellToTypeUnchecked(castTemp, procTemp, resultCellType)
      castTemp
    }

    val createNonProcTemp = { isNotProcPlan : PlanWriter =>
      val knownType = schemeType - procTypeAtom
      val retypedThis = this.withSchemeType(knownType)

      val nonProcTargetType = targetType - procTypeAtom
      val nonProcTemp = retypedThis.toNonProcedureTempValue(
        nonProcTargetType,
        errorMessageOpt,
        staticCheck
      )(isNotProcPlan, worldPtr)

      val castTemp = ps.CellTemp(resultCellType)
      isNotProcPlan.steps += ps.CastCellToTypeUnchecked(castTemp, nonProcTemp, resultCellType)
      castTemp
    }

    vt.SatisfiesType(procTypeAtom, schemeType) match {
      case Some(true) =>
        createProcTemp(plan)

      case Some(false) =>
        createNonProcTemp(plan)
        
      case None =>
        // Branch depending on if this is a proc or not
        val boxedValue = this.toBoxedValue()
        val isProcPred = typecheck.PlanTypeCheck(boxedValue, schemeType, procTypeAtom).toNativePred()

        plan.buildCondBranch(isProcPred, createProcTemp, createNonProcTemp)
    }
  }
  
  protected def toNonProcedureTempValue(
      targetType : vt.SchemeType,
      errorMessageOpt : Option[RuntimeErrorMessage],
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    // Are our possible concrete types a subset of the target types?
    vt.SatisfiesType(targetType, schemeType) match {
      case Some(true) =>
        // Need to cast to the right type
        // We've confirmed that no checking is needed because all of our possible types are equal to or supertypes of the
        // target type
        toBoxedValue().castToCellTempValue(targetType.cellType)

      case None if staticCheck =>
        impossibleConversionToType(targetType, errorMessageOpt, true)
    
      case None if !staticCheck =>
        val errorMessage = errorMessageOpt getOrElse {
          RuntimeErrorMessage(
            name=s"subcastTo${vt.NameForType(targetType)}Failed",
            text=s"Runtime cast to subtype '${vt.NameForType(targetType)}' failed"
          )
        }

        // We have further type checking to do
        val boxedValue = this.toBoxedValue()
        val isTypePred = typecheck.PlanTypeCheck(boxedValue, schemeType, targetType).toNativePred()
            
        plan.steps += ps.AssertPredicate(worldPtr, isTypePred, errorMessage, evidenceOpt=Some(boxedValue.tempValue))
        boxedValue.castToCellTempValue(targetType.cellType)

      case Some(false) =>
        // Not possible
        impossibleConversionToType(targetType, errorMessageOpt, false)
    }
  }

  /** Converts this value to any boxed cell value
    *
    * This is primarily used to interface with the type checking system which works on boxed values only
    */
  def toBoxedValue()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : BoxedValue

  def toInvokableProcedure()(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : InvokableProcedure

  /** Converts this intermediate value to a TempValue of the specified type
    *
    * @param  targetType       Target Scheme type to convert the value to
    * @param  errorMessageOpt  Error message to used to indicate type conversion failure. If this is not provided a
    *                          default message will be generated.
    * @param  staticCheck      If true then the type must be satisfied at compile time. Otherwise a runtime check will
    *                          be generated for possible but not definite type conversions.
    * @param  convertProcType  Indicates if the signature of the procedure should be adapted to match the signature
    *                          expected Scheme type. This may cause expensive type check to be generated or an adapter
    *                          procedure to be allocated. When the result is known to be non-invokable (for example in
    *                          a parameterize) or used to restore the original value (using withSelfTemp) this should
    *                          be skipped.
    */
  def toTempValue(
      targetType : vt.ValueType,
      errorMessageOpt : Option[RuntimeErrorMessage] = None,
      staticCheck : Boolean = false,
      convertProcType : Boolean = true
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = targetType match {
    case vt.Predicate =>
      toTruthyPredicate()

    case nativeType : vt.NativeType =>
      toNativeTempValue(nativeType, errorMessageOpt)

    case procedureType : vt.ProcedureType if convertProcType  =>
      toProcedureTempValue(procedureType, errorMessageOpt, staticCheck)

    case targetSchemeType : vt.SchemeType =>
      for(ourSignature <- procedureSignatureOpt;
          targetProcType <- targetSchemeType.applicableTypeOpt) {
        val targetSignature = ApplicableTypeToAdaptedSignature(targetProcType)

        if (convertProcType && !SatisfiesSignature(targetSignature, ourSignature)) {
          // Need to perform procedure type conversion
          return toProcedureTypeUnionTempValue(targetSchemeType, targetProcType, errorMessageOpt, staticCheck)
        }
      }

      toNonProcedureTempValue(targetSchemeType, errorMessageOpt, staticCheck)

    case closureType : vt.ClosureType =>
      // Closure types are an internal implementation detail.
      // Nothing should attempt to convert to them
      throw new InternalCompilerErrorException("Attempt to convert value to a closure type")
  }
  
  /** Casts this value to the specified cell value type
    *
    * The result may not be of represented by the specified cell value type (e.g. it may be unboxed) but it is 
    * guaranteed to be convertable to that type. toTempValue should be used when a particular representation is 
    * explicitly required
    *
    * @param  targetType       Target Scheme type to convert the value to
    * @param  errorMessageOpt  Error message to used to indicate type conversion failure. If this is not provided a
    *                          default message will be generated.
    * @param  staticCheck      If true then the type must be satisfied at compile time. Otherwise a runtime check will
    *                          be  generated for possible but not definite type conversions.
    */
  def castToSchemeType(
      targetType : vt.SchemeType,
      errorMessageOpt : Option[RuntimeErrorMessage] = None,
      staticCheck : Boolean = false
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : IntermediateValue= {
    if (hasDefiniteType(targetType)) {
      // We don't need to do anything 
      return this
    }

    val castTemp = toTempValue(targetType, errorMessageOpt, staticCheck=staticCheck)
    TempValueToIntermediate(targetType, castTemp)(plan.config)
  }

  /** Returns this value with a new Scheme type
    *
    * The new type must completely satisfy the previous type - that is, the new type must be a subtype of the old type.
    *
    * This may not have an affect on all values. This is merely a type checking and optimisation hint to feed type
    * information the planner discovers back in to the value
    */
  def withSchemeType(newType : vt.SchemeType) : IntermediateValue = newType match {
    case vt.ConstantBooleanType(boolVal) =>
      new ConstantBooleanValue(boolVal)

    case _ =>
      this
  }
  
  /** Returns the preferred type to represent this value
    * 
    * For realized values this will be the type of the TempValue. For unrealized values such as constants and known
    * procedures this will be the type that can specifically represent the value with the minimum of boxing and
    * conversion.
    */
  def preferredRepresentation : vt.ValueType

  /** Indicates if this values need to be stored inside a closure
    *
    * If false is returned the value does not need to be captured and can instead be used directly without storing any
    * data in the closure.
    */
  def needsClosureRepresentation : Boolean

  /** Restores this value from a closure's ps.TempValue
    *
    * This can be overriden to carry value-specific metadata that isn't contained in the value's type alone. This can
    * include things like procedure signature, value ranges, etc.
    */
  def restoreFromClosure(valueType : vt.ValueType, varTemp : ps.TempValue)(planConfig : PlanConfig) : IntermediateValue = {
    TempValueToIntermediate(valueType, varTemp)(planConfig) 
  }
}

