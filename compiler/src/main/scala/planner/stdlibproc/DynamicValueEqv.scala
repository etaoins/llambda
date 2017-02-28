package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._
import llambda.compiler.codegen.RuntimeFunctions


private[stdlibproc] object DynamicValueEqv {
  type EqvFunction = (PlannerState) => (iv.IntermediateValue, iv.IntermediateValue) => (PlanWriter) => PlanResult

  private def allSubtypes(rootType: ct.CellType): Set[ct.CellType] =
    rootType.directSubtypes ++ rootType.directSubtypes.flatMap(allSubtypes)

  private lazy val preconstructedTypes =
    allSubtypes(ct.AnyCell).collect {
      case precons: ct.PreconstructedCellType =>
        vt.SchemeTypeAtom(precons)
    }: Set[vt.NonUnionSchemeType]

  // These can be tested for (equals?) with a simple pointer compare
  private lazy val ptrCompareEqualsTypes = preconstructedTypes ++ (Set(
    vt.ErrorObjectType,
    vt.PortType
  ): Set[vt.NonUnionSchemeType])

  // These can be tested for (eqv?) with a simple pointer compare
  private lazy val ptrCompareEqvTypes = (ptrCompareEqualsTypes ++ Set(
    vt.AnyPairType,
    vt.VectorType,
    vt.BytevectorType
  )): Set[vt.NonUnionSchemeType]

  private def registerCond(state: PlannerState)(
    conditionValue: iv.IntermediateValue,
    subjectValue: iv.IntermediateValue,
    comparedValueType: vt.SchemeType
  ): PlannerState = {
    val falseContraint = if (comparedValueType.isInstanceOf[vt.LiteralValueType]) {
      // This is an exact value - we can safely subtract it
      ConstrainType.SubtractType(comparedValueType)
    }
    else {
      // Just because the values aren't equal doesn't mean their types aren't
      ConstrainType.PreserveType
    }

    ConstrainType.addCondAction(state)(
      conditionValue=conditionValue,
      ConstrainType.CondAction(
        subjectValue=subjectValue,
        trueConstraint=ConstrainType.IntersectType(comparedValueType),
        falseConstraint=falseContraint
      )
    )
  }

  private def directCompareAsType(
      valueType: vt.ValueType,
      val1: iv.IntermediateValue,
      val2: iv.IntermediateValue
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    val val1Temp = val1.toTempValue(valueType, convertProcType=false)
    val val2Temp = val2.toTempValue(valueType, convertProcType=false)

    val predicateTemp = ps.TempValue()

    // Do a direct integer compare
    plan.steps += ps.IntegerCompare(predicateTemp, ps.CompareCond.Equal, None, val1Temp, val2Temp)

    new iv.NativePredicateValue(predicateTemp)
  }

  private def flonumCompare(
      staticValue: Double,
      dynamicValue: iv.IntermediateValue
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    val resultPred = ps.TempValue()

    if (staticValue.isNaN) {
      val dynamicTemp = dynamicValue.toTempValue(vt.Double)
      plan.steps += ps.FloatIsNaN(resultPred, dynamicTemp)
    }
    else {
      // Create the static double
      val staticTemp = ps.TempValue()
      plan.steps += ps.CreateNativeFloat(staticTemp, staticValue, vt.Double)

      // Create the dynamic double
      val dynamicTemp = dynamicValue.toTempValue(vt.Double)

      if (staticValue == 0.0) {
        // Compare the values bitwise to distinguish positive and negative zero
        plan.steps += ps.FloatBitwiseCompare(resultPred, staticTemp, dynamicTemp)
      }
      else {
        plan.steps += ps.FloatCompare(resultPred, ps.CompareCond.Equal, staticTemp, dynamicTemp)
      }
    }

    new iv.NativePredicateValue(resultPred)
  }

  private def invokeCompare(
      runtimeCompareSymbol: String,
      val1: iv.IntermediateValue,
      val2: iv.IntermediateValue)
  (implicit plan: PlanWriter): iv.IntermediateValue = {
    // (eqv?) etc don't invoke their arguments so we can skip the procedure type conversion
    val val1Temp = val1.toTempValue(vt.AnySchemeType, convertProcType=false)
    val val2Temp = val2.toTempValue(vt.AnySchemeType, convertProcType=false)

    val signature = RuntimeFunctions.equivalenceProcSignature
    val entryPointTemp = ps.TempValue()
    val resultTemp = ps.TempValue()

    val invokeArgs = List(val1Temp, val2Temp)

    plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, runtimeCompareSymbol)
    plan.steps += ps.Invoke(Some(resultTemp), signature, entryPointTemp, invokeArgs)

    new iv.NativePredicateValue(resultTemp)
  }

  private def planEquivalenceProc(state: PlannerState)(
      ptrCompareTypes: Set[vt.NonUnionSchemeType],
      runtimeCompareSymbol: String,
      val1: iv.IntermediateValue,
      val2: iv.IntermediateValue)
  (implicit plan: PlanWriter): PlanResult = {
    val ptrCompareUnion = vt.UnionType(ptrCompareTypes)

    val resultValue = if (plan.config.optimise) {
      (val1, val2) match {
        case (pred: iv.NativePredicateValue, iv.ConstantBooleanValue(true)) =>
          pred

        case (iv.ConstantBooleanValue(true), pred: iv.NativePredicateValue) =>
          pred

        case (_: iv.NativePredicateValue, _: iv.NativePredicateValue) |
             (_: iv.ConstantBooleanValue, _: iv.NativePredicateValue) |
             (_: iv.NativePredicateValue, _: iv.ConstantBooleanValue) =>
          directCompareAsType(vt.Predicate, val1, val2)

        case _ if (vt.SatisfiesType(ptrCompareUnion, val1.schemeType) == Some(true)) ||
                  (vt.SatisfiesType(ptrCompareUnion, val2.schemeType) == Some(true)) =>
          // We can fast path this; the possible types for either value consist entirely of fast path types
          directCompareAsType(vt.AnySchemeType, val1, val2)

        case _ if val1.hasDefiniteType(vt.IntegerType) && val2.hasDefiniteType(vt.IntegerType) =>
          directCompareAsType(vt.Int64, val1, val2)

        case _ if val1.hasDefiniteType(vt.CharType) && val2.hasDefiniteType(vt.CharType) =>
          directCompareAsType(vt.UnicodeChar, val1, val2)

        case _ if (val1.hasDefiniteType(vt.SymbolType) && val2.hasDefiniteType(vt.SymbolType)) =>
          val resultPred = PlanSymbolEquality.compareDynamic(val1, val2)
          new iv.NativePredicateValue(resultPred)

        case (iv.ConstantFlonumValue(staticVal1), dynamic2) if dynamic2.hasDefiniteType(vt.FlonumType) =>
          flonumCompare(staticVal1, dynamic2)

        case (dynamic1, iv.ConstantFlonumValue(staticVal2)) if dynamic1.hasDefiniteType(vt.FlonumType) =>
          flonumCompare(staticVal2, dynamic1)

        case _ =>
          // We need to invoke the runtime
          invokeCompare(runtimeCompareSymbol, val1, val2)
      }
    }
    else {
      // Always call our runtime
      invokeCompare(runtimeCompareSymbol, val1, val2)
    }

    // Register our type constraints for occurrence typing
    val val1RegisteredState = registerCond(state)(resultValue, val1, val2.schemeType)
    val val2RegisteredState = registerCond(val1RegisteredState)(resultValue, val2, val1.schemeType)

    PlanResult(
      state=val2RegisteredState,
      value=resultValue
    )
  }

  def valuesAreEqv(state: PlannerState)(
      val1: iv.IntermediateValue,
      val2: iv.IntermediateValue
  )(plan: PlanWriter): PlanResult =
    planEquivalenceProc(state)(ptrCompareEqvTypes, RuntimeFunctions.isEqvSymbol, val1, val2)(plan)

  def valuesAreEqual(state: PlannerState)(
      val1: iv.IntermediateValue,
      val2: iv.IntermediateValue
  )(plan: PlanWriter): PlanResult =
    planEquivalenceProc(state)(ptrCompareEqualsTypes, RuntimeFunctions.isEqualSymbol, val1, val2)(plan)
}
