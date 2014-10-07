package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

import llambda.compiler.codegen.RuntimeFunctions

import llambda.compiler.valuetype.Implicits._

object EquivalenceProcPlanner extends ReportProcPlanner {
  private def allSubtypes(rootType : ct.CellType) : Set[ct.CellType] =
    rootType.directSubtypes ++ rootType.directSubtypes.flatMap(allSubtypes)

  private lazy val preconstructedTypes =
    allSubtypes(ct.AnyCell).collect {
      case precons : ct.PreconstructedCellType =>
        vt.SchemeTypeAtom(precons)
    } : Set[vt.NonUnionSchemeType]

  // These can be tested for (equals?) with a simple pointer compare
  private lazy val ptrCompareEqualsTypes = (preconstructedTypes ++ Set(
    vt.ErrorObjectType,
    vt.PortType
  )) : Set[vt.NonUnionSchemeType]

  // These can be tested for (eqv?) with a simple pointer compare
  private lazy val ptrCompareEqvTypes = (ptrCompareEqualsTypes ++ Set(
    vt.AnyPairType,
    vt.VectorOfType(vt.AnySchemeType),
    vt.BytevectorType
  )) : Set[vt.NonUnionSchemeType]

  private def registerCond(state : PlannerState)(
    conditionValue : iv.IntermediateValue,
    subjectValue : iv.IntermediateValue,
    comparedValueType : vt.SchemeType
  ) : PlannerState = {
    val falseContraint = if (comparedValueType.isInstanceOf[vt.ConstantValueType]) {
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

  private def directCompareAsType(state : PlannerState)(
      valueType : vt.ValueType,
      val1 : iv.IntermediateValue,
      val2 : iv.IntermediateValue
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    val val1Temp = val1.toTempValue(valueType, convertProcType=false)
    val val2Temp = val2.toTempValue(valueType, convertProcType=false)

    val predicateTemp = ps.Temp(vt.Predicate)

    // Do a direct integer compare
    plan.steps += ps.IntegerCompare(predicateTemp, ps.CompareCond.Equal, None, val1Temp, val2Temp)

    new iv.NativePredicateValue(predicateTemp)
  }

  private def invokeCompare(state : PlannerState)(
      runtimeCompareSymbol : String,
      val1 : iv.IntermediateValue,
      val2 : iv.IntermediateValue)
  (implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    // (eqv?) etc don't invoke their arguments so we can skip the procedure type conversion
    val val1Temp = val1.toTempValue(vt.AnySchemeType, convertProcType=false)
    val val2Temp = val2.toTempValue(vt.AnySchemeType, convertProcType=false)

    val signature = RuntimeFunctions.equivalenceProcSignature
    val entryPointTemp = ps.EntryPointTemp()
    val resultTemp = ps.Temp(vt.Predicate)

    val invokeArgs = List(val1Temp, val2Temp).map(ps.InvokeArgument(_))

    plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, runtimeCompareSymbol)
    plan.steps += ps.Invoke(Some(resultTemp), signature, entryPointTemp, invokeArgs)

    new iv.NativePredicateValue(resultTemp)
  }
  
  private def planEquivalenceProc(state : PlannerState)(
      ptrCompareTypes : Set[vt.NonUnionSchemeType],
      runtimeCompareSymbol : String,
      val1 : iv.IntermediateValue,
      val2 : iv.IntermediateValue)
  (implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : PlanResult = {
    val ptrCompareUnion = vt.UnionType(ptrCompareTypes)
    
    val resultValue = if (plan.config.optimize) { 
      if ((vt.SatisfiesType(ptrCompareUnion, val1.schemeType) == Some(true)) ||
          (vt.SatisfiesType(ptrCompareUnion, val2.schemeType) == Some(true))) {
        // We can fast path this; the possible types for either value consist entirely of fast path types
        directCompareAsType(state)(vt.AnySchemeType, val1, val2)
      }
      else if (val1.hasDefiniteType(vt.ExactIntegerType) && 
               val2.hasDefiniteType(vt.ExactIntegerType)) {
        directCompareAsType(state)(vt.Int64, val1, val2)
      }
      else if (val1.hasDefiniteType(vt.CharType) && 
               val2.hasDefiniteType(vt.CharType)) {
        directCompareAsType(state)(vt.UnicodeChar, val1, val2)
      }
      else {
        // We need to invoke the runtime
        invokeCompare(state)(runtimeCompareSymbol, val1, val2)
      }
    }
    else {
      // Always call our runtime
      invokeCompare(state)(runtimeCompareSymbol, val1, val2)
    }

    // Register our type constraints for occurrence typing
    val val1RegisteredState = registerCond(state)(resultValue, val1, val2.schemeType)
    val val2RegisteredState = registerCond(val1RegisteredState)(resultValue, val2, val1.schemeType)

    PlanResult(
      state=val2RegisteredState,
      values=SingleValue(resultValue)
    )
  }

  override def planWithResult(state : PlannerState)(
      reportName : String,
      operands : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case (_, List((_, val1), (_, val2))) if List("eqv?", "eq?").contains(reportName) =>
      StaticValueEqv.valuesAreEqv(val1, val2).map { staticResult =>
        PlanResult(
          state=state,
          values=SingleValue(new iv.ConstantBooleanValue(staticResult))
        )
      } orElse {
        Some(planEquivalenceProc(state)(ptrCompareEqvTypes, RuntimeFunctions.isEqvSymbol, val1, val2))
      }
    
    case ("equal?", List((_, val1), (_, val2))) =>
      StaticValueEqv.valuesAreEqual(val1, val2).map { staticResult =>
        PlanResult(
          state=state,
          values=SingleValue(new iv.ConstantBooleanValue(staticResult))
        )
      } orElse {
        Some(planEquivalenceProc(state)(ptrCompareEqualsTypes, RuntimeFunctions.isEqualSymbol, val1, val2))
      }

    case _ =>
      None
  }
}
