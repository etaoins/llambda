package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._

object EquivalenceProcPlanner extends ReportProcPlanner {
  private def allSubtypes(rootType : ct.CellType) : Set[ct.CellType] =
    rootType.directSubtypes ++ rootType.directSubtypes.flatMap(allSubtypes)

  private lazy val preconstructedTypes =
    allSubtypes(ct.DatumCell).collect {
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
    vt.PairType,
    vt.VectorType,
    vt.BytevectorType
  )) : Set[vt.NonUnionSchemeType]

  private def attemptStaticEqv(val1 : iv.IntermediateValue, val2 : iv.IntermediateValue) : Option[Boolean] = {
    if ((val1.schemeType & val2.schemeType) == vt.EmptySchemeType) {
      // Types are completely disjoint - they can't be equivalent
      return Some(false)
    }

    (val1, val2) match {
      case (constBool1 : iv.ConstantBooleanValue, constBool2 : iv.ConstantBooleanValue) =>
        Some(constBool1.value == constBool2.value)

      case (constSymbol1 : iv.ConstantSymbolValue, constSymbol2 : iv.ConstantSymbolValue) =>
        Some(constSymbol1.value == constSymbol2.value)
      
      case (constString1 : iv.ConstantStringValue, constString2 : iv.ConstantStringValue) =>
        Some(constString1.value == constString2.value)
      
      case (constInteger1 : iv.ConstantExactIntegerValue, constInteger2 : iv.ConstantExactIntegerValue) =>
        Some(constInteger1.value == constInteger2.value)
      
      case (constRational1 : iv.ConstantInexactRationalValue, constRational2 : iv.ConstantInexactRationalValue) =>
        Some(constRational1.value == constRational2.value)
      
      case (constCharacter1 : iv.ConstantCharacterValue, constCharacter2 : iv.ConstantCharacterValue) =>
        Some(constCharacter1.value == constCharacter2.value)
      
      case (iv.UnitValue, iv.UnitValue) =>
        Some(true)
      
      case (iv.EmptyListValue, iv.EmptyListValue) =>
        Some(true)

      case _ =>
        None
    }
  }
  
  private def elementsAreEqual(elems1 : Seq[iv.IntermediateValue], elems2 : Seq[iv.IntermediateValue]) : Option[Boolean] = {
    if (elems1.length != elems2.length) {
      // Nope
      return Some(false)
    }

    val equalityResult = elems1.zip(elems2).map { case (elem1, elem2) =>
      attemptStaticEqual(elem1, elem2)
    }

    if (equalityResult.contains(Some(false))) {
      // Definitely inequal
      Some(false)
    }
    else if (equalityResult.forall(_ == Some(true))) {
      // Definitely equal
      Some(true)
    }
    else {
      // Unknown
      None
    }
  }

  private def attemptStaticEqual(val1 : iv.IntermediateValue, val2 : iv.IntermediateValue) : Option[Boolean] = {
    (val1, val2) match {
      case (constPair1 : iv.ConstantPairValue, constPair2 : iv.ConstantPairValue) =>
       elementsAreEqual(List(constPair1.car, constPair1.cdr), List(constPair2.car, constPair2.cdr))

      case (constVector1 : iv.ConstantVectorValue, constVector2 : iv.ConstantVectorValue) =>
        elementsAreEqual(constVector1.elements, constVector2.elements)
      
      case (constBytevector1 : iv.ConstantBytevectorValue, constBytevector2 : iv.ConstantBytevectorValue) =>
        Some(constBytevector1.elements == constBytevector2.elements)

      case _ =>
        attemptStaticEqv(val1, val2)
    }
  }

  private def directCompareAsType(state : PlannerState)(valueType : vt.ValueType, val1 : iv.IntermediateValue, val2 : iv.IntermediateValue)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    val val1Temp = val1.toTempValue(valueType)
    val val2Temp = val2.toTempValue(valueType)

    val predicateTemp = ps.Temp(vt.Predicate)

    // Do a direct integer compare
    plan.steps += ps.IntegerCompare(predicateTemp, ps.CompareCond.Equal, None, val1Temp, val2Temp)

    Some(PlanResult(
      state=state,
      value=new iv.NativePredicateValue(predicateTemp)
    ))
  }
  
  private def planEquivalenceProc(state : PlannerState)(ptrCompareTypes : Set[vt.NonUnionSchemeType], val1 : iv.IntermediateValue, val2 : iv.IntermediateValue)(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = {
    val ptrCompareUnionType = vt.UnionType(ptrCompareTypes)
    
    if ((val1.schemeType.satisfiesType(ptrCompareUnionType) == Some(true)) ||
             (val2.schemeType.satisfiesType(ptrCompareUnionType) == Some(true))) {
      // We can fast path this?
      // If the pssible types for either value consists entirely of fast path types
      directCompareAsType(state)(vt.AnySchemeType, val1, val2)

    }
    else if (val1.hasDefiniteType(vt.ExactIntegerType) && 
             val2.hasDefiniteType(vt.ExactIntegerType)) {
      directCompareAsType(state)(vt.Int64, val1, val2)
    }
    else if (val1.hasDefiniteType(vt.CharacterType) && 
             val2.hasDefiniteType(vt.CharacterType)) {
      directCompareAsType(state)(vt.UnicodeChar, val1, val2)
    }
    else {
      None
    }
  }

  def apply(state : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[PlanResult] = (reportName, operands) match {
    case (_, List((_, val1), (_, val2))) if List("eqv?", "eq?").contains(reportName) =>
      attemptStaticEqv(val1, val2).map { staticResult =>
        PlanResult(
          state=state,
          value=new iv.ConstantBooleanValue(staticResult)
        )
      } orElse {
        planEquivalenceProc(state)(ptrCompareEqvTypes, val1, val2)
      }
    
    case ("equal?", List((_, val1), (_, val2))) =>
      attemptStaticEqual(val1, val2).map { staticResult =>
        PlanResult(
          state=state,
          value=new iv.ConstantBooleanValue(staticResult)
        )
      } orElse {
        planEquivalenceProc(state)(ptrCompareEqualsTypes, val1, val2)
      }

    case _ =>
      None
  }
}
