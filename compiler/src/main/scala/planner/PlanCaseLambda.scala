package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.et
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{RuntimeErrorMessage, ProcedureSignature, ProcedureAttribute}

import llambda.compiler.valuetype.Implicits._

private[planner] object PlanCaseLambda {
  val noMatchingClauseRuntimeErrorMessage = RuntimeErrorMessage(
    name="noMatchingCaseLambdaClause",
    text="No matching (case-lambda) clause"
  )

  private case class CapturedProc(
      outerProcTemp : ps.TempValue,
      recordField : vt.RecordField
  )

  private case class PlannedClause(
      procValue : iv.KnownSchemeProc,
      capturedProcOpt : Option[CapturedProc]
  )

  private def listOfAtLeast(length : Int) : vt.SchemeType = {
    val anyList = vt.UniformProperListType(vt.AnySchemeType) 

    (0 until length).foldRight(anyList : vt.SchemeType) {
      case (_, cdrType) =>
        vt.SpecificPairType(vt.AnySchemeType, cdrType)
    }
  }
  
  private def listOfExactly(length : Int) : vt.SchemeType = {
    (0 until length).foldRight(vt.EmptyListType : vt.SchemeType) {
      case (_, cdrType) =>
        vt.SpecificPairType(vt.AnySchemeType, cdrType)
    }
  }

  private def planClauseTests(
      plannedClauses : List[PlannedClause],
      innerSelfTempOpt : Option[ps.TempValue],
      closureType : vt.ClosureType,
      argListHeadTemp : ps.TempValue,
      argLengthTemp : ps.TempValue
  )(implicit entryPlan : PlanWriter) : ResultValues = plannedClauses match {
    case Nil =>
      // We were called with no clauses
      // Note that the checkingClause match below will explicitly check for empty clauses before recursing so this will
      // only be called for completely empty (case-lambda)s
      val falsePredTemp = ps.Temp(vt.Predicate)
      entryPlan.steps += ps.CreateNativeInteger(falsePredTemp, value=0, bits=vt.Predicate.bits)
      entryPlan.steps += ps.AssertPredicate(falsePredTemp, noMatchingClauseRuntimeErrorMessage)

      // This isn't reachable but we need to return to make codegen happy
      SingleValue(iv.UnitValue)

    case checkingClause :: tailClauses =>
      // See if our length matched
      val procValue = checkingClause.procValue
      val signature = procValue.polySignature.template
      val testingLength = signature.fixedArgTypes.length

      val testingLengthTemp = ps.Temp(vt.UInt32)
      entryPlan.steps += ps.CreateNativeInteger(testingLengthTemp, testingLength, vt.UInt32.bits)

      val (testingCond, signedOpt, argListType) = if (signature.restArgMemberTypeOpt.isDefined) {
        // We have a rest argument - we can deal with any args >= our fixed arg count
        (ps.CompareCond.GreaterThanEqual, Some(false), listOfAtLeast(testingLength))
      }
      else {
        // We can only match our fixed our count exactly
        (ps.CompareCond.Equal, None, listOfExactly(testingLength))
      }

      val matchesPred = ps.Temp(vt.Predicate)
      entryPlan.steps += ps.IntegerCompare(matchesPred, testingCond, signedOpt, argLengthTemp, testingLengthTemp) 

      // Plan for if the clause matches
      val truePlan = entryPlan.forkPlan()
      val restoredProcValue = checkingClause.capturedProcOpt match {
        case Some(CapturedProc(_, recordField)) =>
          // We need to restore this procedure from our closure
          val closureDataTemp = ps.RecordLikeDataTemp()
          truePlan.steps += ps.LoadRecordLikeData(closureDataTemp, innerSelfTempOpt.get, closureType)

          val restoredTemp = ps.Temp(recordField.fieldType)
          truePlan.steps += ps.LoadRecordDataField(restoredTemp, closureDataTemp, closureType, recordField)

          procValue.withSelfTemp(restoredTemp)

        case None =>
          procValue
      }

      val argListBoxed = BoxedValue(ct.ListElementCell, argListHeadTemp)
      val argListValue = new iv.CellValue(argListType, argListBoxed)

      val trueValues = PlanInvokeApply.withArgumentList(restoredProcValue, argListValue)(truePlan)

      if (tailClauses.isEmpty) {
        // We have to use this clause - assert then use the true values directly
        entryPlan.steps += ps.AssertPredicate(matchesPred, noMatchingClauseRuntimeErrorMessage)
        entryPlan.steps ++= truePlan.steps

        trueValues
      }
      else {
        // If the clause doesn't match then recurse
        val falsePlan = entryPlan.forkPlan()
        val falseValues = planClauseTests(
          plannedClauses=tailClauses,
          innerSelfTempOpt=innerSelfTempOpt,
          closureType=closureType,
          argListHeadTemp=argListHeadTemp,
          argLengthTemp=argLengthTemp
        )(falsePlan)

        // Now phi them together
        val phiResult = PlanResultValuesPhi(truePlan, trueValues, falsePlan, falseValues)

        entryPlan.steps += ps.CondBranch(
          result=phiResult.resultTemp,
          test=matchesPred,
          trueSteps=truePlan.steps.toList,
          trueValue=phiResult.leftTempValue,
          falseSteps=falsePlan.steps.toList,
          falseValue=phiResult.rightTempValue
        )

        phiResult.resultValues
      }
  }

  def apply(parentState : PlannerState, parentPlan : PlanWriter)(
      clauseExprs : List[et.Lambda],
      sourceNameHint : Option[String]
  ) : iv.KnownCaseLambdaProc = {
    val sourceName = sourceNameHint.getOrElse("anonymous-case-lambda")
    val nativeSymbol = parentPlan.allocSymbol(sourceName)

    // Plan each inner lambda clause
    val plannedClauses = clauseExprs.zipWithIndex.map { case (clauseExpr, index) =>
      val clauseNameHint = s"${sourceName} clause ${index}"

      val procValue = PlanLambda(parentState, parentPlan)(clauseExpr, Some(clauseNameHint), None)

      val capturedProcOpt = if (procValue.needsClosureRepresentation) {
        // Convert to a TempValue now because we can't enter GC while building our closure
        val procType = procValue.schemeType
        val procTemp = procValue.toTempValue(procType, convertProcType=false)(parentPlan)

        val recordField = new vt.RecordField(s"clause{$index}", procType)

        Some(CapturedProc(procTemp, recordField))
      }
      else {
        None
      }

      PlannedClause(procValue, capturedProcOpt)
    }

    // Determine if we need a closure
    val closureRequired = plannedClauses.exists(_.capturedProcOpt.isDefined)

    // Make our closure type
    val closureType = if (closureRequired) {
      val closureSourceName = sourceName + "-closure"

      val allFields = plannedClauses.flatMap(_.capturedProcOpt).map(_.recordField)
      new vt.ClosureType(closureSourceName, allFields)
    }
    else {
      vt.EmptyClosureType
    }

    val innerSelfTempOpt = if (closureRequired) {
      Some(ps.CellTemp(ct.ProcedureCell))
    }
    else {
      None
    }

    val argListHeadTemp = ps.CellTemp(ct.ListElementCell)

    val procPlan = parentPlan.forkPlan()

    // Calculate the length of our argument list
    val argLengthTemp = ps.Temp(vt.UInt32)
    procPlan.steps += ps.CalcProperListLength(argLengthTemp, argListHeadTemp)

    val resultValues = planClauseTests(
      plannedClauses=plannedClauses,
      innerSelfTempOpt=innerSelfTempOpt,
      closureType=closureType,
      argListHeadTemp=argListHeadTemp,
      argLengthTemp=argLengthTemp
    )(procPlan)

    val returnType = resultValues.preferredReturnType
    val resultTempOpt = resultValues.toReturnTempValue(returnType)(procPlan)
    procPlan.steps += ps.Return(resultTempOpt)

    // Determine our signature
    val signature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=closureRequired,
      restArgMemberTypeOpt=Some(vt.AnySchemeType),
      fixedArgTypes=Nil,
      returnType=returnType,
      attributes=Set(ProcedureAttribute.FastCC)
    ) : ProcedureSignature

    // Store the planend procedures in our closure
    val outerSelfTempOpt = if (closureRequired) {
      // Save the closure values from the parent's scope
      val cellTemp = ps.RecordTemp()
      val dataTemp = ps.RecordLikeDataTemp()

      parentPlan.steps += ps.InitRecordLike(cellTemp, dataTemp, closureType, isUndefined=false)

      for(plannedClause <- plannedClauses;
          capturedProc <- plannedClause.capturedProcOpt) {
        val recordField = capturedProc.recordField
        val procTemp = capturedProc.outerProcTemp

        parentPlan.steps += ps.SetRecordDataField(dataTemp, closureType, recordField, procTemp)
      }

      // Store our entry point
      val entryPointTemp = ps.EntryPointTemp()
      parentPlan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, nativeSymbol)
      parentPlan.steps += ps.SetProcedureEntryPoint(cellTemp, entryPointTemp)

      Some(cellTemp)
    }
    else {
      None
    }

    val plannedFunction = PlannedFunction(
      signature=signature,
      namedArguments=List(
        "world" -> ps.WorldPtrValue
      ) ++
      innerSelfTempOpt.toList.map({ procSelf =>
        ("self" -> procSelf)
      }) ++
      List(
        "restArg" -> argListHeadTemp
      ),
      steps=procPlan.steps.toList,
      debugContextOpt=None,
      irCommentOpt=None
    )

    parentPlan.plannedFunctions += (nativeSymbol -> plannedFunction)

    val knownClauses = plannedClauses map { case PlannedClause(procValue, capturedProcOpt) =>
      iv.KnownCaseLambdaClause(procValue, capturedProcOpt.map(_.recordField))
    }

    new iv.KnownCaseLambdaProc(
      polySignature=signature.toPolymorphic,
      closureType=closureType,
      clauses=knownClauses,
      plannedSymbol=nativeSymbol,
      selfTempOpt=outerSelfTempOpt,
      clausesInScope=true
    )
  }
}
