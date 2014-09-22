package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.ProcedureSignature
import llambda.compiler.ast
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{RuntimeErrorMessage, ContextLocated}

import llambda.compiler.valuetype.Implicits._

private[planner] object PlanProcedureTrampoline {
  /** Plans a trampoline for the passed procedure 
    * 
    * @param  trampolineSignature  The required signature for the trampoline function
    * @param  targetProc           The target procedure. The trampoline will ensure the arguments it's passed satisfy
    *                              the target procedure's signature and perform any required type conversions.
    * @param  isAdapter            If true this is an adapter procedure. The real procedure cell will be loaded from
    *                              the self parameter and passed to the destination procedure
    * @param  targetProcLocOpt     Source location of the target procedure. This is used to generate a comment in the 
    *                              output IR identifying the trampoline.
    */
  def apply(
      trampolineSignature : ProcedureSignature,
      targetProc : InvokableProcedure,
      isAdapter : Boolean,
      targetProcLocOpt : Option[ContextLocated] = None
  )(implicit parentPlan : PlanWriter) : PlannedFunction = {
    val worldPtrTemp = new ps.WorldPtrValue
    val inSelfTemp = ps.CellTemp(ct.ProcedureCell)
    implicit val plan = parentPlan.forkPlan()

    // Make some aliases
    val inSignature = trampolineSignature
    val outSignature = targetProc.signature

    val inFixedArgCount = inSignature.fixedArgTypes.length
    val outFixedArgCount = outSignature.fixedArgTypes.length

    val inHasRestArg = inSignature.restArgMemberTypeOpt.isDefined
    val outHasRestArg = outSignature.restArgMemberTypeOpt.isDefined
    
    // Create our rest arg temp if we have one
    val inRestArgTempOpt = inSignature.restArgMemberTypeOpt map { _ =>
      ps.CellTemp(ct.ListElementCell)
    }
    
    val inRestArgValueOpt = inRestArgTempOpt map { inRestArgTemp =>
      val inRestArgType = vt.UniformProperListType(trampolineSignature.restArgMemberTypeOpt.get)
      TempValueToIntermediate(inRestArgType, inRestArgTemp)(plan.config)
    }

    // Directly map our fixed args
    val mappedFixedArgs = inSignature.fixedArgTypes.zip(outSignature.fixedArgTypes).map {
      case (trampolineType, targetType) =>
        val inTemp = ps.Temp(trampolineType)
        val inValue = TempValueToIntermediate(trampolineType, inTemp)(plan.config)

        val outTemp = inValue.toTempValue(targetType)(plan, worldPtrTemp)

        (inTemp -> outTemp) 
    }

    val (extraOutFixedArgTemps, extraInFixedArgTemps, outRestArgValueOpt) = if (outFixedArgCount > inFixedArgCount) {
      // We need to pull members off of our in rest argument to pass as out fixed arguments
      val extraOutFixedArgTypes = outSignature.fixedArgTypes.drop(inFixedArgCount)
      
      val insufficientArgsMessage = ArityRuntimeErrorMessage.insufficientArgs(targetProc)(plan)

      val destructuredArgs = DestructureList(
        listValue=inRestArgValueOpt.get,
        memberTypes=extraOutFixedArgTypes,
        insufficientLengthMessage=insufficientArgsMessage
      )(plan, worldPtrTemp)

      (destructuredArgs.memberTemps, Nil, Some(destructuredArgs.listTailValue))
    }
    else if (outFixedArgCount < inFixedArgCount) {
      // We need to add in fixed arguments to our out rest argument
      val extraInFixedArgTypes = inSignature.fixedArgTypes.drop(outFixedArgCount)

      val outRestArgTail = inRestArgValueOpt.getOrElse(iv.EmptyListValue)

      val extraInFixedArgTemps = extraInFixedArgTypes.map { fixedArgType =>
        ps.Temp(fixedArgType)
      }

      val extraInFixedArgValues = extraInFixedArgTypes.zip(extraInFixedArgTemps) map {
        case (fixedArgType, fixedArgTemp) =>
          TempValueToIntermediate(fixedArgType, fixedArgTemp)(plan.config)
      }

      val outRestArgValue = ValuesToList(
        memberValues=extraInFixedArgValues,
        tailValue=outRestArgTail,
        capturable=false
      )(plan, worldPtrTemp)

      (Nil, extraInFixedArgTemps, Some(outRestArgValue))
    }
    else {
      // Our fixed arg arity matches exactly
      (Nil, Nil, inRestArgValueOpt)
    }

    val outRestArgTempOpt = outRestArgValueOpt match {
      case Some(outRestArgValue) if !outHasRestArg =>
        // Make sure out out rest arg is empty
        val tooManyArgsMessage = ArityRuntimeErrorMessage.tooManyArgs(targetProc)(plan)
        outRestArgValue.toTempValue(vt.EmptyListType, Some(tooManyArgsMessage))(plan, worldPtrTemp)
        None

      case Some(outRestArgValue) if outHasRestArg =>
        // Make sure the out rest arg has the right type
        val expectedType = vt.UniformProperListType(outSignature.restArgMemberTypeOpt.get)
        outRestArgValue.castToSchemeType(expectedType)(plan, worldPtrTemp)

        Some(outRestArgValue.toTempValue(vt.ListElementType)(plan, worldPtrTemp))

      case None if !outHasRestArg =>
        None
      
      case None if outHasRestArg =>
        Some(iv.EmptyListValue.toTempValue(vt.ListElementType)(plan, worldPtrTemp))
    }

    val (updatedInvokable, outSelfTemp) = if (isAdapter) {
      // Load the real target proc
      val closureDataTemp = ps.RecordLikeDataTemp()
      plan.steps += ps.LoadRecordLikeData(closureDataTemp, inSelfTemp, AdapterProcType)

      val targetProcCell = ps.CellTemp(ct.ProcedureCell)
      plan.steps += ps.LoadRecordDataField(targetProcCell, closureDataTemp, AdapterProcType, AdapterProcField)

      (targetProc.withSelfValue(targetProcCell), targetProcCell)
    }
    else {
      (targetProc, inSelfTemp) 
    }

    // Collect all of our arguments now
    val allInFixedArgTemps = mappedFixedArgs.map(_._1) ++ extraInFixedArgTemps  
    val allOutFixedArgTemps = mappedFixedArgs.map(_._2) ++ extraOutFixedArgTemps  

    val resultValues = PlanInvokeApply.withTempValues(
      invokableProc=updatedInvokable,
      fixedTemps=allOutFixedArgTemps,
      restTemps=outRestArgTempOpt,
      selfTempOverride=Some(outSelfTemp)
    )(plan, worldPtrTemp)

    val returnTempOpt = resultValues.toReturnTempValue(trampolineSignature.returnType)(plan, worldPtrTemp)
    plan.steps += ps.Return(returnTempOpt)

    val irCommentOpt =
      for(targetProcLoc <- targetProcLocOpt;
          location <- targetProcLoc.locationOpt)
      yield
        s"Trampoline function for Scheme procedure defined at ${location.locationOnlyString}"

    val inNamedArguments = List(
      ("world"   -> worldPtrTemp),
      ("closure" -> inSelfTemp)
    ) ++ allInFixedArgTemps.zipWithIndex.map { case (fixedArgTemp, index) =>
      s"fixedArg${index}" -> fixedArgTemp
    } ++ inRestArgTempOpt.toList.map { case restArgTemp =>
      "restArg" -> restArgTemp
    }

    PlannedFunction(
      signature=trampolineSignature,
      namedArguments=inNamedArguments,
      steps=plan.steps.toList,
      worldPtrOpt=Some(worldPtrTemp),
      debugContextOpt=None,
      irCommentOpt=irCommentOpt
    ) 
  }
}
