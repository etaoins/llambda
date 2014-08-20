package io.llambda.compiler.planner
import io.llambda

import collection.mutable

import llambda.compiler.ProcedureSignature
import llambda.compiler.ast
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._
import llambda.compiler.codegen.AdaptedProcedureSignature
import llambda.compiler.{RuntimeErrorMessage, ContextLocated}

private[planner] object PlanProcedureTrampoline {
  /** Plans a trampoline for the passed procedure 
    * 
    * All trampolines have AdaptedProcedureSignature which means they can be called without knowing the signature of the
    * underlying procedure. It is assumed the argument list a proper list; it is inappropriate to pass user provided
    * arguments lists to a trampoline without confirming the list is proper beforehand.
    *
    * @param  signature         Signature of the target procedure. The trampoline will ensure the arguments it's passed
    *                           satisfy the target procedure's signature and perform any required type conversions. 
    * @param  nativeSymbol      Native symbol of the target procedure 
    * @param  targetProcLocOpt  Source location of the target procedure. This is used to generate a comment in the 
    *                           output IR identifying the trampoline.
    */
  def apply(
      signature : ProcedureSignature,
      nativeSymbol : String,
      targetProcLocOpt : Option[ContextLocated] = None
  )(implicit parentPlan : PlanWriter) : PlannedFunction = {
    val worldPtrTemp = new ps.WorldPtrValue
    val selfTemp = ps.CellTemp(ct.ProcedureCell)
    val argListHeadTemp = ps.CellTemp(ct.ListElementCell)

    implicit val plan = parentPlan.forkPlan()

    // Change our argListHeadTemp to a IntermediateValue
    val argListHeadValue = TempValueToIntermediate(vt.ProperListType(vt.AnySchemeType), argListHeadTemp)

    val argTemps = new mutable.ListBuffer[ps.TempValue]

    if (signature.hasWorldArg) {
      argTemps += worldPtrTemp
    }

    if (signature.hasSelfArg) {
      // Pass the closure through directly
      argTemps += selfTemp
    }

    val insufficientArgsMessage = if (signature.restArgOpt.isDefined) {
      RuntimeErrorMessage(
        name=s"insufficientArgsFor${nativeSymbol}",
        text=s"Called ${nativeSymbol} with insufficient arguments; requires at least ${signature.fixedArgs.length} arguments."
      )
    }
    else {
      RuntimeErrorMessage(
        name=s"insufficientArgsFor${nativeSymbol}",
        text=s"Called ${nativeSymbol} with insufficient arguments; requires exactly ${signature.fixedArgs.length} arguments."
      )
    }
    
    // Convert our arg list in to the arguments our procedure is expecting
    val restArgValue = signature.fixedArgs.foldLeft(argListHeadValue) { case (argListElementValue, nativeType) =>
      // Make sure this is a pair
      val argPairTemp = argListElementValue.toTempValue(vt.AnyPairType, Some(insufficientArgsMessage))(plan, worldPtrTemp)

      // Get the car of the pair as the arg's value 
      val argDatumTemp = ps.CellTemp(ct.AnyCell)
      plan.steps += ps.LoadPairCar(argDatumTemp, argPairTemp)

      // Convert it to the expected type
      val argValue = TempValueToIntermediate(vt.AnySchemeType, argDatumTemp)
      val argTemp = argValue.toTempValue(nativeType)(plan, worldPtrTemp)

      argTemps += argTemp

      // Now load the cdr
      val argCdrTemp = ps.CellTemp(ct.AnyCell)
      plan.steps += ps.LoadPairCdr(argCdrTemp, argPairTemp)

      // We know this is a list element but its type will be AnyCell
      new iv.CellValue(vt.ProperListType(vt.AnySchemeType), BoxedValue(ct.AnyCell, argCdrTemp))
    }

    signature.restArgOpt match {
      case Some(memberType) =>
        val typeCheckedRestArg = restArgValue.toTempValue(vt.ProperListType(memberType))(plan, worldPtrTemp)
        argTemps += typeCheckedRestArg

      case None =>
        val tooManyArgsMessage = RuntimeErrorMessage(
          name=s"tooManyArgsFor${nativeSymbol}",
          text=s"Called ${nativeSymbol} with too many arguments; requires exactly ${signature.fixedArgs.length} arguments."
        )
        
        // Make sure we're out of args by doing a check cast to an empty list
        restArgValue.toTempValue(vt.EmptyListType, Some(tooManyArgsMessage))(plan, worldPtrTemp)
    }

    // Load the entry point for the function we're jumping to
    val entryPointTemp = ps.EntryPointTemp()
    plan.steps += ps.CreateNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

    // Create our result temp value if any
    val resultTempOpt = signature.returnType map { returnType =>
      new ps.TempValue(returnType.isGcManaged)
    }

    // Invoke!
    val invokeArgs = argTemps.toList.map(ps.InvokeArgument(_))
    plan.steps += ps.Invoke(resultTempOpt, signature, entryPointTemp, invokeArgs)

    val returnValue = resultTempOpt map { resultTemp =>
      TempValueToIntermediate(signature.returnType.get, resultTemp)
    } getOrElse {
      DatumToConstantValue(ast.UnitValue())
    }

    val returnTemp = returnValue.toTempValue(vt.AnySchemeType)(plan, worldPtrTemp)
    plan.steps += ps.Return(Some(returnTemp))

    val irCommentOpt =
      for(targetProcLoc <- targetProcLocOpt;
          location <- targetProcLoc.locationOpt)
      yield
        s"Trampoline function for Scheme procedure defined at ${location.locationOnlyString}"

    PlannedFunction(
      signature=AdaptedProcedureSignature,
      namedArguments=List(
        ("world"   -> worldPtrTemp),
        ("closure" -> selfTemp),
        ("argList" -> argListHeadTemp)
      ),
      steps=plan.steps.toList,
      worldPtrOpt=Some(worldPtrTemp),
      debugContextOpt=None,
      irCommentOpt=irCommentOpt
    ) 
  }
}
