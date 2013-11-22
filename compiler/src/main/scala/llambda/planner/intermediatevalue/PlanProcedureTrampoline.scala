package llambda.planner.intermediatevalue

import collection.mutable

import llambda.nfi
import llambda.ast
import llambda.planner._
import llambda.planner.{step => ps}
import llambda.{boxedtype => bt}
import llambda.codegen.BoxedProcedureSignature
import llambda.planner.{intermediatevalue => iv}

private[intermediatevalue] object PlanProcedureTrampoline {
  def apply(signature : nfi.NativeSignature, nativeSymbol : String)(implicit parentPlan : PlanWriter) : PlannedFunction = {
    val closureTemp = new ps.TempValue
    val argListHeadTemp = new ps.TempValue

    implicit val plan = parentPlan.forkPlan()

    // Change our argListHeadTemp to a IntermediateValue
    val argListHeadValue = NativeToIntermediateValue(nfi.BoxedValue(bt.BoxedListElement), argListHeadTemp)

    val argTemps = new mutable.ListBuffer[ps.TempValue]

    if (signature.hasClosureArg) {
      // Pass the closure through directly
      argTemps += closureTemp
    }
    
    // Convert our arg list in to the arguments our procedure is expecting
    val restArgValue = signature.fixedArgs.foldLeft(argListHeadValue) { case (argListElementValue, nativeType) =>
      // Make sure this is a pair
      val argPairTemp = argListElementValue.toRequiredTempValue(nfi.BoxedValue(bt.BoxedPair))(plan)

      // Get the car of the pair as the arg's value 
      val argDatumTemp = new ps.TempValue
      plan.steps += ps.StorePairCar(argDatumTemp, argPairTemp)

      // Convert it to the expected type
      val argValue = NativeToIntermediateValue(nfi.BoxedValue(bt.BoxedDatum), argDatumTemp)
      val argTemp = argValue.toRequiredTempValue(nativeType)(plan)

      argTemps += argTemp

      // Now load the cdr
      val argCdrTemp = new ps.TempValue
      plan.steps += ps.StorePairCdr(argCdrTemp, argPairTemp)

      // We know this is a list element but its type will be BoxedDatum
      new iv.DynamicBoxedValue(bt.BoxedListElement.concreteTypes, bt.BoxedDatum, argCdrTemp)
    }

    if (signature.hasRestArg) {
      // This is already a BoxedListElement
      argTemps += restArgValue.toRequiredTempValue(nfi.BoxedValue(bt.BoxedListElement))(plan)
    }

    // Load the entry point for the function we're jumping to
    val entryPointTemp = new ps.TempValue
    plan.steps += ps.StoreNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

    // Create our result temp value if any
    val resultTempOpt = signature.returnType map { _ =>
      new ps.TempValue
    }

    // Invoke!
    plan.steps += ps.Invoke(resultTempOpt, signature, entryPointTemp, argTemps.toList)

    val returnValue = resultTempOpt map { resultTemp =>
      NativeToIntermediateValue(signature.returnType.get, resultTemp)
    } getOrElse {
      DatumToConstantValue(ast.UnspecificValue())
    }

    val returnTemp = returnValue.toRequiredTempValue(nfi.BoxedValue(bt.BoxedDatum))(plan)
    plan.steps += ps.Return(Some(returnTemp))

    PlannedFunction(
      signature=BoxedProcedureSignature,
      namedArguments=List(
        ("closure" -> closureTemp),
        ("argList" -> argListHeadTemp)
      ),
      steps=plan.steps.toList
    ) 
  }
}
