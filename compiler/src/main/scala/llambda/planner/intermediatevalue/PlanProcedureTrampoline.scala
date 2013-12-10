package llambda.planner.intermediatevalue

import collection.mutable

import llambda.ProcedureSignature
import llambda.ast
import llambda.planner._
import llambda.planner.{step => ps}
import llambda.{celltype => ct}
import llambda.{valuetype => vt}
import llambda.codegen.AdaptedProcedureSignature

private[intermediatevalue] object PlanProcedureTrampoline {
  def apply(signature : ProcedureSignature, nativeSymbol : String)(implicit parentPlan : PlanWriter) : PlannedFunction = {
    val selfTemp = new ps.TempValue
    val argListHeadTemp = new ps.TempValue

    implicit val plan = parentPlan.forkPlan()

    // Change our argListHeadTemp to a IntermediateValue
    val argListHeadValue = TempValueToIntermediate(vt.IntrinsicCellType(ct.ListElementCell), argListHeadTemp)

    val argTemps = new mutable.ListBuffer[ps.TempValue]

    if (signature.hasSelfArg) {
      // Pass the closure through directly
      argTemps += selfTemp
    }
    
    // Convert our arg list in to the arguments our procedure is expecting
    val restArgValue = signature.fixedArgs.foldLeft(argListHeadValue) { case (argListElementValue, nativeType) =>
      // Make sure this is a pair
      val argPairTemp = argListElementValue.toRequiredTempValue(vt.IntrinsicCellType(ct.PairCell))(plan)

      // Get the car of the pair as the arg's value 
      val argDatumTemp = new ps.TempValue
      plan.steps += ps.StorePairCar(argDatumTemp, argPairTemp)

      // Convert it to the expected type
      val argValue = TempValueToIntermediate(vt.IntrinsicCellType(ct.DatumCell), argDatumTemp)
      val argTemp = argValue.toRequiredTempValue(nativeType)(plan)

      argTemps += argTemp

      // Now load the cdr
      val argCdrTemp = new ps.TempValue
      plan.steps += ps.StorePairCdr(argCdrTemp, argPairTemp)

      // We know this is a list element but its type will be DatumCell
      new IntrinsicCellValue(ct.ListElementCell.concreteTypes, ct.DatumCell, argCdrTemp)
    }

    if (signature.hasRestArg) {
      // This is already a ListElementCell
      argTemps += restArgValue.toRequiredTempValue(vt.IntrinsicCellType(ct.ListElementCell))(plan)
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
      TempValueToIntermediate(signature.returnType.get, resultTemp)
    } getOrElse {
      DatumToConstantValue(ast.UnspecificValue())
    }

    val returnTemp = returnValue.toRequiredTempValue(vt.IntrinsicCellType(ct.DatumCell))(plan)
    plan.steps += ps.Return(Some(returnTemp))

    PlannedFunction(
      signature=AdaptedProcedureSignature,
      namedArguments=List(
        ("closure" -> selfTemp),
        ("argList" -> argListHeadTemp)
      ),
      steps=plan.steps.toList
    ) 
  }
}
