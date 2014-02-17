package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import collection.mutable

import llambda.compiler.ProcedureSignature
import llambda.compiler.ast
import llambda.compiler.planner._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.codegen.AdaptedProcedureSignature
import llambda.compiler.RuntimeErrorMessage

/** Plans a trampoline for the passed procedure 
  * 
  * All trampolines have AdaptedProcedureSignature which means they can be
  * called without knowing the signature of the underlying procedure. It is 
  * assumed the argument list a proper list; it is inappropriate to pass user
  * provided arguments lists to a trampoline without confirming the list is
  * proper beforehand.
  */
private[intermediatevalue] object PlanProcedureTrampoline {
  def apply(signature : ProcedureSignature, nativeSymbol : String)(implicit parentPlan : PlanWriter) : PlannedFunction = {
    val worldPtrTemp = ps.GcUnmanagedValue()
    val selfTemp = ps.GcManagedValue()
    val argListHeadTemp = ps.GcManagedValue()

    implicit val plan = parentPlan.forkPlan()

    // Change our argListHeadTemp to a IntermediateValue
    val argListHeadValue = TempValueToIntermediate(vt.IntrinsicCellType(ct.ListElementCell), argListHeadTemp)

    val argTemps = new mutable.ListBuffer[ps.TempValue]

    if (signature.hasWorldArg) {
      argTemps += worldPtrTemp
    }

    if (signature.hasSelfArg) {
      // Pass the closure through directly
      argTemps += selfTemp
    }

    val insufficientArgsMessage = if (signature.hasRestArg) {
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
      val argPairTemp = argListElementValue.toTempValue(vt.IntrinsicCellType(ct.PairCell), Some(insufficientArgsMessage))(plan)

      // Get the car of the pair as the arg's value 
      val argDatumTemp = ps.GcManagedValue()
      plan.steps += ps.StorePairCar(argDatumTemp, argPairTemp)

      // Convert it to the expected type
      val argValue = TempValueToIntermediate(vt.IntrinsicCellType(ct.DatumCell), argDatumTemp)
      val argTemp = argValue.toTempValue(nativeType)(plan)

      argTemps += argTemp

      // Now load the cdr
      val argCdrTemp = ps.GcManagedValue()
      plan.steps += ps.StorePairCdr(argCdrTemp, argPairTemp)

      // We know this is a list element but its type will be DatumCell
      new IntrinsicCellValue(ct.ListElementCell.concreteTypes, ct.DatumCell, argCdrTemp)
    }

    if (signature.hasRestArg) {
      // This is already a ListElementCell
      argTemps += restArgValue.toTempValue(vt.IntrinsicCellType(ct.ListElementCell))(plan)
    }
    else {
      val tooManyArgsMessage = RuntimeErrorMessage(
        name=s"tooManyArgsFor${nativeSymbol}",
        text=s"Called ${nativeSymbol} with too many arguments; requires exactly ${signature.fixedArgs.length} arguments."
      )
      
      // Make sure we're out of args by doing a check cast to an empty list
      restArgValue.toTempValue(vt.IntrinsicCellType(ct.EmptyListCell), Some(tooManyArgsMessage))(plan)
    }

    // Load the entry point for the function we're jumping to
    val entryPointTemp = ps.GcUnmanagedValue()
    plan.steps += ps.StoreNamedEntryPoint(entryPointTemp, signature, nativeSymbol)

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

    val returnTemp = returnValue.toTempValue(vt.IntrinsicCellType(ct.DatumCell))(plan)
    plan.steps += ps.Return(Some(returnTemp))

    PlannedFunction(
      signature=AdaptedProcedureSignature,
      namedArguments=List(
        ("world"   -> worldPtrTemp),
        ("closure" -> selfTemp),
        ("argList" -> argListHeadTemp)
      ),
      steps=plan.steps.toList
    ) 
  }
}
