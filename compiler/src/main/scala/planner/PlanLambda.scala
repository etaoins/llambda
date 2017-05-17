package io.llambda.compiler.planner
import io.llambda

import scala.collection.breakOut
import scala.collection.immutable.ListMap

import llambda.compiler.et
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.StorageLocation


private[planner] object PlanLambda {
  private def closureFieldValues(
      closureType: vt.ClosureType,
      capturedVariables: List[CapturedVariable]
  )(implicit plan: PlanWriter): ListMap[vt.RecordField, ps.TempValue] = {
    capturedVariables.map({ capturedVar =>
      val varTemp = capturedVar match {
        case immutable: CapturedImmutable =>
          // Cast the value to its preferred type
          immutable.parentIntermediate.toTempValue(capturedVar.valueType, convertProcType=false)

        case mutable: CapturedMutable =>
          // Store the pointer to the mutable directly
          mutable.parentMutable.mutableTemp
      }

      // Store to the field
      (capturedVar.recordField -> varTemp)
    })(breakOut)
  }

  def apply(parentState: PlannerState, parentPlan: PlanWriter)(
      lambdaExpr: et.Lambda,
      sourceNameHint: Option[String],
      recursiveSelfLocOpt: Option[StorageLocation]
  ): iv.KnownSchemeProc = {
    // Give ourselves a name. This will be made unique if it collides
    val sourceName = sourceNameHint.getOrElse("anonymous-procedure")
    val nativeSymbol = parentPlan.allocSymbol(sourceName)

    val closedVars = FindClosedVars(parentState, lambdaExpr, recursiveSelfLocOpt)

    // Collect only the capture variables
    val capturedVariables = (closedVars collect {
      case captured: CapturedVariable => captured
    }).toList

    // Make our closure type
    val closureType = if (!capturedVariables.isEmpty) {
      val closureSourceName = sourceName + "-closure"
      new vt.ClosureType(closureSourceName, capturedVariables.map(_.recordField))
    }
    else {
      vt.EmptyClosureType
    }

    // Save our closure information
    val manifest = LambdaManifest(
      parentState=parentState,
      closureType=closureType,
      closedVars=closedVars,
      lambdaExpr=lambdaExpr,
      recursiveSelfLocOpt=recursiveSelfLocOpt
    )

    val plannedFunction = PlanLambdaPolymorph(
      nativeSymbol=nativeSymbol,
      manifest=manifest,
      lambdaExpr.polyType.upperBound,
      isPrimaryPolymorph=true
    )(parentPlan)

    val outerSelfTempOpt = if (!capturedVariables.isEmpty) {
      // Save the closure values from the parent's scope
      val cellTemp = ps.TempValue()

      // Create our closure
      val closureFields = closureFieldValues(closureType, capturedVariables)(parentPlan)
      parentPlan.steps += ps.InitInternalProc(cellTemp, closureType, closureFields)

      Some(cellTemp)
    }
    else {
      None
    }

    parentPlan.plannedFunctions += (nativeSymbol -> plannedFunction)

    new iv.KnownSchemeProc(
      polySignature=plannedFunction.signature.toPolymorphic,
      plannedSymbol=nativeSymbol,
      selfTempOpt=outerSelfTempOpt,
      manifest=manifest,
      isPrimaryPolymorph=true
    )
  }
}

