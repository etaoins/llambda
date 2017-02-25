package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.StorageLocation
import llambda.compiler.planner.{step => ps}

private[planner] object LoadClosureData {
  /** Loads values from a lambda's closure
    *
    * @param  procTemp     Temp value for the procedure to load values from
    * @param  manifest     Information about the lambda's manifest
    * @param  onlyVarsOpt  Optional filter of captured variables to load. If this is not provided then all captured
    *                      variables will be loaded
    * @return Map of loaded values
    */
  def apply(
      procTemp: ps.TempValue,
      manifest: LambdaManifest,
      onlyVarsOpt: Option[Set[StorageLocation]] = None
  )(implicit plan: PlanWriter): Map[StorageLocation, LocationValue] = {
    val wantedVariables = onlyVarsOpt match {
      case Some(onlyVariables) =>
        manifest.capturedVars.filter { capturedVar =>
          onlyVariables.contains(capturedVar.storageLoc)
        }

      case None =>
        manifest.capturedVars
    }

    if (wantedVariables.isEmpty) {
      // Don't bother loading our closure data
      Map()
    }
    else {
      // Build Temps for each record field
      val capturedVarToTemp = wantedVariables.map({ capturedVar =>
        val fieldType = manifest.closureType.typeForField(capturedVar.recordField)
        (capturedVar, new ps.TempValue(fieldType.isGcManaged))
      })

      // Load them all
      val recordFieldToTemp = capturedVarToTemp.map { case (capturedVar, varTemp) =>
        (capturedVar.recordField, varTemp)
      }

      plan.steps += ps.LoadRecordLikeFields(procTemp, manifest.closureType, recordFieldToTemp)

      capturedVarToTemp.map({ case (capturedVar, varTemp) =>
        // Add it to our state
        capturedVar match {
          case immutable: CapturedImmutable =>
            val varValue = immutable.parentIntermediate.restoreFromClosure(capturedVar.valueType, varTemp)(plan.config)
            capturedVar.storageLoc -> ImmutableValue(varValue)

          case capturedMutable: CapturedMutable =>
            capturedVar.storageLoc -> capturedMutable.parentMutable.copy(mutableTemp=varTemp)
        }
      }).toMap
    }
  }
}
