package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.{PolymorphicSignature, StorageLocation, ContextLocated}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner._
import llambda.compiler.et

/** Represents a user-provided procedure with a Scheme language definitio */
class KnownSchemeProc(
    polySignature : PolymorphicSignature,
    plannedSymbol : String,
    selfTempOpt : Option[ps.TempValue],
    val manifest : LambdaManifest,
    isPrimaryPolymorph : Boolean,
    reportNameOpt : Option[String] = None)
extends KnownUserProc(polySignature, plannedSymbol, selfTempOpt, reportNameOpt) {
  // Override this to ensure we have vt.ProcedureType
  // This is required for KnownCaseLambdaProc to collect its type from its clauses
  override val schemeType : vt.ProcedureType = polySignature.toSchemeProcedureType

  override def locationOpt : Option[ContextLocated] =
    Some(manifest.lambdaExpr)

  override def withReportName(newReportName : String) : KnownSchemeProc = {
    new KnownSchemeProc(
      polySignature,
      plannedSymbol,
      selfTempOpt,
      manifest,
      isPrimaryPolymorph,
      Some(newReportName)
    )
  }

  override def withSelfTemp(selfTemp : ps.TempValue) : KnownSchemeProc =
    new KnownSchemeProc(
      polySignature,
      plannedSymbol,
      Some(selfTemp),
      manifest,
      isPrimaryPolymorph,
      reportNameOpt
    )

  private def createPolymorph(resolvedType : vt.ProcedureType)(implicit plan : PlanWriter) : KnownSchemeProc = {
    val polymorphKey = (manifest, resolvedType)

    val polymorphSymbol = plan.polymorphInstances.getOrElseUpdate(polymorphKey, {
      val polymorphSymbol = plan.allocSymbol(plannedSymbol + " " + resolvedType + " Polymorph")

      // Plan the polymorph
      val plannedPolymorph = PlanLambdaPolymorph(polymorphSymbol, manifest, resolvedType, false)
      plan.plannedFunctions += polymorphSymbol -> plannedPolymorph

      polymorphSymbol
    })

    val polymorphSignature = plan.plannedFunctions(polymorphSymbol).signature

    val polymorphSelfTempOpt = selfTempOpt map { selfTemp =>
      val polymorphEntryPointTemp = ps.EntryPointTemp()
      plan.steps += ps.CreateNamedEntryPoint(polymorphEntryPointTemp, polymorphSignature, polymorphSymbol)

      // Create the polymorph procedure cell
      val polymorphProcTemp = ps.CellTemp(ct.ProcedureCell)

      val adapterFields = Map[vt.RecordField, ps.TempValue](AdapterProcField -> selfTemp)
      plan.steps += ps.InitProcedure(polymorphProcTemp, AdapterProcType, polymorphEntryPointTemp, adapterFields)

      polymorphProcTemp
    }

    new KnownSchemeProc(
      polymorphSignature.toPolymorphic,
      polymorphSymbol,
      polymorphSelfTempOpt,
      manifest,
      isPrimaryPolymorph=false,
      reportNameOpt=reportNameOpt
    )
  }

  override def toApplicableValueForArgs (
      args : List[vt.SchemeType]
  )(implicit plan : PlanWriter) : IntermediateValue =
    if (isPrimaryPolymorph && !manifest.lambdaExpr.polyType.typeVars.isEmpty) {
      val polyType = manifest.lambdaExpr.polyType
      val resolvedType = polyType.typeForArgs(args)

      if (resolvedType == polyType.upperBound) {
        // We resolved to the same type
        this
      }
      else {
        createPolymorph(resolvedType)
      }
    }
    else {
      // We're already specialised - don't re-polymorph
      this
    }
}

