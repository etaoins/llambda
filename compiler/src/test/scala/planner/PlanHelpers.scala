package io.llambda.compiler.planner
import io.llambda

import org.scalatest.{FunSuite, Inside}

import llambda.compiler._
import llambda.compiler.frontend.IncludePath
import llambda.compiler.SchemeStringImplicits._
import llambda.compiler.planner.{step => ps}
import llambda.compiler.{valuetype => vt}

trait PlanHelpers extends FunSuite with Inside {
  val topLevelSymbol = codegen.LlambdaTopLevelSignature.nativeSymbol

  private def testPlanConfig(data : List[ast.Datum], optimise : Boolean, includePath : IncludePath = IncludePath()) = {
    val compileConfig = CompileConfig(
      includePath=includePath,
      optimiseLevel=if (optimise) 0 else 2,
      targetPlatform=platform.Posix64LE,
      schemeDialect=dialect.Dialect.default
    )

    val featureIdentifiers =
      compileConfig.targetPlatform.platformFeatures ++ compileConfig.schemeDialect.dialectFeatures
  
    val frontendConfig = frontend.FrontendConfig(
      includePath=includePath,
      featureIdentifiers=featureIdentifiers,
      schemeDialect=dialect.Dialect.default
    )
    
    val loader = new frontend.LibraryLoader(compileConfig.targetPlatform)
    val exprs = frontend.ExtractProgram(None, data)(loader, frontendConfig)
    val analysis = analyser.AnalyseExprs(exprs)

    planner.PlanConfig(
      schemeDialect=dialect.Dialect.default,
      optimise=optimise,
      analysis=analysis
    )
  }

  private def stepsForConstantDatum(datum : ast.Datum) : List[ps.Step] = {
    val planConfig = testPlanConfig(Nil, optimise=true)

    val planWriter = PlanWriter(planConfig)

    val constantValue = DatumToConstantValue(datum)

    constantValue.toTempValue(vt.AnySchemeType)(planWriter)
    planWriter.steps.toList
  }

  private def filterPlanStep : PartialFunction[ps.Step, Boolean] = {
    case _ : ps.CastCellToTypeUnchecked =>
      // This doesn't produce machine code and will be used to convert our result to %datum* to pass to (exit)
      false

    case _ : ps.DisposeValues =>
      // This doesn't produce machine code
      false

    case ps.CreateNamedEntryPoint(_, _, "llprocesscontext_exit") =>
      // This is loading the pointer to (exit)
      false

    case _ => 
      true
  }

  /** Returns a map of planned functions to the given Scheme data */
  protected def planForData(data : List[ast.Datum], optimise : Boolean, includePath : IncludePath = IncludePath()) : Map[String, PlannedFunction] = {
    val planConfig = testPlanConfig(data, optimise, includePath)
    planner.PlanProgram(planConfig.analysis.usedTopLevelExprs)(planConfig).functions
  }

  protected def planStepsFor(scheme : String) : List[ps.Step] = {
    val importDecl = datum"""
      (import (scheme base) (scheme inexact) (scheme case-lambda) (scheme process-context)
              (llambda typed) (llambda r7rs-case-lambda))
    """

    val data = List(
      importDecl,
      ast.ProperList(List(
        // Call exit to force the value to be evaluated
        ast.Symbol("exit"), ast.ProperList(
          ast.Symbol("begin") ::
          SchemeParser.parseStringAsData(scheme, None)
        )
      ))
    )

    val plannedFunctions = planForData(data, optimise=true)
    val topLevelSteps = DisposeValues(plannedFunctions(topLevelSymbol)).steps 

    topLevelSteps.filter(filterPlanStep)
  }

  protected def nativeLibrariesFor(scheme : String) : Set[NativeLibrary] = {
    val importDecl = datum"""(import (scheme base) (llambda nfi) (llambda typed) (scheme process-context))"""

    val data = importDecl :: SchemeParser.parseStringAsData(scheme)

    val planConfig = testPlanConfig(data, true)
    planner.PlanProgram(planConfig.analysis.usedTopLevelExprs)(planConfig).requiredNativeLibraries
  }


  /** Asserts that a Scheme string statically evaluates to the passed constant datum */
  protected def assertStaticPlan(scheme : String, expected : ast.Datum) {
    val planSteps = planStepsFor(scheme)
    
    inside(planSteps.reverse) {
      case ps.Return(None) :: (_ : ps.Invoke) :: reverseActualSteps =>
        // These are the steps we expect to see
        val expectedSteps = stepsForConstantDatum(expected).filter(filterPlanStep)
        // These are the steps we actually saw
        val actualSteps = reverseActualSteps.reverse

        assert(actualSteps.length === expectedSteps.length)

        expectedSteps.zip(actualSteps) map {
          case (_ : ps.CreatePairCell, _ : ps.CreatePairCell) =>
            // XXX: We need to be more clever to deal with this
            // Depend on the fact the actual list members will be interleaved with the pairs in a predictable way
          case (_ : ps.CreateVectorCell, _ : ps.CreateVectorCell) =>
            // XXX: We need to be more clever to deal with this
          case (expectedStep : ps.MergeableStep, actualStep : ps.MergeableStep) =>
            // Use merge the merge key which will replace the result TempValue with a fixed constant
            assert(expectedStep.mergeKey === actualStep.mergeKey)

          case _ =>
            fail("Non-disposable step encountered; cannot compare:")
        }
    }
  }
}
