package io.llambda.compiler.planner
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._
import llambda.compiler.frontend.IncludePath

trait PlanHelpers extends FunSuite {
  protected def planForData(data : List[ast.Datum], optimise : Boolean, includePath : IncludePath = IncludePath()) : Map[String, PlannedFunction] = {
    val frontendConfig = frontend.FrontendConfig(
      includePath=includePath,
      featureIdentifiers=Set()
    )
    
    val compileConfig = CompileConfig(
      includePath=includePath,
      optimizeLevel=if (optimise) 0 else 2,
      targetPlatform=platform.DetectJvmPlatform()
    )
  
    val loader = new frontend.LibraryLoader(compileConfig.targetPlatform)
    val exprs = frontend.ExtractProgram(None, data)(loader, frontendConfig)
    val analysis = analyser.AnalyseExprs(exprs)

    val planConfig = planner.PlanConfig(
      optimize=optimise,
      analysis=analysis
    )

    planner.PlanProgram(exprs)(planConfig)
  }
}
