package io.llambda.compiler.planner
import io.llambda

import org.scalatest.{FunSuite, Inside}

import llambda.compiler._
import llambda.compiler.frontend.IncludePath
import llambda.compiler.SchemeStringImplicits._
import llambda.compiler.planner.{step => ps}

trait PlanHelpers extends FunSuite with Inside {
  val topLevelSymbol = codegen.LlambdaTopLevelSignature.nativeSymbol

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

    planner.PlanProgram(analysis.usedTopLevelExprs)(planConfig)
  }
  
  protected def assertStaticPlan(scheme : String, expected : ast.Datum) {
    val importDecl = datum"(import (scheme base) (scheme process-context) (llambda typed))"
    val data = List(
      importDecl,
      ast.ProperList(List(
        // Call exit to force the value to be evaluated
        ast.Symbol("exit"), SchemeParser.parseStringAsData(scheme, None).head
      ))
    )

    val plannedFunctions = planForData(data, optimise=true)
    val topLevelSteps = plannedFunctions(topLevelSymbol).steps 

    val filteredSteps = topLevelSteps.filter {
      case _ : ps.CastCellToTypeUnchecked =>
        // This doesn't produce machine code and will be used to convert our result to %datum* to pass to (exit)
        false

      case ps.CreateNamedEntryPoint(_, _, "lliby_exit") =>
        // This is loading the pointer to (exit)
        false

      case _ => 
        true
    }

    inside(filteredSteps) {
      case List(constantValueStep, _ : ps.Invoke, ps.Return(None)) =>
        expected match {
          case ast.IntegerLiteral(constantValue) =>
            inside(constantValueStep) { case ps.CreateExactIntegerCell(_, actualValue) =>
              assert(constantValue === actualValue)
            }

          case ast.BooleanLiteral(constantValue) =>
            inside(constantValueStep) { case ps.CreateBooleanCell(_, actualValue) =>
              assert(constantValue === actualValue)
            }
          
          case ast.Symbol(constantValue) =>
            inside(constantValueStep) { case ps.CreateSymbolCell(_, actualValue) =>
              assert(constantValue === actualValue)
            }

          case other =>
            fail("Unhandled datum type: " + other)
        }
    }
  }
}
