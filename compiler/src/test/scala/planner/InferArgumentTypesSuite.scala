package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}
import llambda.compiler.SchemeStringImplicits._

import llambda.compiler._
import org.scalatest.FunSuite

class InferArgumentTypesSuite  extends FunSuite {
  private def signatureFor(scheme : String) : ProcedureSignature = {
    val includePath = frontend.IncludePath(
      fileParentDir=None,
      packageRootDir=None
    )

    val frontendConfig = frontend.FrontendConfig(
      includePath=includePath,
      featureIdentifiers=Set()
    )
    
    val compileConfig = CompileConfig(
      includePath=includePath,
      // Don't optimize in case earlier passes transform the lambda body in
      // ways we don't expect
      optimizeLevel=0,
      targetPlatform=platform.DetectJvmPlatform()
    )
  
    val importDecl = datum"(import (llambda nfi) (scheme base))"
    val procedureData = SchemeParser.parseStringAsData(scheme, None)
    val data = importDecl :: procedureData

    val loader = new frontend.LibraryLoader(compileConfig.targetPlatform)
    val expressions = frontend.ExtractProgram(data)(loader, frontendConfig)
    val analysis = analyzer.Analyize(expressions)

    val planConfig = planner.PlanConfig(
      optimize=true,
      analysis=analysis
    )

    val functions = planner.PlanProgram(expressions)(planConfig)

    // Remove our entry point
    val entryPointSymbol = codegen.LlambdaExecSignature.nativeSymbol
    val (userDefinedFunction :: Nil) = (functions - entryPointSymbol).values.toList

    userDefinedFunction.signature
  }

  test("argless procedure returning integer constant") {
    val signature = signatureFor("""(lambda () 1)""")

    assert(signature.fixedArgs === Nil)
    assert(signature.returnType === Some(vt.Int64))
  }
  
  test("procedure returning its argument") {
    val signature = signatureFor("""(lambda (x) x)""")

    // This can be passed anything so it can return anything
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.DatumCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("procedure proxying (vector-ref)") {
    val signature = signatureFor("""(lambda (vec index) (vector-ref vec index))""")

    // This can be passed anything so it can return anything
    // Note that this could really be ct.UInt32 but we're not smart enough to figure that out
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.VectorCell), vt.IntrinsicCellType(ct.ExactIntegerCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("procedure proxying (vector-ref) past a conditional") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if #t 1 2)
        (vector-ref vec index))""")

    // Because the (vector-ref) is unconditionally executed the condition
    // should change the conditional
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.VectorCell), vt.IntrinsicCellType(ct.ExactIntegerCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
  
  test("procedure proxying (vector-ref) inside a conditional") {
    val signature = signatureFor("""
      (lambda (vec index) 
        (if (vector? vec)
          (vector-ref vec index)))""")

    // The (vector-ref) is conditionally executed, we can't assert anything about our parameters
    assert(signature.fixedArgs === List(vt.IntrinsicCellType(ct.DatumCell), vt.IntrinsicCellType(ct.DatumCell)))
    assert(signature.returnType === Some(vt.IntrinsicCellType(ct.DatumCell)))
  }
}
