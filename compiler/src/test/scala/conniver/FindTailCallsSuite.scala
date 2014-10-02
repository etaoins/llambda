package io.llambda.compiler.conniver
import io.llambda

import io.llambda.compiler.ProcedureSignature
import io.llambda.compiler.{valuetype => vt}
import io.llambda.compiler.planner.PlannedFunction
import io.llambda.compiler.planner.{step => ps}

import org.scalatest.FunSuite

class FindTailCallsSuite extends FunSuite {
  val testSignature = ProcedureSignature( 
    hasWorldArg=false,
    hasSelfArg=false,
    fixedArgTypes=List(),
    restArgMemberTypeOpt=None,
    returnType=vt.ReturnType.SingleValue(vt.UnitType),
    attributes=Set()
  )

  def findTailCalls(steps : List[ps.Step]) : List[ps.Step] = {
    FindTailCalls.conniveSteps(steps)
  }
  
  test("trivial tail call returning void") {
    val entryTemp = ps.EntryPointTemp()
    val retTemp = new ps.TempValue(false)

    val inputSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.Invoke(None, testSignature, entryTemp, Nil),
      ps.Return(None)
    )

    assert(findTailCalls(inputSteps) === List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.TailCall(testSignature, entryTemp, Nil)
    ))
  }

  test("trivial tail call returning value") {
    val entryTemp = ps.EntryPointTemp()
    val retTemp = new ps.TempValue(false)

    val inputSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.Invoke(Some(retTemp), testSignature, entryTemp, Nil),
      ps.Return(Some(retTemp))
    )

    assert(findTailCalls(inputSteps) === List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.TailCall(testSignature, entryTemp, Nil)
    ))
  }
  
  test("tail call followed by futher steps can be converted to tail call") {
    val entryTemp = ps.EntryPointTemp()
    val retTemp = new ps.TempValue(false)
    val otherTemp = new ps.TempValue(true)

    val inputSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.Invoke(Some(retTemp), testSignature, entryTemp, Nil),
      ps.Return(Some(retTemp)),
      ps.CreateBooleanCell(otherTemp, false)
    )

    assert(findTailCalls(inputSteps) === List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.TailCall(testSignature, entryTemp, Nil)
    ))
  }
  
  test("call with intervening step can't be converted to a tail call") {
    val entryTemp = ps.EntryPointTemp()
    val retTemp = new ps.TempValue(false)
    val otherTemp = new ps.TempValue(true)

    val inputSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.Invoke(Some(retTemp), testSignature, entryTemp, Nil),
      ps.CreateBooleanCell(otherTemp, false),
      ps.Return(Some(retTemp))
    )

    assert(findTailCalls(inputSteps) === inputSteps)
  }
  
  test("return with other value cannot be converted to tail call") {
    val entryTemp = ps.EntryPointTemp()
    val retTemp = new ps.TempValue(false)
    val otherTemp = new ps.TempValue(true)

    val inputSteps = List(
      ps.CreateBooleanCell(otherTemp, false),
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_test"),
      ps.Invoke(Some(retTemp), testSignature, entryTemp, Nil),
      ps.Return(Some(otherTemp))
    )

    assert(findTailCalls(inputSteps) === inputSteps)
  }

  test("returning from cond branch can be converted to a tail call") {
    val entryTemp = ps.EntryPointTemp()
    val testTemp = new ps.TempValue(false)
    val retTemp = new ps.TempValue(false)

    val trueResultTemp = new ps.TempValue(true)
    val falseResultTemp = new ps.TempValue(true)

    val inputTrueSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_true_call"),
      ps.Invoke(Some(trueResultTemp), testSignature, entryTemp, Nil)
    )
    
    val inputFalseSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_false_call"),
      ps.Invoke(Some(falseResultTemp), testSignature, entryTemp, Nil)
    )

    val inputSteps = List(
      ps.CreateBooleanCell(testTemp, false),
      ps.CondBranch(retTemp, testTemp, inputTrueSteps, trueResultTemp, inputFalseSteps, falseResultTemp),
      ps.Return(Some(retTemp))
    )

    val expectedTrueSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_true_call"),
      ps.TailCall(testSignature, entryTemp, Nil)
    )
    
    val expectedFalseSteps = List(
      ps.CreateNamedEntryPoint(entryTemp, testSignature, "lliby_false_call"),
      ps.TailCall(testSignature, entryTemp, Nil)
    )

    assert(findTailCalls(inputSteps) === List(
      ps.CreateBooleanCell(testTemp, false),
      ps.CondBranch(retTemp, testTemp, expectedTrueSteps, trueResultTemp, expectedFalseSteps, falseResultTemp),
      // This isn't strictly needed but it's harmless
      ps.Return(Some(retTemp))
    ))
  }
}
