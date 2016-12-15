package io.llambda.compiler.analyser
import io.llambda

import llambda.compiler.{et, ast, StorageLocation, ProcedureSignature}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.NativeSystemLibrary
import org.scalatest.FunSuite

class AnalyseExprsSuite extends FunSuite {
  test("literals have no vars") {
    val testExprs = List(
      et.Literal(ast.String("Hello, world"))
    )

    assert(AnalyseExprs(testExprs) === AnalysedExprs(
      usedTopLevelExprs=testExprs
    ))
  }

  test("immutable let") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(false)))),
      et.VarRef(testLocB),
      et.VarRef(testLocA),
      et.VarRef(testLocB)
    )

    assert(AnalyseExprs(testExprs) === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(),
      constantTopLevelBindings=List(
        (testLocA -> et.Literal(ast.Boolean(true))),
        (testLocA -> et.Literal(ast.Boolean(false)))
      ),
      varUses=Map(
        testLocA -> 1,
        testLocB -> 2
      )
    ))
  }

  test("native symbols") {
    val testSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      mandatoryArgTypes=Nil,
      optionalArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.Reachable(vt.UnitType),
      attributes=Set()
    )

    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")
    val testExprs = List(
      et.TopLevelDefine(
        et.Binding(testLocA, et.NativeFunction(
          NativeSystemLibrary,
          testSignature.toPolymorphic,
          "nativeSymbol1"
        ))
      ),
      et.TopLevelDefine(
        et.Binding(testLocB, et.NativeFunction(
          NativeSystemLibrary,
          testSignature.toPolymorphic,
          "nativeSymbol2"
        ))
      ),
      // Apply the procs so their definitions don't get dropped
      et.Apply(et.VarRef(testLocA), Nil),
      et.Apply(et.VarRef(testLocB), Nil)
    )

    val analysis = AnalyseExprs(testExprs)

    assert(analysis.nativeSymbols === Set("nativeSymbol1", "nativeSymbol2"))
  }

  test("direct mutate var makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.TopLevelDefine(et.Binding(testLocB, et.Literal(ast.Boolean(false)))),
      et.MutateVar(testLocA, et.Literal(ast.EmptyList())),
      et.VarRef(testLocA)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    // We should have dropped testLocB completely
    val expectedUsedExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.MutateVar(testLocA, et.Literal(ast.EmptyList())),
      et.VarRef(testLocA)
    )

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=expectedUsedExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=Nil,
      varUses=Map(
        testLocA -> 1
      )
    ))
  }

  test("unreferenced reference loops aren't considered used") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.VarRef(testLocB))),
      et.TopLevelDefine(et.Binding(testLocB, et.VarRef(testLocA)))
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs())
  }

  test("mutate var in one side of conditional makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.Cond(
        et.Literal(ast.Boolean(false)),
        et.MutateVar(testLocA, et.Literal(ast.EmptyList())),
        et.Literal(ast.Unit())
      ),
      et.VarRef(testLocA)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=Nil,
      varUses=Map(
        testLocA -> 1
      )
    ))
  }

  test("mutate var inside lambda parameters make a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.Apply(
        et.Lambda(
          polyType=vt.ProcedureType(Nil, Nil, None, vt.ReturnType.Reachable(vt.AnySchemeType)).toPolymorphic,
          mandatoryArgs=Nil,
          optionalArgs=Nil,
          restArgOpt=None,
          body=et.Begin(Nil)
        ),
        List(
          et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
        )
      ),
      et.VarRef(testLocA)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=Nil,
      varUses=Map(
        testLocA -> 1
      )
    ))
  }

  test("mutate var inside self-executing lambda makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.Apply(
        et.Lambda(
          polyType=vt.ProcedureType(Nil, Nil, None, vt.ReturnType.Reachable(vt.AnySchemeType)).toPolymorphic,
          mandatoryArgs=Nil,
          optionalArgs=Nil,
          restArgOpt=None,
          body=et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
        ),
        Nil
      ),
      et.VarRef(testLocA)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=Nil,
      varUses=Map(
        testLocA -> 1
      )
    ))
  }

  test("mutate var captured inside lambda makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.Lambda(
        polyType=vt.ProcedureType(Nil, Nil, None, vt.ReturnType.Reachable(vt.AnySchemeType)).toPolymorphic,
        mandatoryArgs=Nil,
        optionalArgs=Nil,
        restArgOpt=None,
        body=et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
      ),
      et.VarRef(testLocA)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=Nil,
      varUses=Map(
        testLocA -> 1
      )
    ))
  }

  test("mutate var inside other let makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.TopLevelDefine(et.Binding(testLocB, et.MutateVar(testLocA, et.Literal(ast.EmptyList())))),
      // Reference testLocB so testLocA becomes mutable
      et.VarRef(testLocB)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=List(
        testLocB -> et.MutateVar(testLocA, et.Literal(ast.EmptyList()))
      ),
      varUses=Map(
        testLocB -> 1
      )
    ))
  }

  test("mutate var inside other mutate var RHS makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(false)))),
      et.MutateVar(testLocA, et.MutateVar(testLocB, et.Literal(ast.Boolean(true))))
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA, testLocB),
      constantTopLevelBindings=Nil
    ))
  }

  test("parameterize") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(true)))),
      et.TopLevelDefine(et.Binding(testLocA, et.Literal(ast.Boolean(false)))),
      et.Parameterize(
        parameterValues=List((et.VarRef(testLocA), et.VarRef(testLocA))),
        body=et.VarRef(testLocB)
      )
    )

    assert(AnalyseExprs(testExprs) === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(),
      constantTopLevelBindings=List(
        (testLocA -> et.Literal(ast.Boolean(true))),
        (testLocA -> et.Literal(ast.Boolean(false)))
      ),
      varUses=Map(
        testLocA -> 2,
        testLocB -> 1
      ),
      parameterized=true
    ))
  }
}
