package io.llambda.compiler.analyser
import io.llambda

import llambda.compiler.{et, ast, StorageLocation, ProcedureSignature}
import llambda.compiler.{valuetype => vt}
import llambda.compiler.NativeSystemLibrary
import org.scalatest.FunSuite

class AnalyseExprsSuite extends FunSuite {
  test("literals have no vars") {
    val testExprs = List(
      et.Literal(ast.StringLiteral("Hello, world"))
    )

    assert(AnalyseExprs(testExprs) === AnalysedExprs(
      usedTopLevelExprs=testExprs
    ))
  }

  test("immutable let") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true))),
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(false)))
        )
      ),
      et.VarRef(testLocA),
      et.VarRef(testLocB)
    )
    
    assert(AnalyseExprs(testExprs) === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(),
      constantTopLevelBindings=List(
        (testLocA -> et.Literal(ast.BooleanLiteral(true))),
        (testLocA -> et.Literal(ast.BooleanLiteral(false)))
      ),
      usedVars=Set(testLocA, testLocB)
    ))
  }
  
  test("native symbols") {
    val testSignature = ProcedureSignature(
      hasWorldArg=true,
      hasSelfArg=false,
      fixedArgTypes=Nil,
      restArgMemberTypeOpt=None,
      returnType=vt.ReturnType.SingleValue(vt.UnitType),
      attributes=Set()
    )

    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")
    val testExprs = List(
      et.TopLevelDefine(List(
        et.SingleBinding(testLocA, et.NativeFunction(
          NativeSystemLibrary,
          testSignature.toPolymorphic,
          "nativeSymbol1"
        )),
        et.SingleBinding(testLocB, et.NativeFunction(
          NativeSystemLibrary,
          testSignature.toPolymorphic,
          "nativeSymbol2"
        ))
      )),
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
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true))),
          et.SingleBinding(testLocB, et.Literal(ast.BooleanLiteral(false)))
        )
      ),
      et.MutateVar(testLocA, et.Literal(ast.EmptyList())),
      et.VarRef(testLocA)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    // We should have dropped testLocB completely
    val expectedUsedExprs = testExprs match {
      case et.TopLevelDefine(bindings) :: tail =>
        et.TopLevelDefine(List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true)))
        )) :: tail

      case _ =>
        throw new Exception("Couldn't parse our own test expressions")
    }

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=expectedUsedExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=Nil,
      usedVars=Set(testLocA)
    ))
  }

  test("unreferenced reference loops aren't considered used") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.VarRef(testLocB)),
          et.SingleBinding(testLocB, et.VarRef(testLocA))
        )
      )
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs())
  }
  
  test("mutate var in one side of conditional makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true)))
        )
      ),
      et.Cond(
        et.Literal(ast.BooleanLiteral(false)),
        et.MutateVar(testLocA, et.Literal(ast.EmptyList())),
        et.Literal(ast.UnitValue())
      ),
      et.VarRef(testLocA)
    )

    val analysedExprs = AnalyseExprs(testExprs)

    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA),
      constantTopLevelBindings=Nil,
      usedVars=Set(testLocA)
    ))
  }
  
  test("mutate var inside lambda parameters make a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List( 
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true)))
        )
      ),
      et.Apply(
        et.Lambda(
          schemeType=vt.ProcedureType(Nil, None, vt.ReturnType.ArbitraryValues),
          fixedArgs=Nil,
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
      usedVars=Set(testLocA)
    ))
  }
  
  test("mutate var inside self-executing lambda makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")

    val testExprs = List(
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true)))
        )
      ),
      et.Apply(
        et.Lambda(
          schemeType=vt.ProcedureType(Nil, None, vt.ReturnType.ArbitraryValues),
          fixedArgs=Nil,
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
      usedVars=Set(testLocA)
    ))
  }
  
  test("mutate var captured inside lambda makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")

    val testExprs = List(
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true)))
        )
      ),
      et.Lambda(
        schemeType=vt.ProcedureType(Nil, None, vt.ReturnType.ArbitraryValues),
        fixedArgs=Nil,
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
      usedVars=Set(testLocA)
    ))
  }

  test("mutate var inside other let makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true)))
        )
      ),
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocB, et.MutateVar(testLocA, et.Literal(ast.EmptyList())))
        )
      ),
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
      usedVars=Set(testLocB)
    ))
  }

  test("mutate var inside other mutate var RHS makes a var mutable") {
    val testLocA = new StorageLocation("testLocA")
    val testLocB = new StorageLocation("testLocB")

    val testExprs = List(
      et.TopLevelDefine(
        List(
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(true))),
          et.SingleBinding(testLocA, et.Literal(ast.BooleanLiteral(false)))
        )
      ),
      et.MutateVar(testLocA, et.MutateVar(testLocB, et.Literal(ast.BooleanLiteral(true))))
    )
    
    val analysedExprs = AnalyseExprs(testExprs)
    
    assert(analysedExprs === AnalysedExprs(
      usedTopLevelExprs=testExprs,
      mutableVars=Set(testLocA, testLocB),
      constantTopLevelBindings=Nil
    ))
  }
}
