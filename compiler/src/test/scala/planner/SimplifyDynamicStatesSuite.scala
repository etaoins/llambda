package io.llambda.compiler.planner
import io.llambda

import org.scalatest.{FunSuite, Inside}

import llambda.compiler.{StorageLocation, StdlibProcedure}

import llambda.compiler.et
import llambda.compiler.ast
import llambda.compiler.{valuetype => vt}


class SimplifyDynamicStatesSuite extends FunSuite with Inside {
  private val makeParameterExpr = et.VarRef(new StdlibProcedure("make-parameter"))
  private val oneExpr = et.Literal(ast.Integer(1))
  private val twoExpr = et.Literal(ast.Integer(2))
  private val threeExpr = et.Literal(ast.Integer(3))
  private val fourExpr = et.Literal(ast.Integer(4))

  test("immediate parameter application") {
    val inputExprs = List(
      et.Apply(et.Apply(makeParameterExpr, List(oneExpr)), Nil)
    )

    val expectedExprs = List(
      oneExpr
    )

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("no parameter usage") {
    val inputExprs = List(
      oneExpr
    )

    val expectedExprs = inputExprs

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("immediate parameter application with too many parameter operands") {
    val inputExprs = List(
      et.Apply(et.Apply(makeParameterExpr, List(oneExpr)), List(oneExpr))
    )

    val expectedExprs = inputExprs

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("immediate parameter application with too many (make-parameter) operands") {
    val inputExprs = List(
      et.Apply(et.Apply(makeParameterExpr, List(oneExpr, oneExpr)), Nil)
    )

    val expectedExprs = inputExprs

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("top-level define where parameter is unused") {
    val paramLoc = new StorageLocation("<test>")

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr))))
    )

    val expectedExprs = List(
      oneExpr
    )

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("top-level define where parameter is unused but could fail type checking") {
    val paramLoc = new StorageLocation("<test>", vt.IntegerType)

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr))))
    )

    val expectedExprs = inputExprs

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("top-level define where parameter is only applied") {
    val paramLoc = new StorageLocation("<test>")

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr)))),
      et.Apply(et.VarRef(paramLoc), Nil)
    )

    val resultExprs = SimplifyDynamicStates(inputExprs)

    inside(resultExprs) {
      case List(
        et.TopLevelDefine(et.Binding(bindingLoc, `oneExpr`)),
        et.VarRef(refLoc)
      ) if bindingLoc == refLoc =>
    }
  }

  test("top-level define where parameter is only parameterized") {
    val paramLoc = new StorageLocation("<test>")

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr))))
    )

    val expectedExprs = List(
      oneExpr
    )

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("top-level define of parameter followed by a top-level define of its value") {
    val paramLoc = new StorageLocation("<test>")
    val valueLoc = new StorageLocation("<value>")

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr)))),
      et.TopLevelDefine(et.Binding(valueLoc, et.Apply(et.VarRef(paramLoc), Nil))),
    )

    val resultExprs = SimplifyDynamicStates(inputExprs)

    inside(resultExprs) {
      case List(
        et.TopLevelDefine(et.Binding(paramBindingLoc, `oneExpr`)),
        et.TopLevelDefine(et.Binding(`valueLoc`, et.VarRef(paramValueLoc)))
      ) if paramBindingLoc == paramValueLoc =>
    }
  }

  test("internal define where all parameters are unused") {
    val paramLoc1 = new StorageLocation("<param-1>")
    val paramLoc2 = new StorageLocation("<param-2>")

    val inputBindings = List(
      et.Binding(paramLoc1, et.Apply(makeParameterExpr, List(oneExpr))),
      et.Binding(paramLoc2, et.Apply(makeParameterExpr, List(twoExpr)))
    )

    val inputExprs = List(
      et.InternalDefine(inputBindings, threeExpr)
    )

    val expectedExprs = List(et.Begin(List(
      oneExpr,
      twoExpr,
      threeExpr
    )))

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("parameterize where all parameters are parameterized only") {
    val paramLoc = new StorageLocation("<test>")

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr)))),
      et.Parameterize(List((et.VarRef(paramLoc), twoExpr)), threeExpr)
    )

    val expectedExprs = List(
      oneExpr,
      et.Begin(List(twoExpr, threeExpr))
    )

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("parameterize where a parameter is mutated") {
    val paramLoc = new StorageLocation("<test>")

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr)))),
      et.MutateVar(paramLoc, oneExpr),
      et.Parameterize(List((et.VarRef(paramLoc), twoExpr)), threeExpr)
    )

    val expectedExprs = inputExprs

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("parameterize where a parameter is applied") {
    val paramLoc = new StorageLocation("<test>")

    val inputExprs = List(
      et.TopLevelDefine(et.Binding(paramLoc, et.Apply(makeParameterExpr, List(oneExpr)))),
      et.Parameterize(
        List((et.VarRef(paramLoc), twoExpr)),
        et.Apply(et.VarRef(paramLoc), Nil)
      )
    )

    val expectedExprs = inputExprs

    assertResult(expectedExprs) {
      SimplifyDynamicStates(inputExprs)
    }
  }

  test("internal define with parameterize") {
    // Unused
    val paramLoc1 = new StorageLocation("<param-1>")
    // Applied only
    val paramLoc2 = new StorageLocation("<param-2>")
    // Non-trivial usage
    val paramLoc3 = new StorageLocation("<param-3>")
    // Parameterized only
    val paramLoc4 = new StorageLocation("<param-4>")
    // An other storage locaiton
    val otherLoc = new StorageLocation("<other>")

    val inputBindings = List(
      et.Binding(paramLoc1, et.Apply(makeParameterExpr, List(oneExpr))),
      et.Binding(paramLoc2, et.Apply(makeParameterExpr, List(twoExpr))),
      et.Binding(paramLoc3, et.Apply(makeParameterExpr, List(threeExpr))),
      et.Binding(paramLoc4, et.Apply(makeParameterExpr, List(fourExpr))),
      et.Binding(otherLoc, oneExpr)
    )

    val inputExprs = List(
      et.InternalDefine(inputBindings, et.Parameterize(
        List(
          (et.VarRef(paramLoc3), threeExpr),
          (et.VarRef(paramLoc4), fourExpr),
          (fourExpr, fourExpr)
        ),
        et.Begin(List(
          et.Apply(et.VarRef(paramLoc2), Nil),
          et.VarRef(paramLoc3)
        ))
      ))
    )

    val resultExprs = SimplifyDynamicStates(inputExprs)

    inside(resultExprs) {
      case List(et.Begin(List(
        `oneExpr`,
        `fourExpr`,
        et.InternalDefine(
          List(
            et.Binding(bindingLoc2, `twoExpr`),
            et.Binding(`paramLoc3`, et.Apply(`makeParameterExpr`, List(`threeExpr`))),
            et.Binding(`otherLoc`, `oneExpr`)
          ),
          et.Begin(List(
            `fourExpr`,
            et.Parameterize(
              List(
                (et.VarRef(`paramLoc3`), threeExpr),
                (`fourExpr`, `fourExpr`)
              ),
              et.Begin(List(
                et.VarRef(valueLoc2),
                et.VarRef(`paramLoc3`)
              ))
            )
          ))
        )))) if bindingLoc2 == valueLoc2 =>
    }
  }
}
