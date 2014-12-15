package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite
import llambda.compiler.DivideByZeroException

class ArithmeticProcSuite extends FunSuite with PlanHelpers {
  test("static (+)") {
    assertStaticPlan("(+)",
      ast.IntegerLiteral(0)
    )

    assertStaticPlan("(+ 56)",
      ast.IntegerLiteral(56)
    )

    assertStaticPlan("(+ (+ 66) (+ -10))",
      ast.IntegerLiteral(56)
    )

    assertStaticPlan("(+ 56 -6)",
      ast.IntegerLiteral(50)
    )

    assertStaticPlan("(+ 56 (+ -3 -3))",
      ast.IntegerLiteral(50)
    )

    assertStaticPlan("(+ 56.5 -6.5)",
      ast.FlonumLiteral(50.0)
    )

    assertStaticPlan("(+ 100.0 200.0)",
      ast.FlonumLiteral(300.0)
    )
  }

  test("static (-)") {
    assertStaticPlan("(- 5)",
      ast.IntegerLiteral(-5)
    )

    assertStaticPlan("(- 200 50)",
      ast.IntegerLiteral(150)
    )

    assertStaticPlan("(- 200 50.0)",
      ast.FlonumLiteral(150.0)
    )
  }

  test("static (*)") {
    assertStaticPlan("(*)",
      ast.IntegerLiteral(1)
    )

    assertStaticPlan("(* 56)",
      ast.IntegerLiteral(56)
    )

    assertStaticPlan("(* (* 7) (* 8))",
      ast.IntegerLiteral(56)
    )

    assertStaticPlan("(* 5 -6)",
      ast.IntegerLiteral(-30)
    )

    assertStaticPlan("(* 0.5 -6)",
      ast.FlonumLiteral(-3.0)
    )
  }

  test("static (/)") {
    assertStaticPlan("(/ 0.25)",
      ast.FlonumLiteral(4.0)
    )

    assertStaticPlan("(/ 10 2)",
      ast.IntegerLiteral(5)
    )

    assertStaticPlan("(/ 0.5 -0.125)",
      ast.FlonumLiteral(-4.0)
    )

    assertStaticPlan("(/ 200.0 20.0 5.0 0.25)",
      ast.FlonumLiteral(8.0)
    )
  }

  test("static (truncate-quotient)") {
    assertStaticPlan("(truncate-quotient 5 2)",
      ast.IntegerLiteral(2)
    )

    assertStaticPlan("(truncate-quotient -5 2)",
      ast.IntegerLiteral(-2)
    )

    assertStaticPlan("(truncate-quotient 5 -2)",
      ast.IntegerLiteral(-2)
    )

    assertStaticPlan("(truncate-quotient -5 -2)",
      ast.IntegerLiteral(2)
    )

    intercept[DivideByZeroException] {
      planStepsFor("(truncate-quotient 5 0)")
    }
  }

  test("static (truncate-remainder)") {
    assertStaticPlan("(truncate-remainder 5 2)",
      ast.IntegerLiteral(1)
    )

    assertStaticPlan("(truncate-remainder -5 2)",
      ast.IntegerLiteral(-1)
    )

    assertStaticPlan("(truncate-remainder 5 -2)",
      ast.IntegerLiteral(1)
    )

    assertStaticPlan("(truncate-remainder -5 -2)",
      ast.IntegerLiteral(-1)
    )

    intercept[DivideByZeroException] {
      planStepsFor("(truncate-remainder 5 0)")
    }
  }

  test("static (expt) for powers of two") {
    assertStaticPlan("(expt 2 0)",
      ast.IntegerLiteral(1)
    )

    assertStaticPlan("(expt 2 1)",
      ast.IntegerLiteral(2)
    )

    assertStaticPlan("(expt 2 8)",
      ast.IntegerLiteral(256)
    )

    assertStaticPlan("(expt 2 16)",
      ast.IntegerLiteral(65536)
    )

    assertStaticPlan("(expt 2 32)",
      ast.IntegerLiteral(4294967296L)
    )

    assertStaticPlan("(expt 2 62)",
      ast.IntegerLiteral(4611686018427387904L)
    )
  }
}
