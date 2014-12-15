package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class NumberProcSuite extends FunSuite with PlanHelpers {
  test("static (=)") {
    assertStaticPlan("(= 50 50)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(= 50 200)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(= 200 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(= 50.0 50.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(= 50.0 200.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(= 200.0 50.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(= 50 50.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(= 50.0 50)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(= 50.0 50 50.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(= 50.0 50 50)",
      ast.BooleanLiteral(true)
    )
  }

  test("static (<)") {
    assertStaticPlan("(< 50 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(< 50 200)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(< 200 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(< 50.0 50.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(< 50.0 200.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(< 200.0 50.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(< 50.0 200.0 500)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(< 200.0 50.0 10)",
      ast.BooleanLiteral(false)
    )
  }

  test("static (<=)") {
    assertStaticPlan("(<= 50 50)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(<= 50 200)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(<= 200 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(<= 50.0 50.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(<= 50.0 200.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(<= 200.0 50.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(<= 50.0 200.0 200)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(<= 200.0 50.0 -100)",
      ast.BooleanLiteral(false)
    )
  }

  test("static (>)") {
    assertStaticPlan("(> 50 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(> 50 200)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(> 200 50)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(> 50.0 50.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(> 50.0 200.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(> 200.0 50.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(> 200.0 50.0 0.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(> 200.0 50.0 200.0)",
      ast.BooleanLiteral(false)
    )
  }

  test("static (>=)") {
    assertStaticPlan("(>= 50 50)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(>= 50 200)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(>= 200 50)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(>= 50.0 50.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(>= 50.0 200.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(>= 200.0 50.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(>= 200.0 50.0 0.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(>= 200.0 50.0 200.0)",
      ast.BooleanLiteral(false)
    )
  }

  test("static (exact)") {
    assertStaticPlan("(exact 50)",
      ast.IntegerLiteral(50)
    )

    assertStaticPlan("(exact -60.0)",
      ast.IntegerLiteral(-60)
    )
  }

  test("static (inexact)") {
    assertStaticPlan("(inexact 50)",
      ast.FlonumLiteral(50.0)
    )

    assertStaticPlan("(inexact -60.0)",
      ast.FlonumLiteral(-60.0)
    )
  }

  test("static (positive?)") {
    assertStaticPlan("(positive? 50)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(positive? 0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(positive? -200.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(positive? +nan.0)",
      ast.BooleanLiteral(false)
    )
  }

  test("static (negative?)") {
    assertStaticPlan("(negative? 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(negative? 0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(negative? -200.0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(negative? +nan.0)",
      ast.BooleanLiteral(false)
    )
  }

  test("static (zero?)") {
    assertStaticPlan("(zero? 50)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(zero? 0)",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("(zero? -200.0)",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("(zero? +nan.0)",
      ast.BooleanLiteral(false)
    )
  }

  test("static (min)") {
    assertStaticPlan("(min 2 3 1 -2)",
      ast.IntegerLiteral(-2)
    )

    assertStaticPlan("(min 2.0 3.0 1.0 -2.0)",
      ast.FlonumLiteral(-2.0)
    )

    assertStaticPlan("(min 2 3.0 1 -2)",
      ast.FlonumLiteral(-2.0)
    )
  }

  test("static (max)") {
    assertStaticPlan("(max 2 3 1 -2)",
      ast.IntegerLiteral(3)
    )

    assertStaticPlan("(max 2.0 3.0 1.0 -2.0)",
      ast.FlonumLiteral(3.0)
    )

    assertStaticPlan("(max 2 3 1.0 -2)",
      ast.FlonumLiteral(3.0)
    )
  }
}
