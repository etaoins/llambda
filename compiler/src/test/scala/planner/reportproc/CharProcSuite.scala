package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.ast
import llambda.compiler.planner.PlanHelpers
import org.scalatest.FunSuite

class CharProcSuite extends FunSuite with PlanHelpers {
  test("(char->integer)") {
    assertStaticPlan("""(char->integer #\A)""",
      ast.IntegerLiteral(65)
    )
  }

  test("(integer->)") {
    assertStaticPlan("""(integer->char 65)""",
      ast.CharLiteral('A')
    )
  }

  test("(char=?)") {
    assertStaticPlan("""(char=? #\1 #\1 #\1)""",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("""(char=? #\1 #\2 #\3)""",
      ast.BooleanLiteral(false)
    )

   assertStaticPlan("""(char=? #\3 #\2 #\1)""",
      ast.BooleanLiteral(false)
    )
  }

  test("(char<?)") {
    assertStaticPlan("""(char<? #\1 #\1 #\1)""",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("""(char<? #\1 #\2 #\3)""",
      ast.BooleanLiteral(true)
    )

   assertStaticPlan("""(char<? #\3 #\2 #\1)""",
      ast.BooleanLiteral(false)
    )
  }

  test("(char>?)") {
    assertStaticPlan("""(char>? #\1 #\1 #\1)""",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("""(char>? #\1 #\2 #\3)""",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("""(char>? #\3 #\2 #\1)""",
      ast.BooleanLiteral(true)
    )
  }

  test("(char<=?)") {
    assertStaticPlan("""(char<=? #\1 #\1 #\1)""",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("""(char<=? #\1 #\2 #\3)""",
      ast.BooleanLiteral(true)
    )

   assertStaticPlan("""(char<=? #\3 #\2 #\1)""",
      ast.BooleanLiteral(false)
    )
  }

  test("(char>=?)") {
    assertStaticPlan("""(char>=? #\1 #\1 #\1)""",
      ast.BooleanLiteral(true)
    )

    assertStaticPlan("""(char>=? #\1 #\2 #\3)""",
      ast.BooleanLiteral(false)
    )

    assertStaticPlan("""(char>=? #\3 #\2 #\1)""",
      ast.BooleanLiteral(true)
    )
  }
}
