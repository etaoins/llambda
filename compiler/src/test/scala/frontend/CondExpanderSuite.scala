package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._

class CondExpanderSuite extends FunSuite {
  private val libraryLoader = new LibraryLoader(platform.Posix64LE)

  private def expansionFor(clauseList: List[String], featureIdentifiers: Set[String]): List[ast.Datum] = {
    val parsedClauses = clauseList.map(SchemeParser.parseStringAsData(_, None).head)

    val frontendConfig = FrontendConfig(
      includePath=IncludePath(Nil),
      featureIdentifiers=featureIdentifiers
    )

    CondExpander.expandData(parsedClauses)(libraryLoader, frontendConfig)
  }

  test("non-proper list clause fails") {
    intercept[BadSpecialFormException] {
      expansionFor(
        List(
          """my-feature"""
        ),
        Set(
          """my-feature"""
        )
      )
    }
  }

  test("empty proper list clause fails") {
    intercept[BadSpecialFormException] {
      expansionFor(
        List(
          """()"""
        ),
        Set(
          """my-feature"""
        )
      )
    }
  }

  test("empty expansion when no conditions match and no else") {
    val data = expansionFor(
      List(
        """(this-is-not-a-feature 1 2 3)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === Nil)
  }

  test("else clause fallback no conditions match") {
    val data = expansionFor(
      List(
        """(this-is-not-a-feature 1 2 3)""",
        """(else 4 5 6)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === List(4, 5, 6).map(ast.Integer(_)))
  }

  test("non-terminal else clause fails") {
    intercept[BadSpecialFormException] {
      expansionFor(
        List(
          """(else 4 5 6)""",
          """(this-is-not-a-feature 1 2 3)"""
        ),
        Set(
          """my-feature"""
        )
      )
    }
  }

  test("trivial feature identifier expansion") {
    val data = expansionFor(
      List(
        """(my-feature 1 2 3)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("first matching clause is expanded") {
    val data = expansionFor(
      List(
        """(feature-1 1 2 3)""",
        """(feature-2 4 3 6)"""
      ),
      Set(
        """feature-1""",
        """feature-2"""
      )
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("trivial fallback feature identifier expansion") {
    val data = expansionFor(
      List(
        """(not-a-feature 1 2 3)""",
        """(my-feature 4 5 6)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === List(4, 5, 6).map(ast.Integer(_)))
  }

  test("trivial existing library expansion") {
    val data = expansionFor(
      List(
        """((library (scheme base)) 1 2 3)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("non-existing library expansion") {
    val data = expansionFor(
      List(
        """((library (not a library)) 1 2 3)""",
        """((library (scheme base)) 4 5 6)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === List(4, 5, 6).map(ast.Integer(_)))
  }

  test("empty (and) condition is true") {
    val data = expansionFor(
      List(
        """((and) 1 2 3)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("(and) with two true requirements") {
    val data = expansionFor(
      List(
        """((and feature-1 feature-2) 1 2 3)"""
      ),
      Set(
        """feature-1""",
        """feature-2"""
      )
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("(and) with one true and one false requirement") {
    val data = expansionFor(
      List(
        """((and feature-1 not-a-feature) 1 2 3)"""
      ),
      Set(
        """feature-1"""
      )
    )

    assert(data === Nil)
  }

  test("empty (or) condition is false") {
    val data = expansionFor(
      List(
        """((or) 1 2 3)"""
      ),
      Set(
        """my-feature"""
      )
    )

    assert(data === Nil)
  }

  test("(or) with two true requirements") {
    val data = expansionFor(
      List(
        """((or feature-1 feature-2) 1 2 3)"""
      ),
      Set(
        """feature-1""",
        """feature-2"""
      )
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("(or) with one true and one false requirement") {
    val data = expansionFor(
      List(
        """((or feature-1 not-a-feature) 1 2 3)"""
      ),
      Set(
        """feature-1"""
      )
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("empty (not) fails") {
    intercept[BadSpecialFormException] {
      expansionFor(
        List(
          """((not) 1 2 3)"""
        ),
        Set(
          """my-feature"""
        )
      )
    }
  }

  test("(not) with true value") {
    val data = expansionFor(
      List(
        """((not feature-1) 1 2 3)"""
      ),
      Set(
        """feature-1"""
      )
    )

    assert(data === Nil)
  }

  test("(not) with false value") {
    val data = expansionFor(
      List(
        """((not not-a-feature) 1 2 3)"""
      ),
      Set()
    )

    assert(data === List(1, 2, 3).map(ast.Integer(_)))
  }

  test("nested conditionals") {
    val data = expansionFor(
      List(
        """((and feature-1 (not (library (scheme base)))) 1 2 3)""",
        """((or (not feature-1) (and feature-2)) 4 5 6)"""
      ),
      Set(
        """feature-1""",
        """feature-2"""
      )
    )

    assert(data === List(4, 5, 6).map(ast.Integer(_)))
  }
}
