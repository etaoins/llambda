package llambda.frontend

import org.scalatest.FunSuite
import llambda._

class ResolveImportDeclSuite extends FunSuite { 
  def bindingFor(scheme : String) = {
    SchemeParser(scheme) match {
      case SchemeParser.Success(datum :: Nil, _) =>
        ResolveImportDecl(datum)(testutil.TestLibraryLoader.apply)
      case err =>
        fail(err.toString)
    }
  }
  
  test("import all primitives") {
    val binding = bindingFor("(import (test primitives))")

    assert(binding("set!") === SchemePrimitives.Set)
  }
  
  test("import only") {
    val binding = bindingFor("(import (only (test primitives) set! lambda))")

    assert(binding === Map("set!" -> SchemePrimitives.Set,
                           "lambda" -> SchemePrimitives.Lambda))
  }
  
  test("import except") {
    val binding = bindingFor("(import (except (test primitives) if quote))")

    assert(binding("set!") === SchemePrimitives.Set)
    assert(binding("lambda") === SchemePrimitives.Lambda)
    assert(binding.get("if") === None)
    assert(binding.get("quote") === None)
  }

  test("import prefix") {
    val binding = bindingFor("(import (prefix (test primitives) core-))")

    assert(binding("core-set!") === SchemePrimitives.Set)
    assert(binding("core-lambda") === SchemePrimitives.Lambda)
    assert(binding.get("if") === None)
  }

  test("import rename") {
    val binding = bindingFor("(import (rename (test primitives) (set! my-set!) (lambda my-lambda)))")

    assert(binding("my-set!") === SchemePrimitives.Set)
    assert(binding("my-lambda") === SchemePrimitives.Lambda)
    assert(binding.get("my-if") === None)

    intercept[ImportedIdentifierNotFoundException] {
      bindingFor("(import (rename (test primitives) (set! my-set!) (doesnt-exist really-doesnt-exist)))")
    }
  }

  test("nested import sets") {
    val binding = bindingFor("(import (prefix (rename (only (test primitives) set! lambda) (set! setter)) imported-))")

    assert(binding === Map("imported-setter" -> SchemePrimitives.Set,
                           "imported-lambda" -> SchemePrimitives.Lambda))
  }
}
