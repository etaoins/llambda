package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite
import llambda.compiler._

class ResolveImportDeclSuite extends FunSuite { 
  def bindingFor(scheme : String) = {
    val frontendConfig = FrontendConfig(
      includePath=IncludePath(),
      featureIdentifiers=Set()
    )

    val datum :: Nil = SchemeParser.parseStringAsData(scheme)

    ResolveImportDecl(datum)(new LibraryLoader(platform.Posix64LE), frontendConfig)
  }
  
  test("import all primitives") {
    val binding = bindingFor("(import (llambda internal primitives))")

    assert(binding("set!") === PrimitiveExpressions.Set)
  }
  
  test("import only") {
    val binding = bindingFor("(import (only (llambda internal primitives) set! lambda))")

    assert(binding === Map("set!" -> PrimitiveExpressions.Set,
                           "lambda" -> PrimitiveExpressions.Lambda))
  }
  
  test("import except") {
    val binding = bindingFor("(import (except (llambda internal primitives) if quote))")

    assert(binding("set!") === PrimitiveExpressions.Set)
    assert(binding("lambda") === PrimitiveExpressions.Lambda)
    assert(binding.get("if") === None)
    assert(binding.get("quote") === None)
  }

  test("import prefix") {
    val binding = bindingFor("(import (prefix (llambda internal primitives) core-))")

    assert(binding("core-set!") === PrimitiveExpressions.Set)
    assert(binding("core-lambda") === PrimitiveExpressions.Lambda)
    assert(binding.get("if") === None)
  }

  test("import rename") {
    val binding = bindingFor("(import (rename (llambda internal primitives) (set! my-set!) (lambda my-lambda)))")

    assert(binding("my-set!") === PrimitiveExpressions.Set)
    assert(binding("my-lambda") === PrimitiveExpressions.Lambda)
    assert(binding.get("my-if") === None)

    intercept[ImportedIdentifierNotFoundException] {
      bindingFor("(import (rename (llambda internal primitives) (set! my-set!) (doesnt-exist really-doesnt-exist)))")
    }
  }

  test("nested import sets") {
    val binding = bindingFor("(import (prefix (rename (only (llambda internal primitives) set! lambda) (set! setter)) imported-))")

    assert(binding === Map("imported-setter" -> PrimitiveExpressions.Set,
                           "imported-lambda" -> PrimitiveExpressions.Lambda))
  }
}
