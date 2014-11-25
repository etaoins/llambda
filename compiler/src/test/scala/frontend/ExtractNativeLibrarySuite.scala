package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._
import llambda.compiler.valuetype.Implicits._

class ExtractNativeLibrarySuite extends FunSuite with testutil.ExprHelpers {
  val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  val nfiScope = new ImmutableScope(testutil.NfiExports(), Some(primitiveScope))

  test("simple static library definition") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("""(define-native-library corelib (static-library "llcore"))""")(scope)

    assert(scope("corelib") === NativeStaticLibrary("llcore"))
  }

  test("static library definition without library name fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-native-library corelib (static-library))""")(scope)
    }
  }

  test("static library definition with non-symbol library name fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-native-library corelib (static-library 'llcore))""")(scope)
    }
  }
}
