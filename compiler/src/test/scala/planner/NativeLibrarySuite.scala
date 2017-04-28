package io.llambda.compiler.planner
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{NativeStaticLibrary,NativeSystemLibrary}

class NativeLibrarySuite extends FunSuite with PlanHelpers {
  test("no native functions require no libraries") {
    val actualLibraries = nativeLibrariesFor("""
      (define-native-library testlib (static-library "libtest"))
      (define x 1)
    """)

    assert(actualLibraries === Set())
  }

  test("unused native function requires no libraries") {
    val actualLibraries = nativeLibrariesFor("""
      (define-native-library testlib (static-library "libtest"))
      (define unused-func (native-function testlib "lliby_test" (-> <unit>)))
    """)

    assert(actualLibraries === Set())
  }

  test("native function using the system library") {
    val actualLibraries = nativeLibrariesFor("""
      (define used-func (native-function system-library "lliby_test" (-> <unit>)))
      (used-func)
    """)

    assert(actualLibraries === Set(NativeSystemLibrary))
  }

  test("native function using a static library") {
    val actualLibraries = nativeLibrariesFor("""
      (define-native-library testlib (static-library "libtest"))
      (define used-func (native-function testlib "lliby_test_static" (-> <unit>)))
      (used-func)
    """)

    assert(actualLibraries === Set(NativeStaticLibrary("libtest")))
  }
}
