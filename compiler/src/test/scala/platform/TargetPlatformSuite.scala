package io.llambda.compiler.platform
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{valuetype => vt}

class TargetPlatformSuite extends FunSuite {
  test("bytesForType for integers") {
    // This shouldn't be platform-specifc
    assert(Posix64LE.bytesForType(vt.Predicate) === 1)
    assert(Posix64LE.bytesForType(vt.Int8) === 1)
    assert(Posix64LE.bytesForType(vt.Int16) === 2)
    assert(Posix64LE.bytesForType(vt.Int32) === 4)
    assert(Posix64LE.bytesForType(vt.Int64) === 8)
  }

  test("bytesForType for floating point") {
    // This shouldn't be platform-specifc
    assert(Posix64LE.bytesForType(vt.Float) === 4)
    assert(Posix64LE.bytesForType(vt.Double) === 8)
  }

  test("bytesForType for pointers") {
    assert(Posix64LE.bytesForType(vt.StringType) === 8)
    assert(Posix32LE.bytesForType(vt.StringType) === 4)
    assert(Win64.bytesForType(vt.StringType) === 8)
    assert(Win32.bytesForType(vt.StringType) === 4)
  }
}
