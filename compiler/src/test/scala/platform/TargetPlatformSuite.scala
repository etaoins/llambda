package io.llambda.compiler.platform
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{valuetype => vt}

class TargetPlatformSuite extends FunSuite {
  test("bytesForType for integers") {
    // This shouldn't be platform-specifc
    val targetPlatform = ExamplePlatform.`x86_64-pc-linux-gnu`

    assert(targetPlatform.bytesForType(vt.Predicate) === 1)
    assert(targetPlatform.bytesForType(vt.Int8) === 1)
    assert(targetPlatform.bytesForType(vt.Int16) === 2)
    assert(targetPlatform.bytesForType(vt.Int32) === 4)
    assert(targetPlatform.bytesForType(vt.Int64) === 8)
  }

  test("bytesForType for floating point") {
    // This shouldn't be platform-specifc
    val targetPlatform = ExamplePlatform.`x86_64-pc-linux-gnu`

    assert(targetPlatform.bytesForType(vt.Float) === 4)
    assert(targetPlatform.bytesForType(vt.Double) === 8)
  }

  test("bytesForType for pointers") {
    val targetPlatform64 = ExamplePlatform.`x86_64-pc-linux-gnu`
    val targetPlatform32 = ExamplePlatform.`armv7-unknown-linux-gnueabihf`

    assert(targetPlatform64.bytesForType(vt.StringType) === 8)
    assert(targetPlatform32.bytesForType(vt.StringType) === 4)
  }
}
