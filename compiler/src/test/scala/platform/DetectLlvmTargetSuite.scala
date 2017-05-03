package io.llambda.compiler.platform

import org.scalatest.FunSuite


// This ensures that we can parse the main triples we care about. The data layout parsing is separately tested in our
// LLVM package.
class DetectLlvmTargetSuite extends FunSuite {
  test("x86_64-apple-macosx10.10.0") {
    val targetPlatform = ExamplePlatform.`x86_64-apple-macosx10.10.0`

    assert(targetPlatform.pointerBits === 64)
  }

  test("i386-apple-macosx10.10.0") {
    val targetPlatform = ExamplePlatform.`i386-apple-macosx10.10.0`

    assert(targetPlatform.pointerBits === 32)
  }

  test("armv7-unknown-linux-gnueabihf") {
    val targetPlatform = ExamplePlatform.`armv7-unknown-linux-gnueabihf`

    assert(targetPlatform.pointerBits === 32)
  }

  test("x86_64-pc-linux-gnu") {
    val targetPlatform = ExamplePlatform.`x86_64-pc-linux-gnu`

    assert(targetPlatform.pointerBits === 64)
  }

  test("x86_64-unknown-freebsd10.1") {
    val targetPlatform = ExamplePlatform.`x86_64-unknown-freebsd10.1`

    assert(targetPlatform.pointerBits === 64)
  }

  test("i386-unknown-freebsd10.1") {
    val targetPlatform = ExamplePlatform.`i386-unknown-freebsd10.1`

    assert(targetPlatform.pointerBits === 32)
  }

  test("x86_64-pc-windows-msvc") {
    val targetPlatform = ExamplePlatform.`x86_64-pc-windows-msvc`

    assert(targetPlatform.pointerBits === 64)
  }

  test("i686-pc-windows-gnu") {
    val targetPlatform = ExamplePlatform.`i686-pc-windows-gnu`

    assert(targetPlatform.pointerBits === 32)
  }
}
