package io.llambda.compiler.platform
import io.llambda

import org.scalatest.FunSuite

class DetectLlvmTargetSuite extends FunSuite {
  test("x86_64-apple-macosx10.10.0") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"""" + "\n" +
      """target triple = "x86_64-apple-macosx10.10.0"""" + "\n"
    )

    assert(targetPlatform === Some(Posix64LE))
  }

  test("i386-apple-macosx10.10.0") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-m:o-p:32:32-f64:32:64-f80:128-n8:16:32-S128"""" + "\n" +
      """target triple = "i386-apple-macosx10.10.0"""" + "\n"
    )

    assert(targetPlatform === Some(Posix32LE))
  }

  test("armv7-unknown-linux-gnueabihf") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-n32-S64"""" + "\n" +
      """target triple = "armv7-unknown-linux-gnueabihf"""" + "\n"
    )

    assert(targetPlatform === Some(Posix32LE))
  }

  test("x86_64-pc-linux-gnu") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"""" + "\n" +
      """target triple = "x86_64-pc-linux-gnu"""" + "\n"
    )

    assert(targetPlatform === Some(Posix64LE))
  }

  test("x86_64-unknown-freebsd10.1") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"""" + "\n" +
      """target triple = "x86_64-unknown-freebsd10.1"""" + "\n"
    )

    assert(targetPlatform === Some(Posix64LE))
  }

  test("i386-unknown-freebsd10.1") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"""" + "\n" +
      """target triple = "i386-unknown-freebsd10.1"""" + "\n"
    )

    assert(targetPlatform === Some(Posix32LE))
  }

  test("x86_64-pc-windows-msvc") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-m:w-i64:64-f80:128-n8:16:32:64-S128"""" + "\n" +
      """target triple = "x86_64-pc-windows-msvc"""" + "\n"
    )

    assert(targetPlatform === Some(Win64))
  }

  test("i686-pc-windows-gnu") {
    val targetPlatform = DetectLlvmTarget.fromLlvmIr(
      """target datalayout = "e-m:w-p:32:32-i64:64-f80:32-n8:16:32-S32"""" + "\n" +
      """target triple = "i686-pc-windows-gnu"""" + "\n"
    )

    assert(targetPlatform === Some(Win32))
  }
}
