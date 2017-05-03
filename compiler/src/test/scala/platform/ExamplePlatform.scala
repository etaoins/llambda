package io.llambda.compiler.platform


object ExamplePlatform {
  lazy val `x86_64-apple-macosx10.10.0` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"""" + "\n" +
    """target triple = "x86_64-apple-macosx10.10.0"""" + "\n"
  )

  lazy val `i386-apple-macosx10.10.0` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-m:o-p:32:32-f64:32:64-f80:128-n8:16:32-S128"""" + "\n" +
    """target triple = "i386-apple-macosx10.10.0"""" + "\n"
  )

  lazy val `armv7-unknown-linux-gnueabihf` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-m:e-p:32:32-i64:64-v128:64:128-n32-S64"""" + "\n" +
    """target triple = "armv7-unknown-linux-gnueabihf"""" + "\n"
  )

  lazy val `x86_64-pc-linux-gnu` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"""" + "\n" +
    """target triple = "x86_64-pc-linux-gnu"""" + "\n"
  )

  lazy val `x86_64-unknown-freebsd10.1` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"""" + "\n" +
    """target triple = "x86_64-unknown-freebsd10.1"""" + "\n"
  )

  lazy val `i386-unknown-freebsd10.1` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32-S128"""" + "\n" +
    """target triple = "i386-unknown-freebsd10.1"""" + "\n"
  )

  lazy val `x86_64-pc-windows-msvc` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-m:w-i64:64-f80:128-n8:16:32:64-S128"""" + "\n" +
    """target triple = "x86_64-pc-windows-msvc"""" + "\n"
  )

  lazy val `i686-pc-windows-gnu` = DetectLlvmTarget.fromLlvmIr(
    """target datalayout = "e-m:w-p:32:32-i64:64-f80:32-n8:16:32-S32"""" + "\n" +
    """target triple = "i686-pc-windows-gnu"""" + "\n"
  )
}
