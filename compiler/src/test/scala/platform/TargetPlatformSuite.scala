package io.llambda.compiler.platform
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{valuetype => vt}

class TargetPlatformSuite extends FunSuite {
  test("short is always 16 bits") {
    assert(Posix64LE.shortType === vt.Int16)
    assert(Posix32LE.shortType === vt.Int16)
    assert(Win64.shortType === vt.Int16)
    assert(Win32.shortType === vt.Int16)
    
    assert(Posix64LE.ushortType === vt.UInt16)
    assert(Posix32LE.ushortType === vt.UInt16)
    assert(Win64.ushortType === vt.UInt16)
    assert(Win32.ushortType === vt.UInt16)
  }
  
  test("int is always 32 bits") {
    assert(Posix64LE.intType === vt.Int32)
    assert(Posix32LE.intType === vt.Int32)
    assert(Win64.intType === vt.Int32)
    assert(Win32.intType === vt.Int32)
    
    assert(Posix64LE.uintType === vt.UInt32)
    assert(Posix32LE.uintType === vt.UInt32)
    assert(Win64.uintType === vt.UInt32)
    assert(Win32.uintType === vt.UInt32)
  }
  
  test("long is 64bit on 64bit POSIX, 32bit elsewhere") {
    assert(Posix64LE.longType === vt.Int64)
    assert(Posix32LE.longType === vt.Int32)
    assert(Win64.longType === vt.Int32)
    assert(Win32.longType === vt.Int32)
  }
  
  test("wchar_t is 32bit on POSIX, 16bit on Windows") {
    assert(Posix64LE.wcharType === vt.UInt32)
    assert(Posix32LE.wcharType === vt.UInt32)
    assert(Win64.wcharType === vt.UInt16)
    assert(Win32.wcharType === vt.UInt16)
  }
  
  test("size_t matches pointer width") {
    assert(Posix64LE.sizeType === vt.Int64)
    assert(Posix32LE.sizeType === vt.UInt32)
    assert(Win64.sizeType === vt.Int64)
    assert(Win32.sizeType === vt.UInt32)
  }

  test("bytesForType for integers") {
    // This shouldn't be platform-specifc
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
  
  test("platform feature identifiers") {
    assert(Posix64BE.platformFeatures === Set("posix", "lp64", "big-endian"))
    assert(Posix64LE.platformFeatures === Set("posix", "lp64", "little-endian"))
    assert(Posix32BE.platformFeatures === Set("posix", "ilp32", "big-endian"))
    assert(Posix32LE.platformFeatures === Set("posix", "ilp32", "little-endian"))
    assert(Win64.platformFeatures === Set("windows", "llp64", "little-endian"))
    assert(Win32.platformFeatures === Set("windows", "ilp32", "little-endian"))
  }
}
