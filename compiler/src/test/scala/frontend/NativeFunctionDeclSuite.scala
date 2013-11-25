package llambda.frontend

import org.scalatest.FunSuite

import llambda._
import llambda.{boxedtype => bt}
import llambda.{valuetype => vt}

class NativeFunctionDeclSuite extends FunSuite with testutil.ExpressionHelpers {
  implicit val nfiScope = new Scope(collection.mutable.Map(NativeFunctionPrimitives.bindings.toSeq : _*))
  
  test("void native function") {
    assertResult(et.NativeFunction(Nil, false, None, "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" ())""")
    }
  }
  
  test("function returning int8") {
    assertResult(et.NativeFunction(Nil, false, Some(vt.ScalarType(nfi.Int8)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () <int8>)""")
    }
  }
  
  test("function returning utf8-cstring") {
    assertResult(et.NativeFunction(Nil, false, Some(vt.ScalarType(nfi.Utf8CString)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () <utf8-cstring>)""")
    }
  }
  
  test("function taking int16 and returning int32") {
    assertResult(et.NativeFunction(vt.ScalarType(nfi.Int16) :: Nil, false, Some(vt.ScalarType(nfi.Int32)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<int16>) <int32>)""")
    }
  }
  
  test("function taking int64, float and returning double") {
    assertResult(et.NativeFunction(vt.ScalarType(nfi.Int64) :: vt.ScalarType(nfi.Float) :: Nil, false, Some(vt.ScalarType(nfi.Double)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<int64> <float>) <double>)""")
    }
  }
  
  test("function taking uint16 and returning uint32") {
    assertResult(et.NativeFunction(vt.ScalarType(nfi.UInt16) :: Nil, false, Some(vt.ScalarType(nfi.UInt32)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<uint16>) <uint32>)""")
    }
  }
  
  test("function taking bool and returning bool") {
    assertResult(et.NativeFunction(vt.ScalarType(nfi.CBool) :: Nil, false, Some(vt.ScalarType(nfi.CBool)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<bool>) <bool>)""")
    }
  }
  
  test("function taking uint8 and returning unicode char") {
    assertResult(et.NativeFunction(vt.ScalarType(nfi.Int8) :: Nil, false, Some(vt.ScalarType(nfi.UnicodeChar)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<int8>) <unicode-char>)""")
    }
  }
  
  test("function taking a boxed integer and returning a boxed rational") {
    assertResult(et.NativeFunction(vt.BoxedValue(bt.BoxedExactInteger) :: Nil, false, Some(vt.BoxedValue(bt.BoxedInexactRational)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<boxed-exact-integer>) <boxed-inexact-rational>)""")
    }
  }

  test("function with only rest arg") {
    assertResult(et.NativeFunction(Nil, true, Some(vt.BoxedValue(bt.BoxedDatum)), "lliby_vector")) {
      expressionFor("""(native-function "lliby_vector" <boxed-list-element> <boxed-datum>)""")
    }
  }
  
  test("function with fixed and rest args") {
    assertResult(et.NativeFunction(vt.ScalarType(nfi.CBool) :: Nil, true, Some(vt.ScalarType(nfi.Int32)), "lliby_misc")) {
      expressionFor("""(native-function "lliby_misc" (<bool> . <boxed-list-element>) <int>)""")
    }
  }
  
  test("function with non-list element rest arg") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_vector" <int64> <boxed-datum>)""")
    }
  }
  
  test("function returning unknown type") {
    intercept[UnboundVariableException] {
      expressionFor("""(native-function "lliby_newline" () <not-a-type>)""")
    }
  }
  
  test("function returning non-symbol") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" () 4)""")
    }
  }
  
  test("function taking unknown type") {
    intercept[UnboundVariableException] {
      expressionFor("""(native-function "lliby_newline" (<not-a-type>))""")
    }
  }
  
  test("function taking non-symbol") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" (4))""")
    }
  }
}

