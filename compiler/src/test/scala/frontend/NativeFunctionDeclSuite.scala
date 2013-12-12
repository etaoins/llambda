package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

class NativeFunctionDeclSuite extends FunSuite with testutil.ExpressionHelpers {
  implicit val nfiScope = {
    new Scope(testutil.NfiExports())
  }
  
  test("void native function") {
    assertResult(et.NativeFunction(Nil, false, None, "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" ())""")
    }
  }
  
  test("function returning int8") {
    assertResult(et.NativeFunction(Nil, false, Some(vt.Int8), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () <int8>)""")
    }
  }
  
  test("function returning utf8-cstring") {
    assertResult(et.NativeFunction(Nil, false, Some(vt.Utf8CString), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () <utf8-cstring>)""")
    }
  }
  
  test("function taking int16 and returning int32") {
    assertResult(et.NativeFunction(vt.Int16 :: Nil, false, Some(vt.Int32), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<int16>) <int32>)""")
    }
  }
  
  test("function taking int64, float and returning double") {
    assertResult(et.NativeFunction(vt.Int64 :: vt.Float :: Nil, false, Some(vt.Double), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<int64> <float>) <double>)""")
    }
  }
  
  test("function taking uint16 and returning uint32") {
    assertResult(et.NativeFunction(vt.UInt16 :: Nil, false, Some(vt.UInt32), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<uint16>) <uint32>)""")
    }
  }
  
  test("function taking bool and returning bool") {
    assertResult(et.NativeFunction(vt.CBool :: Nil, false, Some(vt.CBool), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<bool>) <bool>)""")
    }
  }
  
  test("function taking uint8 and returning unicode char") {
    assertResult(et.NativeFunction(vt.Int8 :: Nil, false, Some(vt.UnicodeChar), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<int8>) <unicode-char>)""")
    }
  }
  
  test("function taking a integer cell and returning a rational cell") {
    assertResult(et.NativeFunction(vt.IntrinsicCellType(ct.ExactIntegerCell) :: Nil, false, Some(vt.IntrinsicCellType(ct.InexactRationalCell)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (<exact-integer-cell>) <inexact-rational-cell>)""")
    }
  }

  test("function with only rest arg") {
    assertResult(et.NativeFunction(Nil, true, Some(vt.IntrinsicCellType(ct.DatumCell)), "lliby_vector")) {
      expressionFor("""(native-function "lliby_vector" <list-element-cell> <datum-cell>)""")
    }
  }
  
  test("function with fixed and rest args") {
    assertResult(et.NativeFunction(vt.CBool :: Nil, true, Some(vt.Int32), "lliby_misc")) {
      expressionFor("""(native-function "lliby_misc" (<bool> . <list-element-cell>) <int>)""")
    }
  }
  
  test("function with non-list element rest arg") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_vector" <int64> <datum-cell>)""")
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

