package llambda.frontend

import org.scalatest.FunSuite

import llambda._
import llambda.{boxedtype => bt}

class NativeFunctionDeclSuite extends FunSuite with testutil.ExpressionHelpers {
  implicit val nfiScope = new Scope(collection.mutable.Map(NativeFunctionPrimitives.bindings.toSeq : _*))
  
  test("void native function") {
    assertResult(et.NativeFunction(Nil, false, None, "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () void)""")
    }
  }
  
  test("function returning int8") {
    assertResult(et.NativeFunction(Nil, false, Some(nfi.Int8), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () int8)""")
    }
  }
  
  test("function returning utf8-cstring") {
    assertResult(et.NativeFunction(Nil, false, Some(nfi.Utf8CString), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () utf8-cstring)""")
    }
  }
  
  test("function taking int16 and returning int32") {
    assertResult(et.NativeFunction(nfi.Int16 :: Nil, false, Some(nfi.Int32), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (int16) int32)""")
    }
  }
  
  test("function taking int64, float and returning double") {
    assertResult(et.NativeFunction(nfi.Int64 :: nfi.Float :: Nil, false, Some(nfi.Double), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (int64 float) double)""")
    }
  }
  
  test("function taking uint16 and returning uint32") {
    assertResult(et.NativeFunction(nfi.UInt16 :: Nil, false, Some(nfi.UInt32), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (uint16) uint32)""")
    }
  }
  
  test("function taking strict-bool and returning bool") {
    assertResult(et.NativeFunction(nfi.CStrictBool :: Nil, false, Some(nfi.CStrictBool), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (strict-bool) bool)""")
    }
  }
  
  test("function taking truthy-bool and returning bool") {
    assertResult(et.NativeFunction(nfi.CTruthyBool :: Nil, false, Some(nfi.CStrictBool), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (truthy-bool) bool)""")
    }
  }
  
  test("function taking uint8 and returning unicode char") {
    assertResult(et.NativeFunction(nfi.Int8 :: Nil, false, Some(nfi.UnicodeChar), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (int8) unicode-char)""")
    }
  }
  
  test("function taking a boxed integer and returning a boxed rational") {
    assertResult(et.NativeFunction(nfi.BoxedValue(bt.BoxedExactInteger) :: Nil, false, Some(nfi.BoxedValue(bt.BoxedInexactRational)), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (boxed-exact-integer) boxed-inexact-rational)""")
    }
  }

  test("function with only rest arg") {
    assertResult(et.NativeFunction(Nil, true, Some(nfi.BoxedValue(bt.BoxedDatum)), "lliby_vector")) {
      expressionFor("""(native-function "lliby_vector" boxed-list-element boxed-datum)""")
    }
  }
  
  test("function with fixed and rest args") {
    assertResult(et.NativeFunction(nfi.CStrictBool :: Nil, true, Some(nfi.Int32), "lliby_misc")) {
      expressionFor("""(native-function "lliby_misc" (strict-bool . boxed-list-element) int)""")
    }
  }
  
  test("function with non-list element rest arg") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_vector" int64 boxed-datum)""")
    }
  }
  
  test("function returning unknown type") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" () not-a-type)""")
    }
  }
  
  test("function returning non-symbol") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" () 4)""")
    }
  }
  
  test("function taking unknown type") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" (not-a-type) void)""")
    }
  }
  
  test("function taking non-symbol") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" (4) void)""")
    }
  }
  
  test("function returning truthy bool") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" () truthy-bool)""")
    }
  }
  
  test("function returning strict  bool") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" () strict-bool)""")
    }
  }
}

