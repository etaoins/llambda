package llambda.frontend

import org.scalatest.FunSuite
import llambda._

class NativeFunctionDeclSuite extends FunSuite with testutil.ExpressionHelpers {
  implicit val nfiScope = new Scope(collection.mutable.Map(NativeFunctionPrimitives.bindings.toSeq : _*))
  
  test("void native function") {
    expectResult(et.NativeFunction(Nil, false, None, "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () void)""")
    }
  }
  
  test("function returning int8") {
    expectResult(et.NativeFunction(Nil, false, Some(nfi.Int8), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" () int8)""")
    }
  }
  
  test("function taking int16 and returning int32") {
    expectResult(et.NativeFunction(nfi.Int16 :: Nil, false, Some(nfi.Int32), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (int16) int32)""")
    }
  }
  
  test("function taking int64, float and returning double") {
    expectResult(et.NativeFunction(nfi.Int64 :: nfi.Float :: Nil, false, Some(nfi.Double), "lliby_newline")) {
      expressionFor("""(native-function "lliby_newline" (int64 float) double)""")
    }
  }

  test("function with only rest arg") {
    expectResult(et.NativeFunction(Nil, true, Some(nfi.BoxedDatum), "lliby_vector")) {
      expressionFor("""(native-function "lliby_vector" boxeddatum boxeddatum)""")
    }
  }
  
  test("function with fixed and rest args") {
    expectResult(et.NativeFunction(nfi.Bool8 :: Nil, true, Some(nfi.Int32), "lliby_misc")) {
      expressionFor("""(native-function "lliby_misc" (bool8 . boxeddatum) int)""")
    }
  }
  
  
  test("function with non-boxed rest arg") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_vector" int64 boxeddatum)""")
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
}

