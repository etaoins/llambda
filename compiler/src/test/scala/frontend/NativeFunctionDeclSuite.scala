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
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=Nil,
        hasRestArg=false,
        returnType=None
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" ())""")
    }
  }
  
  test("function taking world pointer") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=true,
        hasSelfArg=false,
        fixedArgs=Nil,
        hasRestArg=false,
        returnType=None
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" (world-pointer))""")
    }
  }
  
  test("function returning int8") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=Nil,
        hasRestArg=false,
        returnType=Some(vt.Int8)
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" () <int8>)""")
    }
  }
  
  test("function taking int16 and returning int32") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=List(vt.Int16),
        hasRestArg=false,
        returnType=Some(vt.Int32)
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" (<int16>) <int32>)""")
    }
  }
  
  test("function taking world pointer, int64, float and returning double") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=true,
        hasSelfArg=false,
        fixedArgs=List(vt.Int64, vt.Float),
        hasRestArg=false,
        returnType=Some(vt.Double)
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" (world-pointer <int64> <float>) <double>)""")
    }
  }
  
  test("function taking uint16 and returning uint32") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=List(vt.UInt16),
        hasRestArg=false,
        returnType=Some(vt.UInt32)
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" (<uint16>) <uint32>)""")
    }
  }
  
  test("function taking bool and returning bool") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=List(vt.CBool),
        hasRestArg=false,
        returnType=Some(vt.CBool)
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" (<bool>) <bool>)""")
    }
  }
  
  test("function taking int8 and returning unicode char") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=List(vt.Int8),
        hasRestArg=false,
        returnType=Some(vt.UnicodeChar)
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" (<int8>) <unicode-char>)""")
    }
  }
  
  test("function taking a integer cell and returning a rational cell") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=List(vt.IntrinsicCellType(ct.ExactIntegerCell)),
        hasRestArg=false,
        returnType=Some(vt.IntrinsicCellType(ct.InexactRationalCell))
      ),
      "lliby_newline"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_newline" (<exact-integer-cell>) <inexact-rational-cell>)""")
    }
  }

  test("function with only rest arg") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=Nil,
        hasRestArg=true,
        returnType=Some(vt.IntrinsicCellType(ct.DatumCell))
      ),
      "lliby_vector"
    )

    assertResult(expectedFunction) {
      expressionFor("""(native-function "lliby_vector" <list-element-cell> <datum-cell>)""")
    }
  }
  
  test("function with fixed and rest args") {
    val expectedFunction = et.NativeFunction(
      ProcedureSignature(
        hasWorldArg=false,
        hasSelfArg=false,
        fixedArgs=List(vt.CBool),
        hasRestArg=true,
        returnType=Some(vt.Int32)
      ),
      "lliby_misc"
    )

    assertResult(expectedFunction) {
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
  
  test("function returning world pointer fails") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" () world-pointer)""")
    }
  }

  test("function taking world pointer in non-initial position fails") {
    intercept[BadSpecialFormException] {
      expressionFor("""(native-function "lliby_newline" (<bool> world-pointer))""")
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

