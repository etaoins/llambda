package io.llambda.llvmir

import org.scalatest.FunSuite

class IrValueSuite extends FunSuite {
  test("local variable") {
    val testLocal = LocalVariable("test", FloatType)

    assert(testLocal.toIr === "%test")
    assert(testLocal.irType === FloatType)
  }

  test("local variable is escaped") {
    val testLocal = LocalVariable("Test Local", FloatType)

    assert(testLocal.toIr === "%\"Test Local\"")
    assert(testLocal.irType === FloatType)
  }

  test("global variable") {
    val testLocal = GlobalVariable("test", PointerType(DoubleType))

    assert(testLocal.toIr === "@test")
    assert(testLocal.irType === PointerType(DoubleType))
  }

  test("global variable is escaped") {
    val testLocal = GlobalVariable("Test Global", PointerType(DoubleType))

    assert(testLocal.toIr === "@\"Test Global\"")
    assert(testLocal.irType === PointerType(DoubleType))
  }

  test("boolean constant") {
    assert(TrueConstant.toIr === "true")
    assert(TrueConstant.irType === IntegerType(1))

    assert(FalseConstant.toIr === "false")
    assert(FalseConstant.irType === IntegerType(1))
  }

  test("integer constant") {
    assert(IntegerConstant(IntegerType(16), 45).toIr === "45")
  }

  test("single constant") {
    assert(FloatConstant(0.0f).irType === FloatType)
    assert(FloatConstant(20.0f).toIr === "20.0")
    assert(FloatConstant(-300.2f).toIr === "-300.2")
    assert(FloatConstant(Float.PositiveInfinity).toIr === "0x7f800000")
    assert(FloatConstant(Float.NegativeInfinity).toIr === "0xff800000")
    assert(FloatConstant(Float.NaN).toIr === "0x7fc00000")
  }

  test("double constant") {
    assert(DoubleConstant(0.0).irType === DoubleType)
    assert(DoubleConstant(20.0).toIr === "20.0")
    assert(DoubleConstant(-40000.14).toIr === "-40000.14")
    assert(DoubleConstant(Double.PositiveInfinity).toIr === "0x7ff0000000000000")
    assert(DoubleConstant(Double.NegativeInfinity).toIr === "0xfff0000000000000")
    assert(DoubleConstant(Double.NaN).toIr === "0x7ff8000000000000")
  }

  test("null pointer constant") {
    assert(NullPointerConstant(PointerType(IntegerType(8))).toIr === "null")
  }

  test("anonymous structure constant") {
    val testConstant = StructureConstant(List(FalseConstant, IntegerConstant(IntegerType(32), 5), NullPointerConstant(PointerType(IntegerType(32)))))

    assert(testConstant.irType === StructureType(List(IntegerType(1), IntegerType(32), PointerType(IntegerType(32)))))
    assert(testConstant.toIr === "{i1 false, i32 5, i32* null}")
  }

  test("user defined structure constant") {
    val innerConstant = StructureConstant(List(FalseConstant), userDefinedType=Some(UserDefinedType("boolStruct")))
    val testConstant = StructureConstant(List(innerConstant), userDefinedType=Some(UserDefinedType("outerStruct")))

    assert(testConstant.irType === UserDefinedType("outerStruct"))
    assert(testConstant.toIr === "{%boolStruct {i1 false}}")
  }

  test("array constant") {
    val testConstant = ArrayConstant(FloatType, List(FloatConstant(1), FloatConstant(1), FloatConstant(2), FloatConstant(3), FloatConstant(5)))

    assert(testConstant.irType === ArrayType(5, FloatType))
    assert(testConstant.toIr === "[float 1.0, float 1.0, float 2.0, float 3.0, float 5.0]")
  }

  test("ASCII string constant") {
    val testConstant = StringConstant.fromUtf8String("Hello, world!")

    assert(testConstant.irType === ArrayType(14, IntegerType(8)))
    assert(testConstant.toIr === "c\"Hello, world!\\00\"")
  }

  test("ASCII string with quote") {
    val testConstant = StringConstant.fromUtf8String("Hello\"world")
    assert(testConstant.toIr === "c\"Hello\\22world\\00\"")
  }

  test("ASCII string with newline") {
    val testConstant = StringConstant.fromUtf8String("Hello\nworld")
    assert(testConstant.toIr === "c\"Hello\\0Aworld\\00\"")
  }

  test("UTF-8 string") {
    val testConstant = StringConstant.fromUtf8String("๛")
    assert(testConstant.toIr === "c\"\\E0\\B9\\9B\\00\"")
  }

  test("element pointer constant") {
    val globalVarDef = IrGlobalVariableDef(
      name="helloString",
      initializer=StringConstant.fromUtf8String("Hello"),
      constant=true
    )

    val elementPtrConstant = ElementPointerConstant(
      elementType=IntegerType(8),
      basePointer=globalVarDef.variable,
      indices=List(1, 0),
      inbounds=false
    )

    assert(elementPtrConstant.irType === PointerType(IntegerType(8)))
    assert(elementPtrConstant.toIr === "getelementptr ([6 x i8], [6 x i8]* @helloString, i32 1, i32 0)")

    val inboundElementPtrConstant = ElementPointerConstant(
      elementType=IntegerType(8),
      basePointer=globalVarDef.variable,
      indices=List(0, 2),
      inbounds=true
    )

    assert(inboundElementPtrConstant.irType === PointerType(IntegerType(8)))
    assert(inboundElementPtrConstant.toIr === "getelementptr inbounds ([6 x i8], [6 x i8]* @helloString, i32 0, i32 2)")

    intercept[InconsistentIrException] {
      val boolConstant = TrueConstant

      ElementPointerConstant(
        elementType=IntegerType(1),
        basePointer=boolConstant,
        indices=List(0, 2),
        inbounds=true
      )
    }
  }

  test("bitcast constant") {
    val sourceValue = IntegerConstant(IntegerType(32), 50)

    val bitcastConstant = BitcastToConstant(sourceValue, FloatType)

    assert(bitcastConstant.irType === FloatType)
    assert(bitcastConstant.toIr === "bitcast (i32 50 to float)")
  }

  test("ptrtoint constant") {
    val sourceValue = NullPointerConstant(PointerType(DoubleType))

    val intConstant = PtrToIntConstant(sourceValue, IntegerType(32))

    assert(intConstant.irType === IntegerType(32))
    assert(intConstant.toIr === "ptrtoint (double* null to i32)")

  }

  test("ptrtoint with non-pointer fails") {
    intercept[InconsistentIrException] {
      PtrToIntConstant(IntegerConstant(IntegerType(64), 5), IntegerType(64))
    }
  }

  test("inttoptr constant") {
    val sourceValue = IntegerConstant(IntegerType(64), 67)

    val ptrConstant = IntToPtrConstant(sourceValue, PointerType(FloatType))

    assert(ptrConstant.irType === PointerType(FloatType))
    assert(ptrConstant.toIr === "inttoptr (i64 67 to float*)")
  }

  test("inttoptr with non-integer fails") {
    intercept[InconsistentIrException] {
      IntToPtrConstant(DoubleConstant(50.0), PointerType(FloatType))
    }
  }

  test("metadata string") {
    val testMetadata = MetadataString.fromUtf8String("Hello\nworld")
    assert(testMetadata.toIr === "!\"Hello\\0Aworld\"")
  }

  test("user defined metadata node") {
    val testMetadata = UserDefinedMetadataNode(List(
      Some(MetadataString.fromUtf8String("teststr")),
      Some(IntegerConstant(IntegerType(32), 5)),
      None,
      Some(UserDefinedMetadataNode(Nil))
    ))

    assert(testMetadata.toIr === """!{!"teststr", i32 5, null, !{}}""")
  }

  test("numbered metadata") {
    val testMetadata = NumberedMetadata(55)

    assert(testMetadata.toIr === "!55")
  }
}
