package io.llambda.typegen

import collection.immutable.ListMap
import org.scalatest.FunSuite

import io.llambda.llvmir

class ProcessFieldTypesSuite extends FunSuite {
  def processString(str : String) = 
    ProcessFieldTypes(DefinitionParser.parseString(str))

  test("field type inherting predefined type") {
    val fieldTypes = processString("""
      fieldtype CustomType : uint32;
    """)

    val customType = fieldTypes("CustomType")
    assert(customType.signed === Some(false))
    assert(customType.llvmType === llvmir.IntegerType(32))
    assert(customType.cppTypeName === "CustomType")
    assert(customType.needsDefinition === true)
  }
  
  test("field type with explicit C name") {
    val fieldTypes = processString("""
      fieldtype TypeId : uint8 {
        cppname = CellTypeId;
      };
    """)

    val typeIdType = fieldTypes("TypeId")
    assert(typeIdType.signed === Some(false))
    assert(typeIdType.llvmType === llvmir.IntegerType(8))
    assert(typeIdType.cppTypeName === "CellTypeId")
    assert(typeIdType.needsDefinition === true)
  }
  
  test("field type with extern C name") {
    val fieldTypes = processString("""
      fieldtype CustomType : int32 {
        extern cppname = OtherType;
      };
    """)

    val customType = fieldTypes("CustomType")
    assert(customType.signed === Some(true))
    assert(customType.llvmType === llvmir.IntegerType(32))
    assert(customType.cppTypeName === "OtherType")
    assert(customType.needsDefinition === false)
  }
  
  test("field type inheriting pointer type") {
    val fieldTypes = processString("""
      fieldtype DoublePtr : double*;
    """)

    val customType = fieldTypes("DoublePtr")
    assert(customType.signed === None)
    assert(customType.llvmType === llvmir.PointerType(llvmir.DoubleType))
    assert(customType.cppTypeName === "DoublePtr")
    assert(customType.needsDefinition === true)
  }
  
  test("field type inherting unknown type") {
    intercept[UnknownTypeException] {
      processString("""
        fieldtype CustomType : NotAType;
      """)
    }
  }
  
  test("field type inheriting another user defined field type") {
    val fieldTypes = processString("""
      fieldtype DoublePtr : double*;
      fieldtype InexactPtr : DoublePtr;
    """)

    val customType = fieldTypes("InexactPtr")
    assert(customType.signed === None)
    assert(customType.llvmType === llvmir.PointerType(llvmir.DoubleType))
    assert(customType.cppTypeName === "InexactPtr")
    assert(customType.needsDefinition === true)
  }
  
  test("field type inheriting function pointer with forward declared cell types") {
    val fieldTypes = processString("""
      cell Datum;
      cell Procedure;
      cell ListElement;
      fieldtype ProcedureEntryPoint : Datum* (*)(Procedure*, ListElement*);
    """)

    val customType = fieldTypes("ProcedureEntryPoint")

    val llvmType = llvmir.PointerType(
      llvmir.FunctionType(
        llvmir.PointerType(llvmir.UserDefinedType("datum")),
        List(
          llvmir.PointerType(llvmir.UserDefinedType("procedure")),
          llvmir.PointerType(llvmir.UserDefinedType("listElement"))
        )
      )
    )

    assert(customType.signed === None)
    assert(customType.llvmType === llvmType)
    assert(customType.cppTypeName === "ProcedureEntryPoint")
    assert(customType.needsDefinition === true)
  }
}
