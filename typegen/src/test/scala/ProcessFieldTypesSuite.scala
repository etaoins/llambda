package io.llambda.typegen

import collection.immutable.ListMap
import org.scalatest.FunSuite

import io.llambda.llvmir

class ProcessFieldTypesSuite extends FunSuite {
  // Only pull out the field type aliases
  // The predefined types are indirectly tested through their aliaes
  def processString(str : String) : ListMap[String, FieldTypeAlias] = 
    ProcessFieldTypes(DefinitionParser.parseString(str)) collect {
      case (name, (fieldTypeAlias : FieldTypeAlias)) =>
        (name -> fieldTypeAlias)
    }

  test("field type aliasing predefined type") {
    val fieldTypes = processString("""
      fieldtype CustomType : uint32;
    """)

    val customType = fieldTypes("CustomType")

    assert(customType.aliasedType === PrimitiveFieldType(
      Some(false),
      llvmir.IntegerType(32),
      "std::uint32_t"
    ))

    assert(customType.cppTypeName === None)
    assert(customType.needsDefinition === false)
  }
  
  test("field type with C++ name") {
    val fieldTypes = processString("""
      fieldtype TypeId : uint8 {
        cppname = CellTypeId;
      };
    """)

    val typeIdType = fieldTypes("TypeId")

    assert(typeIdType.aliasedType === PrimitiveFieldType(
      Some(false),
      llvmir.IntegerType(8),
      "std::uint8_t"
    ))
    assert(typeIdType.cppTypeName === Some("CellTypeId"))
    assert(typeIdType.needsDefinition === true)
  }
  
  test("field type with extern C++ name") {
    val fieldTypes = processString("""
      fieldtype CustomType : int32 {
        extern cppname = OtherType;
      };
    """)

    val customType = fieldTypes("CustomType")

    assert(customType.aliasedType === PrimitiveFieldType(
      Some(true),
      llvmir.IntegerType(32),
      "std::int32_t"
    ))
    assert(customType.cppTypeName === Some("OtherType"))
    assert(customType.needsDefinition === false)
  }
  
  test("field type aliasing pointer type") {
    val fieldTypes = processString("""
      fieldtype DoublePtr : double*;
    """)

    val customType = fieldTypes("DoublePtr")
    
    assert(customType.aliasedType === PointerFieldType(
      PrimitiveFieldType(
        None,
        llvmir.DoubleType,
        "double"
      )
    ))
    assert(customType.cppTypeName === None)
    assert(customType.needsDefinition === false)
  }
  
  test("field type aliasing unknown type") {
    intercept[UnknownTypeException] {
      processString("""
        fieldtype CustomType : NotAType;
      """)
    }
  }
  
  test("field type aliasing another user defined field type") {
    val fieldTypes = processString("""
      fieldtype DoublePtr : double*;
      fieldtype InexactPtr : DoublePtr;
    """)

    val doubleType = fieldTypes("DoublePtr") 

    val inexactType = fieldTypes("InexactPtr")
    assert(inexactType.aliasedType === doubleType)
    assert(inexactType.cppTypeName === None)
    assert(inexactType.needsDefinition === false)
  }
  
  test("field type aliasing function pointer with forward declared cell types") {
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

    assert(customType.aliasedType === FunctionPointerFieldType(
      Some(
        PointerFieldType(
          PrimitiveFieldType(None, llvmir.UserDefinedType("datum"), "DatumCell")
        )
      ),
      List(
        PointerFieldType(
          PrimitiveFieldType(None, llvmir.UserDefinedType("procedure"), "ProcedureCell")
        ),
        PointerFieldType(
          PrimitiveFieldType(None, llvmir.UserDefinedType("listElement"), "ListElementCell")
        )
      )
    ))

    assert(customType.cppTypeName === None)
    assert(customType.needsDefinition === false)
  }
}
