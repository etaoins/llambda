package io.llambda.typegen

import org.scalatest.FunSuite

class DefinitionParserSuite extends FunSuite {
  val parseString = DefinitionParser.parseString _

  test("empty definitions fails") {
    intercept[ParseErrorException] {
      parseString("")
    }
  }

  test("cell class without concrete specifier fails") {
    intercept[ParseErrorException] {
      parseString("cell class datum;")
    }
  }

  test("simple cell class declaration") {
    val (cellDecl : ParsedCellClassDeclaration) :: Nil = parseString("""
      cell datum;
    """)

    assert(cellDecl.name === "datum")
  }
  
  test("internal cell class definition") {
    val (cellType : ParsedCellClassDefinition) :: Nil = parseString("""
      abstract internal cell datum {
      };
    """)

    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.inherits === None)
    assert(cellType.internal === true)
  }
  
  test("preconstructed cell class definition") {
    val (cellType : ParsedCellClassDefinition) :: Nil = parseString("""
      preconstructed cell datum {
      };
    """)
    
    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Preconstructed)
    assert(cellType.inherits === None)
    assert(cellType.internal === false)
  }

  test("single line comments") {
    val (cellType : ParsedCellClassDefinition) :: Nil = parseString("""
      // This is a simple cell
      abstract cell datum {
      };
    """)

    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.inherits === None)
    assert(cellType.internal === false)
  }
  
  test("multiple definitions") {
    val (datumType : ParsedCellClassDefinition) :: (numericType : ParsedCellClassDefinition) :: Nil = parseString("""
      abstract cell datum {
      };

      concrete cell numeric : datum {
      };
    """)

    assert(datumType.name === "datum")
    assert(datumType.instanceType === CellClass.Abstract)
    assert(datumType.inherits === None)
    assert(datumType.internal === false)
    
    assert(numericType.instanceType === CellClass.Concrete)
    assert(numericType.name === "numeric")
    assert(numericType.inherits === Some("datum"))
    assert(numericType.internal === false)
  }

  test("cell class with fields") {
    val (cellType : ParsedCellClassDefinition) :: Nil = parseString("""
      abstract cell datum {
        uint8 typeId;
        uint8* garbageState;
      };
    """)

    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.inherits === None)

    val typeIdField :: garbageStateField :: Nil = cellType.fields

    assert(typeIdField.name === "typeId")
    assert(typeIdField.fieldType === ParsedTypeName("uint8"))

    assert(garbageStateField.name === "garbageState")
    assert(garbageStateField.fieldType === ParsedPointerType(ParsedTypeName("uint8")))
  }
  
  test("cell class with function pointers") {
    val (cellType : ParsedCellClassDefinition) :: Nil = parseString("""
      abstract cell datum {
        void (*callback)(int64, double *);
        int64* (*delegate)();
      };
    """)

    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.inherits === None)

    val callbackField :: delegateField :: Nil = cellType.fields

    assert(callbackField.name === "callback")
    assert(callbackField.fieldType === ParsedFunctionPointerType(
      None,
      List(
        ParsedTypeName("int64"),
        ParsedPointerType(ParsedTypeName("double"))
      )
    ))

    assert(delegateField.name === "delegate")
    assert(delegateField.fieldType === ParsedFunctionPointerType(
      Some(ParsedPointerType(ParsedTypeName("int64"))),
      Nil
    ))
  }
  
  test("simple user defined field type") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype unicodeChar : int32;
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasOf === ParsedTypeName("int32"))
    assert(fieldType.cppType === None)
  }
  
  test("empty braces are allowed in a user defined field type") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype unicodeChar : int32** {
      };
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasOf === ParsedPointerType(ParsedPointerType(ParsedTypeName("int32"))))
    assert(fieldType.cppType === None)
  }
  
  test("field type without extend fails") {
    intercept[ParseErrorException] {
      parseString("""
        fieldtype unicodeChar;
      """)
    }
  }
  
  test("field type with external C type") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype unicodeChar : untypedptr {
        extern cppname = UnicodeChar;
      };
    """)
    
    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasOf === ParsedTypeName("untypedptr"))
    assert(fieldType.cppType === Some(ParsedCppType("UnicodeChar", false)))
  }
  
  test("field type with typedef C type") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype unicodeChar : int8 {
        cppname = UnicodeChar;
      };
    """)
    
    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasOf === ParsedTypeName("int8"))
    assert(fieldType.cppType === Some(ParsedCppType("UnicodeChar", true)))
  }
  
  test("field type inheriting named type failed") {
    intercept[ParseErrorException] {
      parseString("""
        fieldtype unicodeChar : int8 myint {
          cppname = UnicodeChar;
        };
      """)
    }
  }
  
  test("field type inheriting function pointer") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype procedureEntryPoint : Datum* (*)(Procedure*, ListElement*) {
        cppname = ProcedureEntryPoint;
      };
    """)

    val aliasedType = ParsedFunctionPointerType(
      Some(ParsedPointerType(ParsedTypeName("Datum"))),
      List(
        ParsedPointerType(ParsedTypeName("Procedure")),
        ParsedPointerType(ParsedTypeName("ListElement"))
      )
    )
    
    assert(fieldType.name === "procedureEntryPoint")
    assert(fieldType.aliasOf ===  aliasedType)
    assert(fieldType.cppType === Some(ParsedCppType("ProcedureEntryPoint", true)))
  }
  
  test("field type inheriting named function pointer fails") {
    intercept[ParseErrorException] {
      parseString("""
        fieldtype procedureEntryPoint : Datum* (*myFunction)(Procedure*, ListElement*) {
          cppname = ProcedureEntryPoint;
        };
      """)
    }
  }
}
