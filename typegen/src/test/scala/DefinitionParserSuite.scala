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
  
  test("internal root cell class definition") {
    val (cellType : ParsedRootClassDefinition) :: Nil = parseString("""
      root internal cell Datum typetag typeId {
      };
    """)

    assert(cellType.name === "Datum")
    assert(cellType.typeTagField === "typeId")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.internal === true)
  }
  
  test("internal child cell class definition") {
    val (cellType : ParsedChildClassDefinition) :: Nil = parseString("""
      abstract internal cell Symbol : Datum {
      };
    """)

    assert(cellType.name === "Symbol")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.parent === "Datum")
    assert(cellType.internal === true)
  }
  
  test("preconstructed cell class definition") {
    val (cellType : ParsedChildClassDefinition) :: Nil = parseString("""
      preconstructed cell EmptyList : Datum {
      };
    """)
    
    assert(cellType.name === "EmptyList")
    assert(cellType.instanceType === CellClass.Preconstructed)
    assert(cellType.parent === "Datum")
    assert(cellType.internal === false)
  }

  test("single line comments") {
    val (cellType : ParsedRootClassDefinition) :: Nil = parseString("""
      // This is a simple cell
      root cell Datum typetag typeId {
      };
    """)

    assert(cellType.name === "Datum")
    assert(cellType.typeTagField === "typeId")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.internal === false)
  }
  
  test("multiline comments") {
    val (cellType : ParsedRootClassDefinition) :: Nil = parseString("""
      /* This is 
       * a multiline comment
       */
      root cell Datum typetag typeId{
        /* This could be multiple lines but it isn't */
      };
    """)

    assert(cellType.name === "Datum")
    assert(cellType.typeTagField === "typeId")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.internal === false)
  }
  
  test("multiple definitions") {
    val (datumType : ParsedRootClassDefinition) :: (numericType : ParsedChildClassDefinition) :: Nil = parseString("""
      root cell Datum typetag typeId {
      };

      concrete cell Numeric : Datum {
      };
    """)

    assert(datumType.name === "Datum")
    assert(datumType.instanceType === CellClass.Abstract)
    assert(datumType.internal === false)
    
    assert(numericType.instanceType === CellClass.Concrete)
    assert(numericType.name === "Numeric")
    assert(numericType.parent === "Datum")
    assert(numericType.internal === false)
  }

  test("root cell class with fields") {
    val (cellType : ParsedRootClassDefinition) :: Nil = parseString("""
      root cell Datum typetag typeId {
        uint8 typeId;
        uint8* garbageState;
      };
    """)

    assert(cellType.name === "Datum")
    assert(cellType.instanceType === CellClass.Abstract)

    val typeIdField :: garbageStateField :: Nil = cellType.fields

    assert(typeIdField.name === "typeId")
    assert(typeIdField.fieldType === ParsedTypeName("uint8"))

    assert(garbageStateField.name === "garbageState")
    assert(garbageStateField.fieldType === ParsedPointerType(ParsedTypeName("uint8")))
  }
  
  test("child cell class with function pointers") {
    val (cellType : ParsedChildClassDefinition) :: Nil = parseString("""
      abstract cell Procedure : Datum {
        void (*callback)(int64, double *);
        int64* (*delegate)();
      };
    """)

    assert(cellType.name === "Procedure")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.parent === "Datum")

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
    val (fieldType : ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype unicodeChar : int32;
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasedType === ParsedTypeName("int32"))
    assert(fieldType.cppType === None)
  }
  
  test("empty braces are allowed in a user defined field type") {
    val (fieldType : ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype unicodeChar : int32** {
      };
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasedType === ParsedPointerType(ParsedPointerType(ParsedTypeName("int32"))))
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
    val (fieldType : ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype unicodeChar : untypedptr {
        extern cppname = UnicodeChar;
      };
    """)
    
    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasedType === ParsedTypeName("untypedptr"))
    assert(fieldType.cppType === Some(ParsedCppType("UnicodeChar", false)))
  }
  
  test("field type with typedef C type") {
    val (fieldType : ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype unicodeChar : int8 {
        cppname = UnicodeChar;
      };
    """)
    
    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasedType === ParsedTypeName("int8"))
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
    val (fieldType : ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype procedureEntryPoint : Datum* (*)(Procedure*, ListElement*) {
        cppname = Types::Callbacks::procedure_entry_point;
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
    assert(fieldType.aliasedType ===  aliasedType)
    assert(fieldType.cppType === Some(ParsedCppType("Types::Callbacks::procedure_entry_point", true)))
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
