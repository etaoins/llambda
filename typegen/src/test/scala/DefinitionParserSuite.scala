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
      parseString("""
        cell class datum {
        };
      """)
    }
  }

  test("simple cell class declaration") {
    val (cellDecl: ParsedCellClassDeclaration) :: Nil = parseString("""
      cell datum;
    """)

    assert(cellDecl.name === "datum")
  }

  test("internal root cell class definition") {
    val (cellType: ParsedRootClassDefinition) :: Nil = parseString("""
      root internal cell Datum typetag typeId {
      };
    """)

    assert(cellType.name === "Datum")
    assert(cellType.typeTagField === "typeId")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.visibility === CellClass.Internal)
  }

  test("root cell class definition with variants fails") {
    intercept[ParseErrorException] {
      parseString("""
        root internal cell Datum typetag typeId {
          variant InlineDatum {
            uint32 field1;
          };

          variant HeapDatum {
            uint32 field2;
          };
        };
      """)
    }
  }

  test("internal tagged cell class definition") {
    val (cellType: ParsedTaggedClassDefinition) :: Nil = parseString("""
      abstract internal cell Symbol : Datum {
      };
    """)

    assert(cellType.name === "Symbol")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.parent === "Datum")
    assert(cellType.visibility === CellClass.Internal)
  }

  test("runtime-only tagged cell class definition") {
    val (cellType: ParsedTaggedClassDefinition) :: Nil = parseString("""
      abstract runtime cell Symbol : Datum {
      };
    """)

    assert(cellType.name === "Symbol")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.parent === "Datum")
    assert(cellType.visibility === CellClass.RuntimeOnly)
  }

  test("preconstructed cell class definition") {
    val (cellType: ParsedTaggedClassDefinition) :: Nil = parseString("""
      preconstructed cell EmptyList : Datum {
      };
    """)

    assert(cellType.name === "EmptyList")
    assert(cellType.instanceType === CellClass.Preconstructed)
    assert(cellType.parent === "Datum")
    assert(cellType.visibility === CellClass.Public)
  }

  test("single line comments") {
    val (cellType: ParsedRootClassDefinition) :: Nil = parseString("""
      // This is a simple cell
      root cell Datum typetag typeId {
      };
    """)

    assert(cellType.name === "Datum")
    assert(cellType.typeTagField === "typeId")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.visibility === CellClass.Public)
  }

  test("multiline comments") {
    val (cellType: ParsedRootClassDefinition) :: Nil = parseString("""
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
    assert(cellType.visibility === CellClass.Public)
  }

  test("multiple definitions") {
    val (datumType: ParsedRootClassDefinition) :: (numericType: ParsedTaggedClassDefinition) :: Nil = parseString("""
      root cell Datum typetag typeId {
      };

      concrete cell Numeric : Datum {
      };
    """)

    assert(datumType.name === "Datum")
    assert(datumType.instanceType === CellClass.Abstract)
    assert(datumType.visibility === CellClass.Public)

    assert(numericType.instanceType === CellClass.Concrete)
    assert(numericType.name === "Numeric")
    assert(numericType.parent === "Datum")
    assert(numericType.visibility === CellClass.Public)
  }

  test("root cell class with fields and initializer") {
    val (cellType: ParsedRootClassDefinition) :: Nil = parseString("""
      root cell Datum typetag typeId {
        uint8 typeId;
        uint8* garbageState = 256;
        int64* intPointers[3][5];
      };
    """)

    assert(cellType.name === "Datum")
    assert(cellType.instanceType === CellClass.Abstract)

    val List(typeIdField, garbageStateField, intPointersField) = cellType.fields

    assert(typeIdField.name === "typeId")
    assert(typeIdField.fieldType === ParsedTypeName("uint8"))
    assert(typeIdField.initializer === None)

    assert(garbageStateField.name === "garbageState")
    assert(garbageStateField.fieldType === ParsedPointerType(ParsedTypeName("uint8")))
    assert(garbageStateField.initializer === Some(256))

    assert(intPointersField.name === "intPointers")
    assert(intPointersField.fieldType === ParsedArrayType(List(3, 5), ParsedPointerType(ParsedTypeName("int64"))))
    assert(intPointersField.initializer === None)
  }

  test("tagged cell class with variants") {
    val cellTypes = parseString("""
      concrete cell Procedure : Datum {
        uint16 commonField;
      };

      variant cell InlineProcedure : Procedure {
        uint32 inlineField1;
        uint64*& inlineField2;
      };

      variant cell HeapProcedure : Procedure {
        double& heapField;
      };
    """)

    val List(
      procedureType: ParsedTaggedClassDefinition,
      inlineVariant: ParsedVariantClassDefinition,
      heapVariant: ParsedVariantClassDefinition
    ) = cellTypes

    assert(procedureType.name === "Procedure")
    assert(procedureType.instanceType === CellClass.Concrete)

    val List(commonField) = procedureType.fields

    assert(commonField.name === "commonField")
    assert(commonField.fieldType === ParsedTypeName("uint16"))
    assert(commonField.initializer === None)

    assert(inlineVariant.name === "InlineProcedure")
    assert(inlineVariant.instanceType === CellClass.Variant)

    val List(inlineField1, inlineField2) = inlineVariant.fields
    assert(inlineField1.name === "inlineField1")
    assert(inlineField1.fieldType === ParsedTypeName("uint32"))
    assert(inlineField1.initializer === None)

    assert(inlineField2.name === "inlineField2")
    assert(inlineField2.fieldType === ParsedReferenceType(ParsedPointerType(ParsedTypeName("uint64"))))
    assert(inlineField2.initializer === None)

    assert(heapVariant.name === "HeapProcedure")
    assert(heapVariant.instanceType === CellClass.Variant)

    val List(heapField) = heapVariant.fields
    assert(heapField.name === "heapField")
    assert(heapField.fieldType === ParsedReferenceType(ParsedTypeName("double")))
    assert(heapField.initializer === None)
  }

  test("tagged cell class with function pointers") {
    val (cellType: ParsedTaggedClassDefinition) :: Nil = parseString("""
      abstract cell Procedure : Datum {
        void (*callback)(int64[5], double *);
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
        ParsedArrayType(List(5), ParsedTypeName("int64")),
        ParsedPointerType(ParsedTypeName("double"))
      )
    ))

    assert(delegateField.name === "delegate")
    assert(delegateField.fieldType === ParsedFunctionPointerType(
      Some(ParsedPointerType(ParsedTypeName("int64"))),
      Nil
    ))
  }

  test("tagged cell class with array brackets before identifier fails") {
    intercept[ParseErrorException] {
      parseString("""
        abstract cell Procedure : Datum {
          int64*[5] badArray;
        };
      """)
    }
  }

  test("simple user defined field type") {
    val (fieldType: ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype unicodeChar : int32;
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasedType === ParsedTypeName("int32"))
    assert(fieldType.cppType === None)
  }

  test("empty braces are allowed in a user defined field type") {
    val (fieldType: ParsedFieldTypeAlias) :: Nil = parseString("""
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
    val (fieldType: ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype unicodeChar : untypedptr {
        extern cppname = UnicodeChar;
      };
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasedType === ParsedTypeName("untypedptr"))
    assert(fieldType.cppType === Some(ParsedCppType("UnicodeChar", false)))
  }

  test("field type with typedef C type") {
    val (fieldType: ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype unicodeChar : int8 {
        cppname = UnicodeChar;
      };
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.aliasedType === ParsedTypeName("int8"))
    assert(fieldType.cppType === Some(ParsedCppType("UnicodeChar", true)))
  }

  test("field type inheriting named type fails") {
    intercept[ParseErrorException] {
      parseString("""
        fieldtype unicodeChar : int8 myint {
          cppname = UnicodeChar;
        };
      """)
    }
  }

  test("field type inheriting array of pointers") {
    val (fieldType: ParsedFieldTypeAlias) :: Nil = parseString("""
      fieldtype vectorPair : Vector*[2] {
        cppname = VectorPair;
      };
    """)

    val aliasedType = ParsedArrayType(List(2), ParsedPointerType(ParsedTypeName("Vector")))

    assert(fieldType.name === "vectorPair")
    assert(fieldType.aliasedType ===  aliasedType)
    assert(fieldType.cppType === Some(ParsedCppType("VectorPair", true)))
  }

  test("field type inheriting type with array brackets before pointer fails") {
    intercept[ParseErrorException] {
      parseString("""
        fieldtype vectorPair : Vector[2]* {
          cppname = VectorPair;
        };
      """)
    }
  }

  test("field type inheriting function pointer") {
    val (fieldType: ParsedFieldTypeAlias) :: Nil = parseString("""
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
