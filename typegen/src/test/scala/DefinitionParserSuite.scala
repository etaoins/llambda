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

  test("simple empty root cell class definition") {
    val (cellType : ParsedCellClass) :: Nil = parseString("""
      abstract cell datum;
    """)

    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.inherits === None)
    assert(cellType.internal === false)
  }
  
  test("internal cell class definition") {
    val (cellType : ParsedCellClass) :: Nil = parseString("""
      abstract internal cell datum;
    """)

    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.inherits === None)
    assert(cellType.internal === true)
  }
  
  test("preconstructed cell class definition") {
    val (cellType : ParsedCellClass) :: Nil = parseString("""
      preconstructed cell datum;
    """)
    
    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Preconstructed)
    assert(cellType.inherits === None)
    assert(cellType.internal === false)
  }
  
  test("empty braces are allowed in cell class") {
    val (cellType : ParsedCellClass) :: Nil = parseString("""
      concrete cell datum {
      };
    """)
    
    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Concrete)
    assert(cellType.inherits === None)
    assert(cellType.internal === false)
  }

  test("single line comments") {
    val (cellType : ParsedCellClass) :: Nil = parseString("""
      // This is a simple cell
      abstract cell datum;
    """)

    assert(cellType.name === "datum")
    assert(cellType.instanceType === CellClass.Abstract)
    assert(cellType.inherits === None)
    assert(cellType.internal === false)
  }
  
  test("multiple definitions") {
    val (datumType : ParsedCellClass) :: (numericType : ParsedCellClass) :: Nil = parseString("""
      abstract cell datum;
      concrete cell numeric : datum;
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
    val (cellType : ParsedCellClass) :: Nil = parseString("""
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
    assert(typeIdField.fieldType === "uint8")
    
    assert(garbageStateField.name === "garbageState")
    assert(garbageStateField.fieldType === "uint8*")
  }
  
  test("simple user defined field type") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype unicodeChar : int32;
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.inherits === "int32")
    assert(fieldType.ctype === None)
  }
  
  test("empty braces are allowed in a user defined field type") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype unicodeChar : int32** {
      };
    """)

    assert(fieldType.name === "unicodeChar")
    assert(fieldType.inherits === "int32**")
    assert(fieldType.ctype === None)
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
      fieldtype unicodeChar : void* {
        extern ctype = UnicodeChar;
      };
    """)
    
    assert(fieldType.name === "unicodeChar")
    assert(fieldType.inherits === "void*")
    assert(fieldType.ctype === Some(ParsedCType("UnicodeChar", true)))
  }
  
  test("field type with typedef C type") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype unicodeChar : int8 {
        ctype = UnicodeChar;
      };
    """)
    
    assert(fieldType.name === "unicodeChar")
    assert(fieldType.inherits === "int8")
    assert(fieldType.ctype === Some(ParsedCType("UnicodeChar", false)))
  }
  
  test("field type inheriting function pointer") {
    val (fieldType : ParsedUserDefinedFieldType) :: Nil = parseString("""
      fieldtype procedureEntryPoint : datum* (*)(procedure*, listElement*) {
        ctype = ProcedureEntryPoint;
      };
    """)
    
    assert(fieldType.name === "procedureEntryPoint")
    assert(fieldType.inherits === "datum* (*)(procedure*, listElement*)")
    assert(fieldType.ctype === Some(ParsedCType("ProcedureEntryPoint", false)))
  }
}
