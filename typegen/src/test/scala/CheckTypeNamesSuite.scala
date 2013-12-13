package io.llambda.typegen

import org.scalatest.FunSuite

class CheckTypeNamesSuite extends FunSuite {
  def checkString(str : String) = 
    CheckTypeNames(DefinitionParser.parseString(str))

  test("duplicate cell name fails") {
    intercept[DuplicateTypeNameException] {
      checkString("""
        abstract cell datum {
        };

        concrete cell datum {
        };
      """)
    }
  }
  
  test("duplicate field type fails") {
    intercept[DuplicateTypeNameException] {
      checkString("""
        fieldtype myint : int32;
        fieldtype myint : uint64;
      """)
    }
  }
  
  test("redefining builtin type fails") {
    intercept[DuplicateTypeNameException] {
      checkString("""
        fieldtype bool : int32;
      """)
    }
  }
  
  test("conflicting field type and cell class name") {
    intercept[DuplicateTypeNameException] {
      checkString("""
        abstract cell myint {
        };

        fieldtype myint : uint64;
      """)
    }
  }
  
  test("undefined cell class fails") {
    intercept[UndefinedCellClassException] {
      checkString("""
        cell datum;
      """)
    }
  }
}
