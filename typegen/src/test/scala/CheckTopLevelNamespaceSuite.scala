package io.llambda.typegen

import org.scalatest.FunSuite

class CheckTopLevelNamespaceSuite extends FunSuite {
  def checkString(str : String) =
    CheckTopLevelNamespace(DefinitionParser.parseString(str))

  test("duplicate cell name fails") {
    intercept[DuplicateTypeNameException] {
      checkString("""
        root cell Datum typetag typeId {
        };

        concrete cell String : Datum {
        };

        concrete cell String : Datum {
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

  test("conflicting field type and cell class name fails") {
    intercept[DuplicateTypeNameException] {
      checkString("""
        root cell myint typetag typeId{
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
