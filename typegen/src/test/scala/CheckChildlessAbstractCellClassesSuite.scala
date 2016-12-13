package io.llambda.typegen

import org.scalatest.FunSuite

class CheckChildlessAbstractCellClassesSuite extends FunSuite {
  def checkString(str: String) : Unit = {
    val defns = DefinitionParser.parseString(str)
    CheckTopLevelNamespace(defns)

    val fieldTypes = ProcessFieldTypes(defns)
    val processedTypes = ProcessCellClasses(fieldTypes)(defns)

    CheckChildlessAbstractCellClasses(processedTypes)
  }

  test("childless root cell class fails") {
    intercept[ChildlessAbstractCellClassException] {
      checkString("""
        root cell Datum typetag typeId {
          int32 typeId;
        };
      """);
    }
  }

  test("root cell with concrete child succeeds") {
    checkString("""
      root cell Datum typetag typeId {
        int32 typeId;
      };

      concrete cell Child : Datum {
      };
    """);
  }

  test("root cell with preconstructed child succeeds") {
    checkString("""
      root cell Datum typetag typeId {
        int32 typeId;
      };

      preconstructed cell Child : Datum {
      };
    """);
  }


  test("childless child cell class fails") {
    intercept[ChildlessAbstractCellClassException] {
      checkString("""
        root cell Datum typetag typeId {
          int32 typeId;
        };

        abstract cell ChildCell : Datum {
        };
      """);
    }
  }

  test("child cell class with concrete child succeeds") {
    checkString("""
      root cell Datum typetag typeId {
        int32 typeId;
      };

      abstract cell ChildCell : Datum {
      };

      concrete cell GrandchildCell : ChildCell {
      };
    """);
  }
}
