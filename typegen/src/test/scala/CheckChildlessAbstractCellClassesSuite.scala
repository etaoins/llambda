package io.llambda.typegen

import org.scalatest.FunSuite

class CheckChildlessAbstractCellClassesSuite extends FunSuite {
  def checkString(str : String) : Unit = { 
    val defns = DefinitionParser.parseString(str)
    CheckTopLevelNamespace(defns)

    val fieldTypes = ProcessFieldTypes(defns)
    val cellClasses = ProcessCellClasses(fieldTypes)(defns).cellClasses

    CheckChildlessAbstractCellClasses(cellClasses.values.toList)
  }

  test("childless root cell class fails") {
    intercept[ChildlessAbstractCellClassException] {
      checkString("""
        root cell Datum {
        };
      """);
    }
  }
  
  test("root cell with concrete child succeeds") {
    checkString("""
      root cell Datum {
      };

      concrete cell Child : Datum {
      };
    """);
  }

  test("root cell with preconstructed child succeeds") {
    checkString("""
      root cell Datum {
      };

      preconstructed cell Child : Datum {
      };
    """);
  }


  test("childless child cell class fails") {
    intercept[ChildlessAbstractCellClassException] {
      checkString("""
        root cell Datum {
        };

        abstract cell ChildCell : Datum {
        };
      """);
    }
  }
  
  test("child cell class with concrete child succeeds") {
    checkString("""
      root cell Datum {
      };

      abstract cell ChildCell : Datum {
      };

      concrete cell GrandchildCell : ChildCell {
      };
    """);
  }
}
