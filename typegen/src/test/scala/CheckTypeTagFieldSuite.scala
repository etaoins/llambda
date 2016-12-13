package io.llambda.typegen

import org.scalatest.FunSuite

class CheckTypeTagFieldSuite extends FunSuite {
  def checkString(str: String) : Unit = {
    val defns = DefinitionParser.parseString(str)
    CheckTopLevelNamespace(defns)

    val fieldTypes = ProcessFieldTypes(defns)
    val processedTypes = ProcessCellClasses(fieldTypes)(defns)

    CheckTypeTagField(processedTypes.rootCellClass)
  }

  test("non-aliased type tag fails") {
    intercept[NonAliasedTypeTagFieldException] {
      checkString("""
        root cell Datum typetag typeId {
          int32 typeId;
        };
      """)
    }
  }

  test("type tag alias without cppname fails") {
    intercept[TypeTagAliasMissingCppNameException] {
      checkString("""
        fieldtype TypeId : int32;

        root cell Datum typetag typeId {
          TypeId typeId;
        };
      """)
    }
  }

  test("type tag alias with extern cppname fails") {
    intercept[TypeTagAliasExternallyDefinedException] {
      checkString("""
        fieldtype TypeId : int32 {
          extern cppname = CellTypeId;
        };

        root cell Datum typetag typeId {
          TypeId typeId;
        };
      """)
    }
  }

  test("type tag alias with non-integral type fails") {
    intercept[TypeTagAliasNonIntegralException] {
      checkString("""
        fieldtype TypeId : double {
          cppname = CellTypeId;
        };

        root cell Datum typetag typeId {
          TypeId typeId;
        };
      """)
    }
  }

  test("type tag alias defined integral type succeeds") {
    checkString("""
      fieldtype TypeId : int32 {
        cppname = CellTypeId;
      };

      root cell Datum typetag typeId {
        TypeId typeId;
      };
    """)
  }
}
