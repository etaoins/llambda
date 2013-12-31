package io.llambda.typegen

import collection.immutable.ListMap
import org.scalatest.{FunSuite, Inside}

import io.llambda.llvmir

class ProcessCellClassesSuite extends FunSuite with Inside {
  def processString(str : String) = { 
    val defns = DefinitionParser.parseString(str)
    CheckTopLevelNamespace(defns)

    val fieldTypes = ProcessFieldTypes(defns)
    ProcessCellClasses(fieldTypes)(defns)
  }
  
  test("multiple root cell classes fails") {
    intercept[DuplicateRootCellClassException] {
      processString("""
        root cell FirstRoot typetag typeId {
          int32 typeId;
        };

        root cell SecondRoot typetag typeId {
          int32 typeId;
        };
      """)
    }
  }
  
  test("no root cell classes fails") {
    intercept[NoRootCellClassException] {
      processString("""
        fieldtype myField : int32;
      """)
    }
  }

  test("inheriting undefined cell class fails") {
    intercept[UndefinedCellClassException] {
      // We use Datum after it's declared but after it's defined
      processString("""
        cell Datum;

        abstract cell Numeric : Datum {
        };

        root cell Datum typetag typeId {
          int32 typeId;
        };
      """)
    }
  }
 
  test("root class with undefined tag field fails") {
    intercept[UndefinedTypeTagFieldException] {
      processString("""
        root cell FirstRoot typetag typeId {
        };
      """)
    }
  }
  
  test("duplicate field names fails") {
    intercept[DuplicateFieldNameException] {
      processString("""
        root cell FirstRoot typetag typeId {
          int32 typeId;
          int64 typeId;
        };
      """)
    }
  }
  
  test("initializing non-integer field fails") {
    intercept[InitializingNonIntegralFieldException] {
      processString("""
        root cell FirstRoot typetag typeId {
          int32 typeId;
          untypedptr somePtr = 5;
        };
      """)
    }
  }

  test("minimal root class") {
    val processedTypes = processString("""
      root cell Datum typetag typeId {
        int32 typeId;
      };
    """)

    val classes = processedTypes.cellClasses
    assert(processedTypes.nextTbaaIndex === 1)

    inside(classes("Datum")) { case (datumClass : RootCellClass) =>
      assert(datumClass.name === "Datum")
      assert(datumClass.fields.size === 1)
      assert(datumClass.internal === false)
      assert(datumClass.typeId === None)
      assert(datumClass.fieldTbaaNodes.size === 1)

      assert(processedTypes.rootCellClass === datumClass)
    }
  }
  
  test("minimal internal root class") {
    val processedTypes = processString("""
      root internal cell Datum typetag typeId {
        int32 typeId;
      };
    """)
    
    val classes = processedTypes.cellClasses

    inside(classes("Datum")) { case (datumClass : RootCellClass) =>
      assert(datumClass.internal === true)
    }
  }
  
  test("root class with additional fields") {
    val processedTypes = processString("""
      root cell Datum typetag typeId {
        int32 typeId;
        int8 gcState = 16;
      };
    """)
    
    val classes = processedTypes.cellClasses
    assert(processedTypes.nextTbaaIndex === 2)

    inside(classes("Datum")) { case (datumClass : RootCellClass) =>
      assert(datumClass.name === "Datum")
      assert(datumClass.instanceType === CellClass.Abstract)
      assert(datumClass.internal === false)
      assert(datumClass.typeId === None)

      val List(typeIdField, gcStateField) = datumClass.fields

      assert(typeIdField.fieldType === PrimitiveFieldType(
        Some(true),
        llvmir.IntegerType(32),
        "std::int32_t"
      ))
      assert(typeIdField.initializer === None)

      assert(gcStateField.fieldType === PrimitiveFieldType(
        Some(true),
        llvmir.IntegerType(8),
        "std::int8_t"
      ))
      assert(gcStateField.initializer === Some(16))

      val typeIdTbaaNode = datumClass.fieldTbaaNodes(typeIdField)
      assert(typeIdTbaaNode.index === 0)
      assert(typeIdTbaaNode.parentIndex === None)

      val gcStateTbaaNode = datumClass.fieldTbaaNodes(gcStateField)
      assert(gcStateTbaaNode.index === 1)
      assert(gcStateTbaaNode.parentIndex === None)
    }
  }
  
  test("abstract child class with fields of root class with fields") {
    val processedTypes = processString("""
      root cell Datum typetag typeId {
        int32 typeId;
        int8 gcState;
      };

      abstract cell StringLike : Datum {
        uint32 charCount;
        uint32 byteCount;
        uint8 *data;
      };
    """)
    
    val classes = processedTypes.cellClasses
    assert(processedTypes.nextTbaaIndex === 7)

    val datumClass = classes("Datum")

    inside(classes("StringLike")) { case (stringLikeClass : ChildCellClass) =>
      assert(stringLikeClass.name === "StringLike")
      assert(stringLikeClass.internal === false)
      assert(stringLikeClass.typeId === None)
      assert(stringLikeClass.instanceType === CellClass.Abstract)
      assert(stringLikeClass.parent === datumClass) 

      val List(charCountField, byteCountField, dataField) = stringLikeClass.fields

      assert(charCountField.fieldType === PrimitiveFieldType(
        Some(false),
        llvmir.IntegerType(32),
        "std::uint32_t"
      ))
      
      assert(byteCountField.fieldType === PrimitiveFieldType(
        Some(false),
        llvmir.IntegerType(32),
        "std::uint32_t"
      ))

      assert(dataField.fieldType === PointerFieldType(
        PrimitiveFieldType(
          Some(false),
          llvmir.IntegerType(8),
          "std::uint8_t"
        )
      ))

      // Get our inherited fields
      val List(typeIdField, gcStateField) = datumClass.fields

      // Check our parents TBAA nodes
      val parentTypeIdTbaaNode = datumClass.fieldTbaaNodes(typeIdField)
      assert(parentTypeIdTbaaNode.index === 0)
      assert(parentTypeIdTbaaNode.parentIndex === None)
      
      val parentGcStateTbaaNode = datumClass.fieldTbaaNodes(gcStateField)
      assert(parentGcStateTbaaNode.index === 1)
      assert(parentGcStateTbaaNode.parentIndex === None)

      // Now our inherited nodes
      val childTypeIdTbaaNode = stringLikeClass.fieldTbaaNodes(typeIdField)
      assert(childTypeIdTbaaNode.index === 2)
      assert(childTypeIdTbaaNode.parentIndex === Some(parentTypeIdTbaaNode.index))
      
      val childGcStateTbaaNode = stringLikeClass.fieldTbaaNodes(gcStateField)
      assert(childGcStateTbaaNode.index === 3)
      assert(childGcStateTbaaNode.parentIndex === Some(parentGcStateTbaaNode.index))

      // And finally a new nodes
      val charCountNode = stringLikeClass.fieldTbaaNodes(charCountField)
      assert(charCountNode.index === 4)
      assert(charCountNode.parentIndex === None)
    }
  }
  
  test("concrete and preconstructed child classes") {
    val processedTypes = processString("""
      root cell Datum typetag typeId {
        int32 typeId;
      };

      preconstructed cell Boolean : Datum {
      };
      
      concrete cell Character : Datum {
      };
    """)

    val classes = processedTypes.cellClasses
    assert(processedTypes.nextTbaaIndex === 3)

    val datumClass = classes("Datum")

    inside(classes("Boolean")) { case (booleanClass : ChildCellClass) =>
      assert(booleanClass.name === "Boolean")
      assert(booleanClass.internal === false)
      assert(booleanClass.typeId === Some(0))
      assert(booleanClass.instanceType === CellClass.Preconstructed)
      assert(booleanClass.parent === datumClass) 
    }

    inside(classes("Character")) { case (characterClass : ChildCellClass) =>
      assert(characterClass.name === "Character")
      assert(characterClass.internal === false)
      assert(characterClass.typeId === Some(1))
      assert(characterClass.instanceType === CellClass.Concrete)
      assert(characterClass.parent === datumClass) 
    }
  }
  
  test("inheriting from non-abstract cell class fails") {
    intercept[InheritingNonAbstractCellClassException] {
      processString("""
        root cell Datum typetag typeId {
          int32 typeId;
        };

        preconstructed cell Boolean : Datum {
        };
        
        concrete cell Character : Boolean {
        };
      """)
    }
  }
}
