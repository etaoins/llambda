package io.llambda.typegen

import org.scalatest.{FunSuite, Inside}

import io.llambda.llvmir


class ProcessCellClassesSuite extends FunSuite with Inside {
  def processString(str: String) = {
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

  test("duplicate field names from parent fails") {
    intercept[DuplicateFieldNameException] {
      processString("""
        root cell FirstRoot typetag typeId {
          int32 typeId;
        };

        concrete cell Procedure : FirstRoot {
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
    assert(processedTypes.nextMetadataIndex === 11)

    inside(classes("Datum")) { case (datumClass: RootCellClass) =>
      assert(datumClass.name === "Datum")
      assert(datumClass.fields.size === 1)
      assert(datumClass.visibleFromScheme === true)
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

    inside(classes("Datum")) { case (datumClass: RootCellClass) =>
      assert(datumClass.visibleFromScheme === false)
    }
  }

  test("root class with additional fields") {
    val processedTypes = processString("""
      root cell Datum typetag typeId {
        int32 typeId;
        const int8 gcState = 16;
      };
    """)

    val classes = processedTypes.cellClasses
    assert(processedTypes.nextMetadataIndex === 12)

    inside(classes("Datum")) { case (datumClass: RootCellClass) =>
      assert(datumClass.name === "Datum")
      assert(datumClass.instanceType === CellClass.Abstract)
      assert(datumClass.visibleFromScheme === true)
      assert(datumClass.typeId === None)

      val List(typeIdField, gcStateField) = datumClass.fields

      assert(typeIdField.fieldType === PrimitiveFieldType(
        Some(true),
        llvmir.IntegerType(32),
        "std::int32_t"
      ))
      assert(typeIdField.isConst === false)
      assert(typeIdField.initializer === None)

      assert(gcStateField.fieldType === PrimitiveFieldType(
        Some(true),
        llvmir.IntegerType(8),
        "std::int8_t"
      ))
      assert(gcStateField.isConst === true)
      assert(gcStateField.initializer === Some(16))

      val typeIdTbaaNode = datumClass.fieldTbaaNodes(typeIdField)
      assert(typeIdTbaaNode.index === 10)
      inside(typeIdTbaaNode.metadataNode) {
        case tbaaMetadata: llvmir.TbaaMetadata =>
          assert(tbaaMetadata.parentOpt === Some(llvmir.NumberedMetadata(0)))
      }

      val gcStateTbaaNode = datumClass.fieldTbaaNodes(gcStateField)
      assert(gcStateTbaaNode.index === 11)
      inside(gcStateTbaaNode.metadataNode) {
        case tbaaMetadata: llvmir.TbaaMetadata =>
          assert(tbaaMetadata.parentOpt === Some(llvmir.NumberedMetadata(0)))
      }
    }
  }

  test("abstract tagged class with fields of root class with fields") {
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
    assert(processedTypes.nextMetadataIndex === 17)

    val datumClass = classes("Datum")

    inside(classes("StringLike")) { case (stringLikeClass: TaggedCellClass) =>
      assert(stringLikeClass.name === "StringLike")
      assert(stringLikeClass.visibleFromScheme === true)
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
      assert(parentTypeIdTbaaNode.index === 10)
      inside(parentTypeIdTbaaNode.metadataNode) {
        case tbaaMetadata: llvmir.TbaaMetadata =>
          assert(tbaaMetadata.parentOpt === Some(llvmir.NumberedMetadata(0)))
      }

      val parentGcStateTbaaNode = datumClass.fieldTbaaNodes(gcStateField)
      assert(parentGcStateTbaaNode.index === 11)
      inside(parentGcStateTbaaNode.metadataNode) {
        case tbaaMetadata: llvmir.TbaaMetadata =>
          assert(tbaaMetadata.parentOpt === Some(llvmir.NumberedMetadata(0)))
      }

      // Now our inherited nodes
      val childTypeIdTbaaNode = stringLikeClass.fieldTbaaNodes(typeIdField)
      assert(childTypeIdTbaaNode.index === 12)
      inside(childTypeIdTbaaNode.metadataNode) {
        case tbaaMetadata: llvmir.TbaaMetadata =>
          assert(tbaaMetadata.parentOpt === Some(parentTypeIdTbaaNode.numberedMetadata))
      }

      val childGcStateTbaaNode = stringLikeClass.fieldTbaaNodes(gcStateField)
      assert(childGcStateTbaaNode.index === 13)
      inside(childGcStateTbaaNode.metadataNode) {
        case tbaaMetadata: llvmir.TbaaMetadata =>
          assert(tbaaMetadata.parentOpt === Some(parentGcStateTbaaNode.numberedMetadata))
      }

      // And finally a new nodes
      val charCountNode = stringLikeClass.fieldTbaaNodes(charCountField)
      assert(charCountNode.index === 14)
      inside(charCountNode.metadataNode) {
        case tbaaMetadata: llvmir.TbaaMetadata =>
          assert(tbaaMetadata.parentOpt === Some(llvmir.NumberedMetadata(0)))
      }
    }
  }

  test("concrete and preconstructed tagged classes") {
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
    assert(processedTypes.nextMetadataIndex === 13)

    val datumClass = classes("Datum")

    inside(classes("Boolean")) { case (booleanClass: TaggedCellClass) =>
      assert(booleanClass.name === "Boolean")
      assert(booleanClass.visibleFromScheme === true)
      assert(booleanClass.typeId === Some(1))
      assert(booleanClass.instanceType === CellClass.Preconstructed)
      assert(booleanClass.parent === datumClass)
    }

    inside(classes("Character")) { case (characterClass: TaggedCellClass) =>
      assert(characterClass.name === "Character")
      assert(characterClass.visibleFromScheme === true)
      assert(characterClass.typeId === Some(2))
      assert(characterClass.instanceType === CellClass.Concrete)
      assert(characterClass.parent === datumClass)
    }
  }

  test("tagged classes with variants") {
    val processedTypes = processString("""
      root cell Datum typetag typeId {
        int32 typeId;
      };

      concrete cell String : Datum {
        uint32 byteLength;
      };

      variant cell InlineString : String {
        uint8 inlineData[8];
      };

      variant cell HeapString : String {
        uint8 *heapData;
      };
    """)

    val classes = processedTypes.cellClasses
    assert(processedTypes.nextMetadataIndex === 15)

    val datumClass = classes("Datum")

    inside((classes("String"), classes("InlineString"), classes("HeapString"))) {
      case (stringClass: TaggedCellClass, inlineVariant: VariantCellClass, heapVariant: VariantCellClass) =>
        assert(stringClass.name === "String")
        assert(stringClass.visibleFromScheme === true)
        assert(stringClass.typeId === Some(1))
        assert(stringClass.instanceType === CellClass.Concrete)
        assert(stringClass.parent === datumClass)

        val List(byteLengthField) = stringClass.fields
        assert(byteLengthField.name === "byteLength")

        assert(inlineVariant.name === "InlineString")
        assert(inlineVariant.instanceType === CellClass.Variant)
        assert(inlineVariant.parent === stringClass)

        val List(inlineDataField) = inlineVariant.fields
        assert(inlineDataField.name === "inlineData")

        assert(heapVariant.name === "HeapString")
        assert(heapVariant.instanceType === CellClass.Variant)
        assert(heapVariant.parent === stringClass)

        val List(heapDataField) = heapVariant.fields
        assert(heapDataField.name === "heapData")
    }
  }

  test("inheriting from non-abstract tagged cell class fails") {
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

  test("variant inheriting from abstact cell class fails") {
    intercept[InheritingAbstractCellClassException] {
      processString("""
        root cell Datum typetag typeId {
          int32 typeId;
        };

        variant cell Character : Datum {
        };
      """)
    }
  }

  test("inheriting from variant cell class fails") {
    intercept[InheritingVariantCellClassException] {
      processString("""
        root cell Datum typetag typeId {
          int32 typeId;
        };

        concrete cell Character : Datum {
        };

        variant cell InlineCharacter : Character {
        };

        concrete cell EmojiCharacter : InlineCharacter {
        };
      """)
    }
  }
}
