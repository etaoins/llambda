package io.llambda.typegen.writer.compiler
import io.llambda.typegen._

import scala.io.Source

import io.llambda.llvmir

object WriteScalaCellTypes extends writer.OutputWriter {
  private sealed abstract class ConstructorValue {
    val field: CellField
    // If this is None we don't need a parameter
    val paramScalaType: Option[String]
    val valueExpr: String
  }

  private class IrConstantConstructorValue(val field: CellField) extends ConstructorValue {
    val paramScalaType = Some("IrConstant")
    val valueExpr = s"${field.name}"
  }

  private class IntegerConstructorValue(val field: CellField) extends ConstructorValue {
    val paramScalaType = Some("Long")
    val valueExpr = s"IntegerConstant(${field.name}IrType, ${field.name})"
  }

  private class InitializedConstructorValue(val field: CellField, initializer: Long) extends ConstructorValue {
    val paramScalaType = None
    val valueExpr = s"IntegerConstant(${field.name}IrType, ${initializer})"
  }

  private class KnownTypeIdConstructorValue(val field: CellField) extends ConstructorValue {
    val paramScalaType = Some("Long")
    val valueExpr = field.name
  }

  private case class ConstructorValues(
    selfValues: List[ConstructorValue],
    superValues: List[ConstructorValue]
  )

  private def resourceAsString(resourcePath: String): String = {
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    Source.fromInputStream(stream, "UTF-8").mkString
  }

  private def writeFieldTrait(scalaBuilder: ScalaBuilder, cellClass: CellClass) {
    val traitName = cellClass.names.scalaFieldsTraitName

    val extendsClause = cellClass.parentOption.map({ parent =>
      s" extends ${parent.names.scalaFieldsTraitName}"
    }).getOrElse("")

    scalaBuilder += "sealed trait " + traitName + extendsClause

    // Output our field information
    scalaBuilder.blockSep {
      scalaBuilder += s"val irType: FirstClassType"
      scalaBuilder.sep()

      // Output our field types
      for(field <- cellClass.fields) {
        // Find the field's LLVM type
        val llvmType = FieldTypeToLlvm(field.fieldType)
        // Build a Scala constructor for it
        val scalaConstructor = LlvmTypeToScalaConstructor(llvmType)

        scalaBuilder += s"val ${field.name}IrType = ${scalaConstructor}"
        scalaBuilder += s"val ${field.name}TbaaNode: Metadata"
        scalaBuilder += s"val ${field.name}GepIndices: List[Int]"
        scalaBuilder.sep()
      }

      scalaBuilder.sep()

      // Output our abstract field accessors
      for(field <- cellClass.fields) {
        // These are reused by every method
        val capitalizedName = field.name.capitalize
        val pointerVarName = field.name + "Ptr"

        // This generates a pointer to the field
        scalaBuilder += s"def genPointerTo${capitalizedName}(block: IrBlockBuilder)(valueCell: IrValue): IrValue ="
        scalaBuilder.blockSep {
          scalaBuilder += "if (valueCell.irType != PointerType(irType))"
          scalaBuilder.block {
            // This is tricky because our generated code is using string interpolation
            // Note this is evaluated in the generated code, not at generation time
            val exceptionMessage = "Unexpected type for cell value. Passed " + "$" + "{valueCell.irType}, expected " + "$" + "{PointerType(irType)}"
            scalaBuilder += s"""throw new InternalCompilerErrorException(s"${exceptionMessage}")"""
          }

          scalaBuilder.sep()

          scalaBuilder += s"""block.getelementptr("${pointerVarName}")("""
          scalaBuilder.indented {
            scalaBuilder += s"elementType=${field.name}IrType,"
            scalaBuilder +=  "basePointer=valueCell,"
            scalaBuilder += s"indices=${field.name}GepIndices.map(IntegerConstant(IntegerType(32), _)),"
            scalaBuilder += "inbounds=true"
          }
          scalaBuilder += ")"
        }

        // This generates a TBAA-annotated store to the field
        scalaBuilder += s"def genStoreTo${capitalizedName}(block: IrBlockBuilder)(toStore: IrValue, valueCell: IrValue, metadata: Map[String, Metadata] = Map()) "
        scalaBuilder.blockSep {
          scalaBuilder += s"val ${pointerVarName} = genPointerTo${capitalizedName}(block)(valueCell)"
          scalaBuilder += s"""val allMetadata = metadata ++ Map("tbaa" -> ${field.name}TbaaNode)"""
          scalaBuilder += s"block.store(toStore, ${pointerVarName}, metadata=allMetadata)"
        }

        scalaBuilder += s"def genLoadFrom${capitalizedName}(block: IrBlockBuilder)(valueCell: IrValue, metadata: Map[String, Metadata] = Map()): IrValue ="

        val tbaaMetadata = s""""tbaa" -> ${field.name}TbaaNode"""
        val invariantLoadMetadataOpt = if (field.isConst) {
          Some(""""invariant.load" -> NumberedMetadata(7L)""")
        }
        else {
          None
        }

        val standardMetadata = List(tbaaMetadata) ++ invariantLoadMetadataOpt
        val standardMetadataInitialiser = s"Map(${standardMetadata.mkString(", ")})"

        scalaBuilder.blockSep {
          scalaBuilder += s"val ${pointerVarName} = genPointerTo${capitalizedName}(block)(valueCell)"
          scalaBuilder += s"""val allMetadata = ${standardMetadataInitialiser} ++ metadata"""
          scalaBuilder += s"""block.load("${field.name}")(${pointerVarName}, metadata=allMetadata)"""
        }
      }
    }
  }

  private def collectConstructorValues(typeTagField: CellField, cellClass: CellClass): ConstructorValues = {
    // Does this cell class like have a defined type ID field?
    val hasTypeId = cellClass.instanceType != CellClass.Abstract

    val selfValues = cellClass.fields map { field =>
      val llvmType = FieldTypeToLlvm(field.fieldType)

      if (field.initializer.isDefined) {
        // This is initialized with a fixed integer value
        new InitializedConstructorValue(field, field.initializer.get)
      }
      else if (llvmType.isInstanceOf[llvmir.IntegerType]) {
        new IntegerConstructorValue(field)
      }
      else {
        new IrConstantConstructorValue(field)
      }
    }

    cellClass.parentOption match {
      case Some(parentCellClass) =>
        // Call ourselves recursively
        val parentValuesResult = collectConstructorValues(typeTagField, parentCellClass)

        val allParentValues = (parentValuesResult.selfValues ++ parentValuesResult.superValues) flatMap { parentValue =>
          if (parentValue.isInstanceOf[KnownTypeIdConstructorValue]) {
            // This has already been supplied; we can forget about it completely
            None
          }
          else if ((parentValue.field == typeTagField) && hasTypeId) {
            // We can fill this in without requesting it from the caller
            Some(new KnownTypeIdConstructorValue(parentValue.field))
          }
          else {
            Some(parentValue)
          }
        }

        // Our parents self values become our super values
        ConstructorValues(selfValues, allParentValues)

      case None =>
        ConstructorValues(selfValues, Nil)
    }
  }

  private def writeConstructor(scalaBuilder: ScalaBuilder, typeTagField: CellField, cellClass: CellClass) {
    val constructorValues = collectConstructorValues(typeTagField, cellClass)
    val allValues = constructorValues.selfValues ++ constructorValues.superValues

    val methodParams = allValues.filter({ value =>
      // If paramScalaType is None then that value doesn't need a parameter
      value.paramScalaType.isDefined && !value.isInstanceOf[KnownTypeIdConstructorValue]
    }).map({ paramValue =>
      s"${paramValue.field.name}: ${paramValue.paramScalaType.get}"
    })

    scalaBuilder += s"""def createConstant(${methodParams.mkString(", ")}): StructureConstant ="""
    scalaBuilder.blockSep {
      // Make sure our fields are of the correct type
      for(selfValue <- constructorValues.selfValues if selfValue.paramScalaType == Some("IrConstant")) {
        scalaBuilder += s"if (${selfValue.field.name}.irType != ${selfValue.field.name}IrType)"
        scalaBuilder.blockSep {
          scalaBuilder += s"""throw new InternalCompilerErrorException("Unexpected type for field ${selfValue.field.name}")"""
        }
      }

      // Create the constructor for our supertype field
      val superFieldOpt = cellClass.parentOption map { parent =>
        val superConstructorFields = for(superValue <- constructorValues.superValues if superValue.paramScalaType.isDefined) yield
          s"${superValue.field.name}=${superValue.field.name}"

        parent.names.scalaCellTypeName + ".createConstant(" + superConstructorFields.mkString(", ") + ")"
      }

      // Calculate our selffields fields
      val selfFields =  for(selfValue <- constructorValues.selfValues) yield
        selfValue.valueExpr

      // Create our constant
      scalaBuilder += "StructureConstant(List("
      scalaBuilder.indented {
        scalaBuilder.appendLines(superFieldOpt.toList ++ selfFields, ",")
      }

      scalaBuilder += "), userDefinedType=Some(irType))"
    }
  }

  private def writeGepIndices(scalaBuilder: ScalaBuilder, cellClass: CellClass, depth: Int = 1) {
    // Write our parent fields first so they come out in memory order
    for(parent <- cellClass.parentOption) {
      writeGepIndices(scalaBuilder, parent, depth + 1)
    }

    // If we have a parent that takes the position of the first field
    val fieldBaseIndex = if (cellClass.parentOption.isDefined) {
      1
    }
    else {
      0
    }

    for((field, fieldIndex) <- cellClass.fields.zipWithIndex) {
      val gepIndices = List.fill(depth)(0) :+ (fieldBaseIndex + fieldIndex)

      scalaBuilder += s"""val ${field.name}GepIndices = List(${gepIndices.mkString(", ")})"""
    }
  }

  private def writeCellObject(scalaBuilder: ScalaBuilder, processedTypes: ProcessedTypes, cellClass: CellClass) = {
    val names  = cellClass.names

    val scalaSuperclass = cellClass.instanceType match {
      case CellClass.Abstract =>
        // This is an abstract class
        "CellType"

      case CellClass.Variant =>
        // This a variant
        "CellTypeVariant"

      case CellClass.Concrete =>
        "ConcreteCellType"

      case CellClass.Preconstructed =>
        "PreconstructedCellType"
    }

    scalaBuilder += s"object ${names.scalaCellTypeName} extends ${scalaSuperclass} with ${names.scalaFieldsTraitName}"

    scalaBuilder.blockSep {
      val irType = llvmir.UserDefinedType(names.llvmName)
      val scalaConstructor = LlvmTypeToScalaConstructor(irType)

      scalaBuilder += "val llvmName = \"" + names.llvmName + "\""
      scalaBuilder += s"val irType = ${scalaConstructor}"

      // Variants aren't properly part of the Scheme type tree
      // This is because our automatic type determination machinery cannot
      // distinguish them from the tagged cell class they inherit
      if (cellClass.instanceType != CellClass.Variant) {
        scalaBuilder += "val schemeName = \"" + names.schemeName + "\""

        // Get all of our subclasses
        val subtypes = processedTypes.taggedCellClassesByParent.getOrElse(cellClass, Nil)
        val subtypeParts = subtypes filter(_.visibility.fromCompiler) map { taggedClass =>
          taggedClass.names.scalaCellTypeName
        }

        scalaBuilder += s"""val directSubtypes = Set[CellType](${subtypeParts.mkString(", ")})"""

        scalaBuilder.sep()

        for(typeId <- cellClass.typeId) {
          val typeTagFieldName = processedTypes.rootCellClass.typeTagField.name
          scalaBuilder += s"val ${typeTagFieldName} = ${typeId}L"
        }
      }

      scalaBuilder.sep()

      // Create our GEP indices
      writeGepIndices(scalaBuilder, cellClass)

      scalaBuilder.sep()

      // Add our TBAA indexes
      for((field, metadataDef) <- cellClass.fieldTbaaNodes) {
        scalaBuilder += s"val ${field.name}TbaaNode = NumberedMetadata(${metadataDef.index}L)"
      }

      // Cell classes don't repeat their parent TBAA nodes because they use the
      // same values as their parent. However, they need to be in the Scala
      // object for our accessors to work
      cellClass match {
        case variantCellClass: VariantCellClass =>
          for((field, tbaaNode) <- variantCellClass.parent.fieldTbaaNodes) {
            scalaBuilder += s"val ${field.name}TbaaNode = NumberedMetadata(${tbaaNode.index}L)"
          }

        case _ =>
      }

      // Add our constructor if we're not preconstructed
      if (cellClass.instanceType != CellClass.Preconstructed) {
        scalaBuilder.sep()
        writeConstructor(scalaBuilder, processedTypes.rootCellClass.typeTagField, cellClass)
      }
    }
  }

  def apply(processedTypes: ProcessedTypes): Map[String, String] = {
    val initialTemplate = resourceAsString("CellType.template.scala")

    // Expand our template
    val rootClassFieldsTrait = processedTypes.rootCellClass.names.scalaFieldsTraitName
    val expandedTemplate = (initialTemplate
      .replaceAllLiterally("$" + "{ROOT_CLASS_FIELDS_TRAIT}", rootClassFieldsTrait)
      .replaceAllLiterally("$" + "{NEXT_METADATA_INDEX}", processedTypes.nextMetadataIndex.toString)
      .replaceAllLiterally("$" + "{TYPE_TAG_FIELD_NAME}", processedTypes.rootCellClass.typeTagField.name)
    )

    // Make a source builder
    val scalaBuilder = new ScalaBuilder

    scalaBuilder.appendRaw(writer.GeneratedClikeFileComment)
    scalaBuilder.appendRaw(expandedTemplate)
    scalaBuilder.sep()

    for(cellClass <- processedTypes.cellClasses.values if cellClass.visibility.fromCompiler) {
      // Write the fields trait
      writeFieldTrait(scalaBuilder, cellClass)

      // Write the cell object
      writeCellObject(scalaBuilder, processedTypes, cellClass)
    }

    Map("compiler/src/main/scala/celltype/generated/CellType.scala" -> scalaBuilder.toString)
  }
}
