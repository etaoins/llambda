package io.llambda.typegen.writer.compiler
import io.llambda.typegen._

import scala.io.Source

import io.llambda.llvmir

object WriteScalaObjects extends writer.OutputWriter {
  private sealed abstract class ConstructorValue {
    val field : CellField
    // If this is None we don't need a parameter
    val paramScalaType : Option[String]
    val valueExpr : String
  }

  private class IrConstantConstructorValue(val field : CellField) extends ConstructorValue { 
    val paramScalaType = Some("IrConstant")
    val valueExpr = s"${field.name}"
  }

  private class IntegerConstructorValue(val field : CellField) extends ConstructorValue {
    val paramScalaType = Some("Long")
    val valueExpr = s"IntegerConstant(${field.name}IrType, ${field.name})"
  }

  private class InitializedConstructorValue(val field : CellField, initializer : Long) extends ConstructorValue {
    val paramScalaType = None
    val valueExpr = s"IntegerConstant(${field.name}IrType, ${initializer})"
  }

  private case class ConstructorValues(
    selfValues : List[ConstructorValue],
    superValues : List[ConstructorValue]
  )

  private def resourceAsString(resourcePath : String) : String = {
    val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)
    Source.fromInputStream(stream, "UTF-8").mkString
  }

  private def writeFieldTrait(scalaBuilder : ScalaBuilder, cellClass : CellClass) {
    // Unlike the cell class objects the fields classes have an inheritence
    // tree matching the cell classes themselves
    val extendsClause = cellClass.parentOption.map({ parent =>
      s" extends ${parent.names.scalaFieldsTraitName}"
    }).getOrElse("") 

    scalaBuilder += "sealed trait " + cellClass.names.scalaFieldsTraitName + extendsClause

    // Output our field information
    scalaBuilder.blockSep {
      scalaBuilder += s"val irType : FirstClassType" 
      scalaBuilder.sep()

      // Output our field types
      for((name, field) <- cellClass.fields) {
        // Find the field's LLVM type
        val llvmType = FieldTypeToLlvm(field.fieldType)
        // Build a Scala constructor for it
        val scalaConstructor = LlvmTypeToScalaConstructor(llvmType)

        scalaBuilder += s"val ${name}IrType = ${scalaConstructor}"
        scalaBuilder += s"val ${field.name}TbaaIndex : Long" 
        scalaBuilder += s"val ${field.name}GepIndices : List[Int]"
        scalaBuilder.sep()
      }

      scalaBuilder.sep()

      // Output our abstract field accessors 
      for((fieldName, field) <- cellClass.fields) {
        // These are reused by every method
        val capitalizedName = fieldName.capitalize
        val pointerVarName = fieldName + "Ptr"

        // This generates a pointer to the field
        scalaBuilder += s"def genPointerTo${capitalizedName}(block : IrBlockBuilder)(valueCell : IrValue) : IrValue ="
        scalaBuilder.blockSep {
          scalaBuilder += "if (valueCell.irType != PointerType(irType))"
          scalaBuilder.block {
            // This is tricky because our generated code is using string interpolation
            // Note this is evaluated in the generated code, not at generation time
            val exceptionMessage = "Unexpected type for cell value. Passed ${valueCell.irType}, expected ${PointerType(irType)}"
            scalaBuilder += s"""throw new InternalCompilerErrorException(s"${exceptionMessage}")"""
          }

          scalaBuilder.sep()

          scalaBuilder += s"""block.getelementptr("${pointerVarName}")("""
          scalaBuilder.indented {
            scalaBuilder += s"elementType=${fieldName}IrType,"
            scalaBuilder +=  "basePointer=valueCell,"
            scalaBuilder += s"indices=${fieldName}GepIndices.map(IntegerConstant(IntegerType(32), _)),"
            scalaBuilder += "inbounds=true"
          }
          scalaBuilder += ")"
        }
  
        // This generates a TBAA-annotated store to the field
        scalaBuilder += s"def genStoreTo${capitalizedName}(block : IrBlockBuilder)(toStore : IrValue, valueCell : IrValue) "
        scalaBuilder.blockSep {
          scalaBuilder += s"val ${pointerVarName} = genPointerTo${capitalizedName}(block)(valueCell)"
          scalaBuilder += s"block.store(toStore, ${pointerVarName}, tbaaIndex=Some(${fieldName}TbaaIndex))"
        }
  
        scalaBuilder += s"def genLoadFrom${capitalizedName}(block : IrBlockBuilder)(valueCell : IrValue) : IrValue ="
        scalaBuilder.blockSep {
          scalaBuilder += s"val ${pointerVarName} = genPointerTo${capitalizedName}(block)(valueCell)"
          scalaBuilder += s"""block.load("${fieldName}")(${pointerVarName}, tbaaIndex=Some(${fieldName}TbaaIndex))"""
        }
      }
    }
  }

  private def collectConstructorValues(cellClass : CellClass) : ConstructorValues = {
    val selfValues = cellClass.fields.values.toList.map { field =>
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
        val parentValuesResult = collectConstructorValues(parentCellClass)

        val allParentValues = (parentValuesResult.selfValues ++ parentValuesResult.superValues)

        // Our parents self values become our super values
        ConstructorValues(selfValues, allParentValues)

      case None =>
        ConstructorValues(selfValues, Nil)
    }
  }

  private def writeConstructor(scalaBuilder : ScalaBuilder, typeTagField : CellField, cellClass : CellClass) {
    val constructorValues = collectConstructorValues(cellClass)
    val allValues = constructorValues.selfValues ++ constructorValues.superValues

    val methodParams = allValues.filter({ value =>
      // If paramScalaType is None then that value doesn't need a parameter
      value.paramScalaType.isDefined 
    }).filterNot({ value =>
      // If it's for typeId and we have one then remove it and it will be 
      // provided by our member variable
      (value.field == typeTagField) && (cellClass.typeId.isDefined)
    })map({ paramValue =>
      s"${paramValue.field.name} : ${paramValue.paramScalaType.get}"
    })

    scalaBuilder += s"""def createConstant(${methodParams.mkString(", ")}) : StructureConstant ="""
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

        "supertype.get.createConstant(" + superConstructorFields.mkString(", ") + ")"
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

  private def writeGepIndices(scalaBuilder : ScalaBuilder, cellClass : CellClass, depth : Int = 1) {
    // Write our parent fields first so they come out in memory order
    for(parent <- cellClass.parentOption) {
      writeGepIndices(scalaBuilder, parent, depth + 1)
    }
    
    // If we have a parent that takes the position ofV the first field
    val fieldBaseIndex = if (cellClass.parentOption.isDefined) {
      1
    }
    else {
      0
    }

    for(((fieldName, field), fieldIndex) <- cellClass.fields.zipWithIndex) {
      val gepIndices = List.fill(depth)(0) :+ (fieldBaseIndex + fieldIndex) 

      scalaBuilder += s"""val ${fieldName}GepIndices = List(${gepIndices.mkString(", ")})"""
    }
  }

  private def writeCellObject(scalaBuilder : ScalaBuilder, processedTypes : ProcessedTypes, cellClass : CellClass) = {
    val names  = cellClass.names

    val scalaSuperclass = if (cellClass.instanceType == CellClass.Abstract) {
      // This is an abstract class
      "CellType"
    }
    else {
      // This is concrete
      "ConcreteCellType"
    }

    scalaBuilder += s"object ${names.scalaObjectName} extends ${scalaSuperclass} with ${names.scalaFieldsTraitName}"

    scalaBuilder.blockSep {
      val irType = llvmir.UserDefinedType(names.llvmName)
      val scalaConstructor = LlvmTypeToScalaConstructor(irType)

      scalaBuilder += "val llvmName = \"" + names.llvmName + "\"" 
      scalaBuilder += s"val irType = ${scalaConstructor}"
      scalaBuilder += "val supertype = " + (cellClass.parentOption match {
        case None => "None"
        case Some(parent) => s"Some(${parent.names.scalaObjectName})"
      })

      // Get all of our subclasses
      val subtypeParts = processedTypes.cellClassesByParent.getOrElse(cellClass, Nil) map { childClass =>
        childClass.names.scalaObjectName
      }

      scalaBuilder += s"""val directSubtypes = Set[CellType](${subtypeParts.mkString(", ")})"""

      scalaBuilder.sep()

      for(typeId <- cellClass.typeId) {
        val typeTagFieldName = processedTypes.rootCellClass.typeTagField.name
        scalaBuilder += s"val ${typeTagFieldName} = ${typeId}L"
      }

      scalaBuilder.sep()

      // Create our GEP indices
      writeGepIndices(scalaBuilder, cellClass)

      scalaBuilder.sep()

      // Add our TBAA indexes
      for((field, tbaaNode) <- cellClass.fieldTbaaNodes) {
        scalaBuilder += s"val ${field.name}TbaaIndex = ${tbaaNode.index}L" 
      }

      // Add our constructor if we're not preconstructed
      if (cellClass.instanceType != CellClass.Preconstructed) {
        scalaBuilder.sep()
        writeConstructor(scalaBuilder, processedTypes.rootCellClass.typeTagField, cellClass)
      }
    }
  }

  def apply(processedTypes : ProcessedTypes) : Map[String, String] = {
    val initialTemplate = resourceAsString("CellType.template.scala")

    // Expand our template
    val rootClassFieldsTrait = processedTypes.rootCellClass.names.scalaFieldsTraitName
    val expandedTemplate = (initialTemplate
      .replaceAllLiterally("${ROOT_CLASS_FIELDS_TRAIT}", rootClassFieldsTrait)
      .replaceAllLiterally("${NEXT_TBAA_INDEX}", processedTypes.nextTbaaIndex.toString)
      .replaceAllLiterally("${TYPE_TAG_FIELD_NAME}", processedTypes.rootCellClass.typeTagField.name)
    )

    // Make a source builder
    val scalaBuilder = new ScalaBuilder

    scalaBuilder.appendRaw(writer.GeneratedClikeFileComment)
    scalaBuilder.appendRaw(expandedTemplate)
    scalaBuilder.sep()

    for(cellClass <- processedTypes.cellClasses.values) {
      // Write the fields trait
      writeFieldTrait(scalaBuilder, cellClass)

      // Write the cell object
      writeCellObject(scalaBuilder, processedTypes, cellClass)
    }
    
    Map("compiler/src/main/scala/celltype/generated/CellType.scala" -> scalaBuilder.toString)
  }
}
