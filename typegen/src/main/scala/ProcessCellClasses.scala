package io.llambda.typegen

import collection.immutable.ListMap

import io.llambda.llvmir

object ProcessCellClasses {
  /** Simple class encapsulting an incrementing integer counter */
  private class IntCounter(initialValue : Int = 0) extends Function0[Int] {
    var nextValue : Int = initialValue

    def apply() : Int = {
      val currentValue = nextValue
      nextValue = nextValue + 1

      currentValue
    }
  }

  private def createSelfTbaaNodes(selfName : String, selfFields : List[CellField], indexCounter : () => Int) : ListMap[CellField, llvmir.NumberedMetadataDef] = 
    // Create parentless TBAA nodes for the new fields we introduce
    ListMap(selfFields.map { cellField =>
      val identity = s"${selfName}::${cellField.name}"
      val tbaaNode = llvmir.TbaaMetadata(identity, None)

      val tbaaIndex = indexCounter()
      (cellField -> llvmir.NumberedMetadataDef(tbaaIndex, tbaaNode))
    } : _*)

  /** Creates TBAA nodes for our fields and the fields we inherit from our superclasses */
  private def createTbaaNodes(selfName : String, selfFields : List[CellField], parentTbaaNodes : ListMap[CellField, llvmir.NumberedMetadataDef], indexCounter : () => Int) : ListMap[CellField, llvmir.NumberedMetadataDef] = {
    // Inherit the TBAA nodes from our parents
    val inheritsNodes = parentTbaaNodes.map {
      case (cellField, parentNumberedMetadataDef @ llvmir.NumberedMetadataDef(_, parentTbaaNode : llvmir.TbaaMetadata)) =>
        val identity = s"${parentTbaaNode.identity}->${selfName}"
        val tbaaNode = llvmir.TbaaMetadata(identity, Some(parentNumberedMetadataDef.numberedMetadata))

        val tbaaIndex = indexCounter()
        (cellField -> llvmir.NumberedMetadataDef(tbaaIndex, tbaaNode))
    }

    // Create parentless TBAA nodes for the new fields we introduce
    inheritsNodes ++ createSelfTbaaNodes(selfName, selfFields, indexCounter)
  }

  private def processField(fieldTypes : Map[String, FieldType])(parsedField : ParsedCellField) : CellField = {
    val resolvedType = ResolveParsedType(fieldTypes)(parsedField.fieldType)

    if (parsedField.initializer.isDefined) {
      // We only support integer intiializers
      FieldTypeToLlvm(resolvedType) match {
        case llvmir.IntegerType(_) => // Good
        case _ =>
          throw new InitializingNonIntegralFieldException(parsedField)
      }
    }

    val cellField = new CellField(parsedField.name, resolvedType, parsedField.initializer)
    cellField.setPos(parsedField.pos)
  }

  private def collectFieldNames(cellClass : CellClass) : Set[String] = {
    val fieldNames = cellClass.fields.map(_.name).toSet

    cellClass.parentOption match {
      case None =>
        // We're at the top!
        fieldNames

      case Some(parentCellClass) =>
        fieldNames ++ collectFieldNames(parentCellClass)
    }
  }

  def apply(fieldTypes : Map[String, FieldType])(definitions : List[ParsedDefinition]) : ProcessedTypes = {
    val parsedCellDefs = definitions.collect {
      case parsedCellDef : ParsedCellClassDefinition =>
        parsedCellDef
    }

    val typeIdGenerator = new IntCounter(1)

    // Reserve 10 TBAA nodes for the %world* fields
    val tbaaIndexGenerator = new IntCounter(10)

    val cellClasses = parsedCellDefs.foldLeft(ListMap[String, CellClass]()) { case (cellClasses, parsedCellDef) =>
      // Find our parent class
      val parentCellClassOpt = parsedCellDef.parentOption map { parentName =>
        val parentClass = cellClasses.getOrElse(parentName, {
          // Our parent hasn't been declared yet
          throw new UndefinedCellClassException(parsedCellDef, parentName)
        })

        // It's not possible to inherit from variants at the moment
        if (parentClass.instanceType == CellClass.Variant) {
          throw new InheritingVariantCellClassException(parsedCellDef)
        }

        parentClass
      }

      // Make sure all field names are unique
      val inheritedFieldNames = parentCellClassOpt.map(collectFieldNames).getOrElse(Set[String]())

      // First check our fields
      parsedCellDef.fields.foldLeft(inheritedFieldNames) { (seenFields, parsedField) =>
        if (seenFields.contains(parsedField.name)) {
          throw new DuplicateFieldNameException(parsedField)
        }

        seenFields + parsedField.name
      }

      // Process our fields
      val processedFields = parsedCellDef.fields.map(processField(fieldTypes)(_))

      // Assign our TBAA nodes
      val parentTbaaNodes = parentCellClassOpt.map(_.fieldTbaaNodes).getOrElse(ListMap())

      val fieldTbaaNodes = parsedCellDef match {
        case _ : ParsedVariantClassDefinition =>
          // Don't redefine parent nodes
          // We use our parent nodes directly because we can switch between variants
          // of a cell at runtime so it's possible for them to alias
          createSelfTbaaNodes(parsedCellDef.name, processedFields, tbaaIndexGenerator)

        case _ =>
          createTbaaNodes(parsedCellDef.name, processedFields, parentTbaaNodes, tbaaIndexGenerator)
      }

      // Assign us a type ID if one is needed
      val typeId = if (List(CellClass.Concrete, CellClass.Preconstructed).contains(parsedCellDef.instanceType)) {
        Some(typeIdGenerator())
      }
      else {
        None
      }

      // Turn the cell definition in to a cell class
      val cellClass = parsedCellDef match {
        case rootClass : ParsedRootClassDefinition =>
          // Find our type ID tag field
          val typeTagField = processedFields.find(rootClass.typeTagField == _.name).getOrElse({
            throw new UndefinedTypeTagFieldException(rootClass)
          })

          RootCellClass(
            name=rootClass.name,
            typeTagField=typeTagField,
            fields=processedFields,
            visibility=rootClass.visibility,
            fieldTbaaNodes=fieldTbaaNodes
          )

        case taggedClass : ParsedTaggedClassDefinition =>
          val parentCellClass = parentCellClassOpt.get

          // It doesn't make sense to inherit from non-abstract cell classes
          if (parentCellClass.instanceType != CellClass.Abstract) {
            throw new InheritingNonAbstractCellClassException(taggedClass)
          }

          TaggedCellClass(
            name=taggedClass.name,
            instanceType=taggedClass.instanceType,
            parent=parentCellClass,
            fields=processedFields,
            visibility=taggedClass.visibility,
            typeId=typeId,
            fieldTbaaNodes=fieldTbaaNodes
          )

        case variantClass : ParsedVariantClassDefinition =>
          val parentCellClass = parentCellClassOpt.get

          // It doesn't make sense to inherit from abstract cell classes
          if (parentCellClass.instanceType == CellClass.Abstract) {
            throw new InheritingAbstractCellClassException(variantClass)
          }

          VariantCellClass(
            name=variantClass.name,
            parent=parentCellClass,
            fields=processedFields,
            fieldTbaaNodes=fieldTbaaNodes
          )
      }

      // Preserve our source position for late error reporting
      val positionedCellClass = cellClass.setPos(parsedCellDef.pos)

      cellClasses + (cellClass.name -> positionedCellClass)
    }
    
    // Make sure we have exactly one root cell class
    val rootCellClassOpt = cellClasses.values.foldLeft(None : Option[RootCellClass]) { (seenRootClass, cellClass) =>
      cellClass match {
        case rootClass : RootCellClass  =>
          if (seenRootClass.isDefined) {
            throw new DuplicateRootCellClassException(rootClass)
          }

          Some(rootClass)

        case _ => seenRootClass
      }
    }

    val rootCellClass = rootCellClassOpt.getOrElse({
      throw new NoRootCellClassException;
    })

    // Calculate this once so it's easy to navigate the cell class
    // hierarchy downwards
    val taggedCellClassesByParent = cellClasses.values.toList.collect({
      case taggedClass : TaggedCellClass => taggedClass
    }).groupBy(_.parent)

    ProcessedTypes(
      nextMetadataIndex=tbaaIndexGenerator(),
      fieldTypes=fieldTypes,
      cellClasses=cellClasses,
      rootCellClass=rootCellClass,
      taggedCellClassesByParent=taggedCellClassesByParent
    )
  }
}
