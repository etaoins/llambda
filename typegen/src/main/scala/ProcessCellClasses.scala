package io.llambda.typegen

import io.llambda.llvmir

import collection.immutable.ListMap

object ProcessCellClasses {
  /** Simple class encapsulting an incrementing integer counter */
  private class IntCounter {
    var nextValue : Int = 0

    def apply() : Int = {
      val currentValue = nextValue
      nextValue = nextValue + 1

      currentValue
    }
  }

  /** Creates TBAA nodes for our fields and the fields we inherit from our superclasses */
  private def createTbaaNodes(selfName : String, selfFields : Map[String, CellField], parentClass : Option[CellClass], indexCounter : () => Int) : Map[CellField, llvmir.IrTbaaNode] = {
    // Inherit the TBAA nodes from our parents
    val inheritsNodes = parentClass.map(_.fieldTbaaNodes).getOrElse(Map.empty).map { case (cellField, parentTbaaNode) =>
      val identity = s"${parentTbaaNode.identity}->${selfName}"
      val tbaaIndex = indexCounter()

      (cellField -> llvmir.IrTbaaNode(tbaaIndex, identity, Some(parentTbaaNode.index)))
    }

    // Create parentless TBAA nodes for the new fields we introduce
    val selfNodes = (selfFields.map { case (fieldName, cellField) =>
      val identity = s"${selfName}::${fieldName}"
      val tbaaIndex = indexCounter()

      (cellField -> llvmir.IrTbaaNode(tbaaIndex, identity, None))
    }).toMap

    selfNodes ++ inheritsNodes
  }

  /** Converts a list of parsed fields in to CellField instances by resolving their types */ 
  private def processFields(fieldTypes : Map[String, FieldType])(parsedFields : List[ParsedCellField]) : ListMap[String, CellField] = {
    parsedFields.foldLeft(ListMap[String, CellField]()) { (seenFields, parsedField) =>
      if (seenFields.contains(parsedField.name)) {
        throw new DuplicateFieldNameException(parsedField)
      }

      val resolvedType = ResolveParsedType(fieldTypes)(parsedField.fieldType)
      seenFields + (parsedField.name -> new CellField(resolvedType))
    }
  }

  def apply(fieldTypes : Map[String, FieldType])(definitions : List[ParsedDefinition]) : ProcessedTypes = {
    val parsedCellDefs = definitions.collect {
      case parsedCellDef : ParsedCellClassDefinition =>
        parsedCellDef
    }

    val typeIdGenerator = new IntCounter
    val tbbaIndexGenerator = new IntCounter

    val cellClasses = parsedCellDefs.foldLeft(ListMap[String, CellClass]()) { case (cellClasses, parsedCellDef) =>
      // Process our fields
      val cellFields = processFields(fieldTypes)(parsedCellDef.fields)

      // Assign us a type ID if one is needed
      val typeId = if (parsedCellDef.instanceType == CellClass.Abstract) {
        None
      }
      else {
        Some(typeIdGenerator())
      }
      
      // Find our parent class
      val parentCellClass = parsedCellDef.optionalParent map { parentName =>
        cellClasses.getOrElse(parentName, {
          // Our parent hasn't been declared yet
          throw new UndefinedCellClassException(parsedCellDef, parentName)
        })
      }

      // Assign our TBAA nodes
      val fieldTbaaNodes = createTbaaNodes(parsedCellDef.name, cellFields, parentCellClass, tbbaIndexGenerator.apply)

      // Turn each cell definition in to a cell class
      val cellClass = parsedCellDef match {
        case rootClass : ParsedRootClassDefinition =>
          RootCellClass(
            name=rootClass.name,
            fields=cellFields,
            internal=rootClass.internal,
            fieldTbaaNodes=fieldTbaaNodes
          )

        case childClass : ParsedChildClassDefinition =>
          ChildCellClass(
            name=childClass.name,
            instanceType=childClass.instanceType,
            parent=parentCellClass.get,
            fields=cellFields,
            internal=childClass.internal,
            typeId=typeId,
            fieldTbaaNodes=fieldTbaaNodes
          )
      }

      // Preserve our source position for late error reporting
      val positionedCellClass = cellClass.setPos(parsedCellDef.pos)

      cellClasses + (cellClass.name -> positionedCellClass)
    }
    
    // Make sure we have exactly one root cell class
    val rootCellClassOpt = cellClasses.values.foldLeft(None : Option[CellClass]) { (seenRootClass, cellClass) =>
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

    ProcessedTypes(
      nextTbaaIndex=tbbaIndexGenerator(),
      fieldTypes=fieldTypes,
      cellClasses=cellClasses,
      rootCellClass=rootCellClass
    )
  }
}
