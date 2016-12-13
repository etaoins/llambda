package io.llambda.typegen

/** Verifies every abstract cell class has at least one child
  *
  * Childless abstract classes aren't useful and indicate a probable mistake
  * in the cell definition field.
  */
object CheckChildlessAbstractCellClasses {
  def apply(processedTypes: ProcessedTypes): Unit = {
    for(cellClass <- processedTypes.cellClasses.values if cellClass.instanceType == CellClass.Abstract) {
      if (!processedTypes.taggedCellClassesByParent.contains(cellClass)) {
        throw new ChildlessAbstractCellClassException(cellClass)
      }
    }
  }
}

