package io.llambda.typegen

case class ProcessedTypes(
  nextTbaaIndex : Int,
  fieldTypes : Map[String, FieldType],
  cellClasses : Map[String, CellClass],
  rootCellClass : RootCellClass,
  cellClassesByParent : Map[CellClass, List[ChildCellClass]]
)
