package io.llambda.typegen

case class ProcessedTypes(
  nextMetadataIndex : Int,
  fieldTypes : Map[String, FieldType],
  cellClasses : Map[String, CellClass],
  rootCellClass : RootCellClass,
  taggedCellClassesByParent : Map[CellClass, List[TaggedCellClass]]
)
