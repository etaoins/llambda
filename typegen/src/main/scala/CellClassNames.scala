package io.llambda.typegen

case class CellClassNames(definitionName : String) {
  val llvmName = definitionName.charAt(0).toLower + definitionName.drop(1)
  val cppName = definitionName + "Cell"
}

