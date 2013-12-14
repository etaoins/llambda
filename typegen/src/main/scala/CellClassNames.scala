package io.llambda.typegen

case class CellClassNames(definitionName : String) {
  val llvmName = definitionName.charAt(0).toLower + definitionName.drop(1)
  val cppName = definitionName + "Cell"

  // Insert a _ before any capital letter not at the beginning of a string and
  // lowercase the result
  val underscoreName = definitionName.replaceAll("""(?<!^)([A-Z]+)""", """_$1""").toLowerCase
}

