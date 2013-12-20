package io.llambda.typegen

case class CellClassNames(definitionName : String) {
  lazy val llvmName = definitionName.charAt(0).toLower + definitionName.drop(1)
  lazy val cppClassName = definitionName + "Cell"
  lazy val scalaObjectName = cppClassName
  lazy val scalaFieldsTraitName = definitionName + "Fields"

  // Insert a _ before any capital letter not at the beginning of a string and
  // lowercase the result
  lazy val underscoreName = definitionName.replaceAll("""(?<!^)([A-Z]+)""", """_$1""").toLowerCase
  
  lazy val schemeName = "<" + definitionName.replaceAll("""(?<!^)([A-Z]+)""", """-$1""").toLowerCase + "-cell>"
}

