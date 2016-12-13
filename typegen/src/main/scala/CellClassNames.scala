package io.llambda.typegen

/** Alternative names for a [[CellClass]]
  *
  * @param definitionName  Name of the class defined in the definition file.
  *                        This is the same as [CellClass.name]
  **/
case class CellClassNames(definitionName: String) {
  /** Name for the user defined type in LLVM
    *
    * This is the definition name with the initial letter lowercased
    */
  lazy val llvmName = definitionName.charAt(0).toLower + definitionName.drop(1)

  /** Name for the C++ binding class
    *
    * This is the definition name with "Cell" appended
    */
  lazy val cppClassName = definitionName + "Cell"

  /** Name for the C++ StrongRef typedef */
  lazy val cppRefName = definitionName + "Ref"

  /** Name for the Scala object in the celltype package */
  lazy val scalaCellTypeName = cppClassName

  /** Name for the Scala fields trait in celltype package */
  lazy val scalaFieldsTraitName = definitionName + "Fields"

  /** Name for the value type in the valuetype package */
  lazy val scalaValueTypeName = definitionName + "Type"

  /** Name for the Scheme type */
  lazy val schemeName = "<" + definitionName.replaceAll("""(?<!^)([A-Z]+)""", """-$1""").toLowerCase + ">"
}

