package io.llambda.llvmir

case class IrAliasDef(
  name: String,
  aliasee: GlobalVariable,
  linkage: Linkage = Linkage.Default,
  visibility: Visibility = Visibility.Default
) extends Irable with IrNamedGlobal {
  def variable: GlobalVariable =
    GlobalVariable(name, aliasee.irType)

  def toIr: String = {
    val defParts = List(linkage, visibility).flatMap(_.toOptIr) ++
                   List(aliasee.toIrWithType)

    s"@${name} = alias " + defParts.mkString(" ")
  }
}
