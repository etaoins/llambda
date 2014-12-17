package io.llambda.llvmir

case class IrGlobalVariableDef(
    name : String,
    initializer : IrConstant,
    linkage : Linkage = Linkage.Default,
    visibility : Visibility = Visibility.Default,
    unnamedAddr : Boolean = false,
    constant : Boolean = false) extends Irable with IrNamedGlobal {
  def variable : GlobalVariable =
    GlobalVariable(name, PointerType(initializer.irType))

  def toIr : String = {
    val defParts = List(linkage, visibility).flatMap(_.toOptIr) ++
                    (unnamedAddr match {
                      case true => List("unnamed_addr")
                      case false => Nil
                    }) ++
                    (constant match {
                      case true => List("constant")
                      case false => Nil
                    }) ++
                    List(initializer.toIrWithType )
                  
    s"${variable.toIr} = " + defParts.mkString(" ")
  }
}

