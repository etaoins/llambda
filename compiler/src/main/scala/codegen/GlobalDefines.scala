package io.llambda.compiler.codegen
import io.llambda

import llambda.compiler.codegen.llvmir._
import llambda.compiler.{celltype => ct}

object GlobalDefines {
  val trueIrValue = GlobalVariable("lliby_true_value", PointerType(ct.BooleanCell.irType))
  val falseIrValue = GlobalVariable("lliby_false_value", PointerType(ct.BooleanCell.irType))
  val emptyListIrValue = GlobalVariable("lliby_empty_list_value", PointerType(ct.EmptyListCell.irType))
  val unspecificIrValue = GlobalVariable("lliby_unspecific_value", PointerType(ct.UnspecificCell.irType))
}
