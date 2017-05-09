package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{celltype => ct}

object GlobalDefines {
  val trueIrValue = GlobalVariable("llcore_true_value", PointerType(ct.BooleanCell.irType))
  val falseIrValue = GlobalVariable("llcore_false_value", PointerType(ct.BooleanCell.irType))
  val emptyListIrValue = GlobalVariable("llcore_empty_list_value", PointerType(ct.EmptyListCell.irType))
  val unitIrValue = GlobalVariable("llcore_unit_value", PointerType(ct.UnitCell.irType))

  val emptyMetadataNode = NumberedMetadata(1L)
}
