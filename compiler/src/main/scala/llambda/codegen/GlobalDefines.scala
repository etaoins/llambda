package llambda.codegen

import llambda.codegen.llvmir._
import llambda.{boxedtype => bt}

object GlobalDefines {
  val trueIrValue = GlobalVariable("lliby_true_value", PointerType(bt.BoxedBoolean.irType))
  val falseIrValue = GlobalVariable("lliby_false_value", PointerType(bt.BoxedBoolean.irType))
  val emptyListIrValue = GlobalVariable("lliby_empty_list_value", PointerType(bt.BoxedEmptyList.irType))
  val unspecificIrValue = GlobalVariable("lliby_unspecific_value", PointerType(bt.BoxedUnspecific.irType))
}
