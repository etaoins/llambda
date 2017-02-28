package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir.IrValue
import scala.language.implicitConversions


object Implicits {
  implicit def CollectableIrValue2IrValue(civ: CollectableIrValue): IrValue = civ.irValue
}
