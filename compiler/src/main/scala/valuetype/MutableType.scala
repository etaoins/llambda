package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}

/** Value field of mutables */
object MutableField extends RecordField("value", IntrinsicCellType(ct.DatumCell))

/** Mutable values are implemented as single field records */
object MutableType extends RecordType("mutable", List(MutableField))
