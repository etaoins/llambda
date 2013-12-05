package llambda.valuetype

import llambda.{celltype => ct}

/** Value field of mutables */
object MutableField extends RecordField("value", IntrinsicCellType(ct.DatumCell))

/** Mutable values are implemented as single field records */
object MutableCellType extends RecordCellType("mutable", List(MutableField))
