package io.llambda.typegen

import io.llambda.llvmir

case class CellField(
  name : String,
  fieldType : FieldType,
  tbaaNode : llvmir.IrTbaaNode
)
