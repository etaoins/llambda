package io.llambda.typegen

import io.llambda.llvmir

case class FieldType(
  signed : Option[Boolean],
  llvmType : llvmir.FirstClassType,
  cppTypeName : String,
  needsDefinition : Boolean
)
