package io.llambda.llvmir.debug

import io.llambda.llvmir._

object SubroutineTypeMetadata extends MetadataNode {
  // This is a simple DWARF type descriptor for a subroutine
  // I have no idea what any of these fields are supposed to mean - they're presumably null/unset values that would
  // only have meaning for more complex or user-defined types
  val operandOpts = List(
    Some(IntegerConstant(IntegerType(32), 786453)),
    Some(IntegerConstant(IntegerType(32), 0)),
    None,
    Some(MetadataString.fromUtf8String("")),
    Some(IntegerConstant(IntegerType(32), 0)),
    Some(IntegerConstant(IntegerType(64), 0)),
    Some(IntegerConstant(IntegerType(64), 0)),
    Some(IntegerConstant(IntegerType(64), 0)),
    Some(IntegerConstant(IntegerType(32), 0)),
    None,
    Some(UserDefinedMetadataNode(List(
      Some(IntegerConstant(IntegerType(32), 0))
    ))),
    Some(IntegerConstant(IntegerType(32), 0)),
    None,
    None,
    None
  )
}
