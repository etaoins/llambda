package io.llambda.llvmir.debug

import io.llambda.llvmir._

sealed abstract class ModuleFlagMetadata extends MetadataNode {
  val flagId : Int
  val flagName : String
  val value : IrConstant

  lazy val operandOpts = List(
    Some(IntegerConstant(IntegerType(32), flagId)),
    Some(MetadataString.fromUtf8String(flagName)),
    Some(value)
  )

}

case class DebugInfoVersionMetadata(debugInfoVersion : Int) extends ModuleFlagMetadata {
  val flagId : Int = 1
  val flagName : String = "Debug Info Version"
  val value = IntegerConstant(IntegerType(32), debugInfoVersion)
}

case class DwarfVersionMetadata(dwarfVersion : Int) extends ModuleFlagMetadata {
  val flagId : Int = 2
  val flagName : String = "Dwarf Version"
  val value = IntegerConstant(IntegerType(32), dwarfVersion)
}
