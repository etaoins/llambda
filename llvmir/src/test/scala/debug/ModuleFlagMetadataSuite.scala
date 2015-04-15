package io.llambda.llvmir.debug
import io.llambda

import java.io.File
import org.scalatest.FunSuite
import llambda.llvmir._

class ModuleFlagMetadataSuite extends FunSuite {
  test("debug info version metadata") {
    val debugInfoMetadata = DebugInfoVersionMetadata(
      debugInfoVersion=6
    )

    assert(debugInfoMetadata.toIr ===
      """!{i32 1, !"Debug Info Version", i32 6}"""
    )
  }

  test("dwarf version metadata") {
    val dwarfMetadata = DwarfVersionMetadata(
      dwarfVersion=3
    )

    assert(dwarfMetadata.toIr ===
      """!{i32 2, !"Dwarf Version", i32 3}"""
    )
  }
}
