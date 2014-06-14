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

    assert(debugInfoMetadata.toIrWithType === 
      """metadata !{i32 1, metadata !"Debug Info Version", i32 6}"""
    )
  }
  
  test("dwarf version metadata") {
    val dwarfMetadata = DwarfVersionMetadata(
      dwarfVersion=3
    )

    assert(dwarfMetadata.toIrWithType === 
      """metadata !{i32 2, metadata !"Dwarf Version", i32 3}"""
    )
  }
}
