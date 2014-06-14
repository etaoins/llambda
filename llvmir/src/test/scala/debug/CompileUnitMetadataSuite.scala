package io.llambda.llvmir.debug
import io.llambda

import java.io.File
import org.scalatest.FunSuite
import llambda.llvmir._

class CompileUnitMetadataSuite extends FunSuite {
  test("trivial compile unit") {
    val sourcePathMetadata = NumberedMetadata(1)

    val compileUnitNode = CompileUnitMetadata(
      sourcePath=sourcePathMetadata,
      dwarfLanguage=12,
      producer="llvmir unit test"
    )

    assert(compileUnitNode.toIrWithType === 
      """metadata !{i32 786449, metadata !1, i32 12, metadata !"llvmir unit test", i1 0, metadata !"", i32 0, metadata !{i32 0}, metadata !{i32 0}, metadata !{i32 0}, metadata !{i32 0}, metadata !{i32 0}, metadata !""}"""
    )
  }
  
  test("christmas tree compile unit") {
    val sourcePathMetadata = NumberedMetadata(1)
    
    val fakeEnum = NumberedMetadata(2)
    
    val fakeRetainedType1 = NumberedMetadata(3)
    val fakeRetainedType2 = NumberedMetadata(4)

    val fakeSubprogram1 = NumberedMetadata(5)
    
    val fakeGlobalVariable1 = NumberedMetadata(6)
    val fakeGlobalVariable2 = NumberedMetadata(7)

    val fakeImportedEntity = NumberedMetadata(8)

    val compileUnitNode = CompileUnitMetadata(
      sourcePath=sourcePathMetadata,
      dwarfLanguage=12,
      producer="llvmir unit test",
      optimised=true,
      flags="--some-flag",
      runtimeVersion=556,
      enums=List(fakeEnum),
      retainedTypes=List(fakeRetainedType1, fakeRetainedType2),
      subprograms=List(fakeSubprogram1),
      globalVariables=List(fakeGlobalVariable1, fakeGlobalVariable2),
      importedEntities=List(fakeImportedEntity),
      splitDebugFilename="lol.wut"
    )

    assert(compileUnitNode.toIrWithType === 
      """metadata !{i32 786449, metadata !1, i32 12, metadata !"llvmir unit test", i1 1, metadata !"--some-flag", i32 556, """ +
      """metadata !{metadata !2}, """ +
      """metadata !{metadata !3, metadata !4}, """ +
      """metadata !{metadata !5}, """ +
      """metadata !{metadata !6, metadata !7}, """ +
      """metadata !{metadata !8}, """ +
      """metadata !"lol.wut"}"""
    )
  }
}
