package io.llambda.llvmir.debug
import io.llambda

import org.scalatest.FunSuite

import llambda.llvmir._

class FileContextMetadataSuite extends FunSuite {
  test("file context metadata") {
    val fakeSourcePath = NumberedMetadata(1)
    val fileContextNode = FileContextMetadata(fakeSourcePath)

    assert(fileContextNode.toIr === "!{i32 786473, !1}")
  }
}
