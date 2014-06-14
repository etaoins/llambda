package io.llambda.llvmir.debug

import java.io.File

import org.scalatest.FunSuite

class FilePathMetadataSuite extends FunSuite {
  test("trivial file path") {
    val filePathNode = FilePathMetadata("file.scm", "/home/example")

    assert(filePathNode.toIrWithType === """metadata !{metadata !"file.scm", metadata !"/home/example"}""")
  }
  
  test("trivial file path from Java File") {
    val filePathNode = FilePathMetadata.fromFile(new File("/home/example/file.scm"))

    assert(filePathNode.toIrWithType === """metadata !{metadata !"file.scm", metadata !"/home/example"}""")
  }
}
