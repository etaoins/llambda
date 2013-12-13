package io.llambda.typegen

import org.scalatest.FunSuite

class CellNamesSuite extends FunSuite {
  test("one word name") {
    val names = CellClassNames("Datum")

    assert(names.definitionName === "Datum")
    assert(names.llvmName === "datum")
    assert(names.cppName === "DatumCell")
  }
  
  test("camel case name") {
    val names = CellClassNames("StringLike")

    assert(names.definitionName === "StringLike")
    assert(names.llvmName === "stringLike")
    assert(names.cppName === "StringLikeCell")
  }
}
