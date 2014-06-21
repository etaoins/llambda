package io.llambda.typegen

import org.scalatest.FunSuite

class CellNamesSuite extends FunSuite {
  test("one word name") {
    val names = CellClassNames("Datum")

    assert(names.definitionName === "Datum")
    assert(names.llvmName === "datum")
    assert(names.cppClassName === "DatumCell")
    assert(names.cppRefName === "DatumRef")
    assert(names.cppRefRangeName === "DatumRefRange")
    assert(names.scalaObjectName === "DatumCell")
    assert(names.scalaFieldsTraitName === "DatumFields")
    assert(names.schemeName === "<datum-cell>")
  }
  
  test("camel case name") {
    val names = CellClassNames("StringLike")

    assert(names.definitionName === "StringLike")
    assert(names.llvmName === "stringLike")
    assert(names.cppClassName === "StringLikeCell")
    assert(names.cppRefName === "StringLikeRef")
    assert(names.cppRefRangeName === "StringLikeRefRange")
    assert(names.scalaObjectName === "StringLikeCell")
    assert(names.scalaFieldsTraitName === "StringLikeFields")
    assert(names.schemeName === "<string-like-cell>")
  }
}
