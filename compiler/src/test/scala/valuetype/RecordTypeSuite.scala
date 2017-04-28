package io.llambda.compiler.valuetype


class RecordTypeSuite extends SchemeTypeSuite {
  test("record types definitely satisfy themselves") {
    assert(SatisfiesType(recordType1, recordType1) ===
      Some(true)
    )
  }

  test("record types definitely don't satisfy other record types") {
    assert(SatisfiesType(recordType1, recordType2) ===
      Some(false)
    )
  }

  test("record types definitely satisfy the record atom") {
    assert(SatisfiesType(recordAtomType, recordType1) ===
      Some(true)
    )
  }

  test("the record atom may satisfy a record type") {
    assert(SatisfiesType(recordType2, recordAtomType) ===
      None
    )
  }

  test("child record types satisfy parent types") {
    assert(SatisfiesType(recordType1, recordType1Child1) === Some(true))
  }

  test("parent record types may satisfy child record types") {
    assert(SatisfiesType(recordType1Child1, recordType1) === None)
  }

  test("sibling record types do not satisfy each other") {
    assert(SatisfiesType(recordType1Child1, recordType1Child2) === Some(false))
  }

  test("distinct record types intersected with each other is an empty union") {
    assertIntersection(recordType1, recordType2, EmptySchemeType)
  }

  test("union of record types intersected with the record type atom is the original union") {
    assertIntersection(UnionType(Set(recordType1, recordType2)), recordAtomType, UnionType(Set(recordType1, recordType2)))
  }
}
