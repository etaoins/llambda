package io.llambda.compiler.valuetype


class ExternalRecordTypeSuite extends SchemeTypeSuite {
  val externalType1 = new ExternalRecordType(Some("<external1>"), None)
  val externalType2 = new ExternalRecordType(Some("<external2>"), None)

  test("external types definitely satisfy themselves") {
    assert(SatisfiesType(externalType1, externalType1) ===
      Some(true)
    )
  }

  test("external types definitely don't satisfy other external types") {
    assert(SatisfiesType(externalType1, externalType2) ===
      Some(false)
    )
  }

  test("external types definitely don't satisfy other record types") {
    assert(SatisfiesType(externalType1, recordType1) ===
      Some(false)
    )
  }

  test("external types definitely satisfy the record atom") {
    assert(SatisfiesType(recordAtomType, externalType1) ===
      Some(true)
    )
  }

  test("the record atom may satisfy an external type") {
    assert(SatisfiesType(externalType2, recordAtomType) ===
      None
    )
  }

  test("distinct external types intersected with each other is an empty union") {
    assertIntersection(externalType1, externalType2, EmptySchemeType)
  }

  test("union of external types intersected with the external type atom is the original union") {
    assertIntersection(UnionType(Set(externalType1, externalType2)), recordAtomType, UnionType(Set(externalType1, externalType2)))
  }
}
