package io.llambda.compiler.valuetype
import io.llambda

class HashMapTypeSuite extends SchemeTypeSuite {
  test("the any hash map type satisfies itself") {
    assert(SatisfiesType(AnyHashMapType, AnyHashMapType) === Some(true))
  }

  test("specific hash map type satisfies the any hash map type") {
    val specificHashMapType = HashMapType(SymbolType, StringType)
    assert(SatisfiesType(AnyHashMapType, specificHashMapType) === Some(true))
  }

  test("the any hash map type may satisfy a specific hash map type") {
    val specificHashMapType = HashMapType(SymbolType, StringType)
    assert(SatisfiesType(specificHashMapType, AnyHashMapType) === None)
  }

  test("the any hash map type is convermap to the unit type") {
    assert(ConvertibleToType(UnitType, AnyHashMapType) === Some(true))
  }

  test("completely compatible specific hash map types satisfy each other") {
    val superHashMapType = HashMapType(SymbolType, NumberType)
    val derivedHashMapType = HashMapType(SymbolType, IntegerType)

    assert(SatisfiesType(superHashMapType, derivedHashMapType) === Some(true))
  }

  test("completely incompatible specific hash map types do not satisfy each other") {
    val specificHashMapType1 = HashMapType(SymbolType, StringType)
    val specificHashMapType2 = HashMapType(StringType, SymbolType)

    assert(SatisfiesType(specificHashMapType1, specificHashMapType2) === Some(false))
  }

  test("partially compatible specific hash map types do not satisfy each other") {
    // car may satisfy, cdr does not satisfy
    val specificHashMapType1 = HashMapType(IntegerType, StringType)
    val specificHashMapType2 = HashMapType(NumberType, SymbolType)

    assert(SatisfiesType(specificHashMapType1, specificHashMapType2) === Some(false))
  }
}
