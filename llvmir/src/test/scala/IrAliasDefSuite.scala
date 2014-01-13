package io.llambda.llvmir

import org.scalatest.FunSuite

class IrAliaseDefSuite extends FunSuite {
  val testGlobal = GlobalVariable("testGlobal", PointerType(IntegerType(32)))

  test("trivial alias") {
    val aliasDef = IrAliasDef(
      name="test",
      aliasee=testGlobal
    )

    assert(aliasDef.variable.irType === PointerType(IntegerType(32)))
    assert(aliasDef.toIr === "@test = alias i32* @testGlobal")
  }
  
  test("externally available alias") {
    val aliasDef = IrAliasDef(
      name="test",
      aliasee=testGlobal,
      linkage=Linkage.ExternallyAvailable
    )

    assert(aliasDef.toIr === "@test = alias externally_available i32* @testGlobal")
  }
  
  test("hidden alias") {
    val aliasDef = IrAliasDef(
      name="test",
      aliasee=testGlobal,
      visibility=Visibility.Hidden
    )

    assert(aliasDef.toIr === "@test = alias hidden i32* @testGlobal")
  }
  
  test("christmas tree alias") {
    val aliasDef = IrAliasDef(
      name="test",
      aliasee=testGlobal,
      visibility=Visibility.Protected,
      linkage=Linkage.Private
    )

    assert(aliasDef.toIr === "@test = alias private protected i32* @testGlobal")
  }
}


