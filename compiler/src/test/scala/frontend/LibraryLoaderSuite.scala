package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite
import llambda.compiler._

class LibraryLoaderSuite extends FunSuite { 
  implicit val defaultIncludePath = IncludePath()

  test("load non-existant library") {
    val loader = new LibraryLoader(platform.Posix64)
    val nonExistantName = List(StringComponent("not"), StringComponent("a"), StringComponent("library"))

    intercept[LibraryNotFoundException] {
      loader.load(nonExistantName)
    }

    assert(loader.exists(nonExistantName) === false)
  }
  
  test("dubious library names") {
    val loader = new LibraryLoader(platform.Posix64)

    intercept[DubiousLibraryNameComponentException] {
      loader.load(StringComponent("foo/bar") :: Nil)
    }
    
    intercept[DubiousLibraryNameComponentException] {
      loader.exists(StringComponent("foo/bar") :: Nil)
    }
    
    intercept[DubiousLibraryNameComponentException] {
      loader.load(StringComponent("bar\0baz") :: Nil)
    }
    
    intercept[DubiousLibraryNameComponentException] {
      loader.exists(StringComponent("bar\0baz") :: Nil)
    }
  }

  test("load scheme base") {
    val loader = new LibraryLoader(platform.Posix64)
    val bindings = loader.loadSchemeBase

    assert(bindings.contains("set!"))
  }

  test("load llambda primitives") {
    val loader = new LibraryLoader(platform.Posix64)
    val bindings = loader.load(List("llambda", "internal", "primitives").map(StringComponent(_)))

    assert(bindings.contains("set!"))
  }
  test("llambda primitives exists") {
    val loader = new LibraryLoader(platform.Posix64)

    assert(loader.exists(List("llambda", "internal", "primitives").map(StringComponent(_))) === true)
  }
  
  test("load llambda nfi") {
    val loader = new LibraryLoader(platform.Posix64)
    val bindings = loader.load(StringComponent("llambda") :: StringComponent("nfi") :: Nil)

    assert(bindings.contains("native-function"))
  }

  test("unmatched library name fails") {
    val loader = new LibraryLoader(platform.Posix64)

    intercept[LibraryNameMismatchException] {
      loader.load(List(StringComponent("test"), StringComponent("unmatchedname")))
    }
  }
  
  test("unmatched library name exists") {
    val loader = new LibraryLoader(platform.Posix64)

    // This is testing we don't attempt to parse in exists()
    assert(loader.exists(List(StringComponent("test"), StringComponent("unmatchedname"))) === true)
  }


  test("load single expression library") {
    val loader = new LibraryLoader(platform.Posix64)

    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)

    assert(loader.libraryExpressions.length === 1)
  }
  
  test("single expression library exists") {
    val loader = new LibraryLoader(platform.Posix64)

    assert(loader.exists(List(StringComponent("test"), StringComponent("singleexpr"))) === true)
  }
  
  test("load single expression library with non-default include path") {
    val loader = new LibraryLoader(platform.Posix64)

    val includePath = IncludePath(
      userConfiguredPaths=getClass.getClassLoader.getResource("libraries/test/") :: Nil
    )

    // We should be able to load without the "text" prefix
    loader.load(StringComponent("pathedsingleexpr") :: Nil)(includePath)

    assert(loader.libraryExpressions.length === 1)
  }
  
  test("single expression library with non-default include path exists") {
    val loader = new LibraryLoader(platform.Posix64)

    val includePath = IncludePath(
      userConfiguredPaths=getClass.getClassLoader.getResource("libraries/test/") :: Nil
    )

    assert(loader.exists(List(StringComponent("pathedsingleexpr")))(includePath) === true)
  }

  test("multiple loads don't introduce duplicate expressions") {
    val loader = new LibraryLoader(platform.Posix64)

    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)
    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)

    assert(loader.libraryExpressions.length === 1)
  }
  
  test("multiple top-level data library") {
    val loader = new LibraryLoader(platform.Posix64)

    intercept[BadSpecialFormException] {
      loader.load(StringComponent("test") :: StringComponent("multipledatum") :: Nil)
    }
  }
}
