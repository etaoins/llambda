package test.scala

import org.scalatest.FunSuite
import llambda._

class DefaultLibraryLoaderSuite extends FunSuite { 
  test("load nonexistant library") {
    val loader = new DefaultLibraryLoader

    intercept[LibraryNotFoundException] {
      loader.load(StringComponent("not") :: StringComponent("a") :: StringComponent("library") :: Nil)
    }
  }
  
  test("load dubious library names") {
    val loader = new DefaultLibraryLoader

    intercept[DubiousLibraryNameComponentException] {
      loader.load(StringComponent("foo/bar") :: Nil)
    }
    
    intercept[DubiousLibraryNameComponentException] {
      loader.load(StringComponent("bar\0baz") :: Nil)
    }
  }

  test("load scheme core") {
    val loader = new DefaultLibraryLoader
    val bindings = loader.loadSchemeCore

    assert(bindings.contains("set!"))
  }

  test("unmatched library name") {
    val loader = new DefaultLibraryLoader

    intercept[LibraryNameMismatchException] {
      loader.load(StringComponent("test") :: StringComponent("unmatchedname") :: Nil)
    }
  }

  test("load single expression library") {
    val loader = new DefaultLibraryLoader

    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)

    assert(loader.libraryExpressions.length === 1)
  }

  test("multiple loads don't introduce duplicate expressions") {
    val loader = new DefaultLibraryLoader

    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)
    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)

    assert(loader.libraryExpressions.length === 1)
  }
  
  test("multiple top-level data library") {
    val loader = new DefaultLibraryLoader

    intercept[BadSpecialFormException] {
      loader.load(StringComponent("test") :: StringComponent("multipledatum") :: Nil)
    }
  }
}
