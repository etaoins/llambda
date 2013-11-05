package llambda.frontend

import org.scalatest.FunSuite
import llambda._

class LibraryLoaderSuite extends FunSuite { 
  implicit val defaultIncludePath = IncludePath()

  test("load nonexistant library") {
    val loader = new LibraryLoader

    intercept[LibraryNotFoundException] {
      loader.load(StringComponent("not") :: StringComponent("a") :: StringComponent("library") :: Nil)
    }
  }
  
  test("load dubious library names") {
    val loader = new LibraryLoader

    intercept[DubiousLibraryNameComponentException] {
      loader.load(StringComponent("foo/bar") :: Nil)
    }
    
    intercept[DubiousLibraryNameComponentException] {
      loader.load(StringComponent("bar\0baz") :: Nil)
    }
  }

  test("load scheme base") {
    val loader = new LibraryLoader
    val bindings = loader.loadSchemeBase

    assert(bindings.contains("set!"))
  }

  test("load llambda primitives") {
    val loader = new LibraryLoader
    val bindings = loader.load(StringComponent("llambda") :: StringComponent("primitives") :: Nil)

    assert(bindings.contains("set!"))
  }
  
  test("load llambda nfi") {
    val loader = new LibraryLoader
    val bindings = loader.load(StringComponent("llambda") :: StringComponent("nfi") :: Nil)

    assert(bindings.contains("native-function"))
  }

  test("load llambda internal") {
    val loader = new LibraryLoader
    val bindings = loader.load(StringComponent("llambda") :: StringComponent("internal") :: Nil)

    assert(bindings.contains("define-report-procedure"))
  }

  test("unmatched library name") {
    val loader = new LibraryLoader

    intercept[LibraryNameMismatchException] {
      loader.load(StringComponent("test") :: StringComponent("unmatchedname") :: Nil)
    }
  }

  test("load single expression library") {
    val loader = new LibraryLoader

    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)

    assert(loader.libraryExpressions.length === 1)
  }
  
  test("load single expression library with non-default include path") {
    val loader = new LibraryLoader

    val includePath = IncludePath(
      userConfiguredPaths=getClass.getClassLoader.getResource("libraries/test/") :: Nil
    )

    // We should be able to load without the "text" prefix
    loader.load(StringComponent("pathedsingleexpr") :: Nil)(includePath)

    assert(loader.libraryExpressions.length === 1)
  }

  test("multiple loads don't introduce duplicate expressions") {
    val loader = new LibraryLoader

    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)
    loader.load(StringComponent("test") :: StringComponent("singleexpr") :: Nil)

    assert(loader.libraryExpressions.length === 1)
  }
  
  test("multiple top-level data library") {
    val loader = new LibraryLoader

    intercept[BadSpecialFormException] {
      loader.load(StringComponent("test") :: StringComponent("multipledatum") :: Nil)
    }
  }
}
