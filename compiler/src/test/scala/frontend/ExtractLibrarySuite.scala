package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside}
import llambda.compiler._

class ExtractLibrarySuite extends FunSuite with Inside {
  val resourceBaseUrl = getClass.getClassLoader.getResource("")
  val includeBaseUrl = getClass.getClassLoader.getResource("includes/")
  
  val includePath = IncludePath(
    fileParentDir=Some(resourceBaseUrl),
    packageRootDir=Some(resourceBaseUrl)
  )

  val frontendConfig = FrontendConfig(
    includePath=includePath,
    featureIdentifiers=Set()
  )

  val exampleName = List(StringComponent("example"), StringComponent("lib"))

  def libraryFor(scheme : String) : Library = {
    val datum :: Nil = SchemeParser.parseStringAsData(scheme)
    ExtractLibrary(datum)(new LibraryLoader(platform.Posix64LE), frontendConfig)
  }
  
  test("empty datum is invalid") {
    intercept[BadSpecialFormException] {
      libraryFor("()")
    }
  }

  test("empty library declaration") {
    assert(libraryFor(
      """(define-library (example lib))"""
      ) === Library(exampleName, Map(), Nil))
  }

  test("exporting imported symbol") {
    assert(libraryFor(
      """(define-library (example lib)
           (import (llambda internal primitives))
           (export set! lambda))"""
      ) === Library(exampleName, Map("set!" -> PrimitiveExpressions.Set, "lambda" -> PrimitiveExpressions.Lambda), Nil)) 
  }
  
  test("exporting internal symbol") {
    inside(libraryFor(
      """(define-library (example lib)
           (import (llambda internal primitives))
           (export number5)
           (begin 
             (define number5 5)))"""
     )) {
       case Library(_, bindings, exprs) =>
          inside(bindings("number5")) {
            case storageLoc : StorageLocation =>
              assert(exprs === List(et.Bind(List(storageLoc -> et.Literal(ast.IntegerLiteral(5))))))
          }
     }
  }

  test("exporting unbound idenitifer failure") {
    intercept[UnboundVariableException] {
      libraryFor(
        """(define-library (example lib)
             (export number5))""")
    }
  }
  
  test("renaming exports") {
    assert(libraryFor(
      """(define-library (example lib)
           (import (llambda internal primitives))
           (export set! (rename lambda new-lambda)))"""
      ) === Library(exampleName, Map("set!" -> PrimitiveExpressions.Set, "new-lambda" -> PrimitiveExpressions.Lambda), Nil)) 
  }

  test("single body include") {
    inside(libraryFor(
        """(define-library (example lib)
             (import (llambda internal primitives))
             (include "includes/include1.scm"))"""
    )) {
       case Library(_, bindings, exprs) =>
         assert(exprs === 
            List(
              et.Literal(ast.StringLiteral("include1-line1")), 
              et.Literal(ast.StringLiteral("include1-line2"))
            )
         )
    }
  }

  test("multiple body include") {
    inside(libraryFor(
        """(define-library (example lib)
             (import (llambda internal primitives))
             (include "includes/include1.scm" "includes/include2.scm"))"""
    )) {
       case Library(_, bindings, exprs) =>
         assert(exprs === 
            List(
              et.Literal(ast.StringLiteral("include1-line1")), 
              et.Literal(ast.StringLiteral("include1-line2")),
              et.Literal(ast.StringLiteral("include2-line1")), 
              et.Literal(ast.StringLiteral("include2-line2"))
            )
         )
    }
  }

  test("body include with relative includes and scope") {
    inside(libraryFor(
        """(define-library (example lib)
             (import (llambda internal primitives))
             (export a b)
             (include "includes/definea.scm"))"""
    )) {
      case Library(_, bindings, exprs) =>
        inside(bindings("a")) {
          case storageLocA : StorageLocation =>
            inside(bindings("b")) {
              case storageLocB : StorageLocation =>
                assert(exprs === 
                  List(
                    et.Bind(List(storageLocA -> et.Literal(ast.IntegerLiteral(1)))),
                    et.Begin(List(
                      et.Bind(List(storageLocB -> et.Literal(ast.IntegerLiteral(2)))),
                      et.Begin(List(
                        et.VarRef(storageLocA),
                        et.VarRef(storageLocB)
                      ))
                    ))
                  )
                )
            }
        }
    }
  }
  
  test("library declaration include") {
    inside(libraryFor(
        """(define-library (example lib)
             (include-library-declarations "includes/libdecl1.scm" "includes/libdecl2.scm"))"""
    )) {
      case Library(_, bindings, exprs) =>
        inside(bindings("a")) {
          case storageLocA : StorageLocation =>
            inside(bindings("b")) {
              case storageLocB : StorageLocation =>
                assert(exprs === 
                  List(
                    et.Bind(List(storageLocA -> et.Literal(ast.IntegerLiteral(1)))),
                    et.Begin(List(
                      et.Bind(List(storageLocB -> et.Literal(ast.IntegerLiteral(2)))),
                      et.Begin(List(
                        et.VarRef(storageLocA),
                        et.VarRef(storageLocB)
                      ))
                    ))
                  )
                )
            }
        }
    }
  }
}
