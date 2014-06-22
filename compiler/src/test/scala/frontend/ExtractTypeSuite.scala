package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

class ExtractTypeSuite extends FunSuite with testutil.ExprHelpers {
  val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  val nfiScope = new ImmutableScope(testutil.NfiExports(), Some(primitiveScope))
  
  test("define simple type aliases") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("(define-type <custom-type> <int32>)")(scope)

    assert(scope("<custom-type>") === BoundType(vt.Int32))

    intercept[UnboundVariableException] {
      bodyFor("(define-type <another-type> <doesnt-exist>)")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Not enough args
      bodyFor("(define-type <another-type>)")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Too many args
      bodyFor("(define-type <another-type> <int32> <unicode-char>)")(scope)
    }
  }

  test("defining union types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("(define-type <custom-type> (U <string-cell> <port-cell>))")(scope)

    assert(scope("<custom-type>") ===
      BoundType(vt.UnionType(Set(vt.PortType, vt.StringType)))
    )

    // Single type unions should degrade to that exact typer
    bodyFor("(define-type <single-type-union> (U <string-cell>))")(scope)
    assert(scope("<single-type-union>") === BoundType(vt.StringType))
    
    // Unions of unions should break down to their member types
    // Also we should ignore duplicate types (<empty-list-cell> in this case)
    bodyFor("(define-type <union-of-union> (U <list-element-cell> <empty-list-cell> <numeric-cell>))")(scope)
    assert(scope("<union-of-union>") ===
      BoundType(vt.UnionType(Set(
        vt.ExactIntegerType,
        vt.InexactRationalType,
        vt.EmptyListType,
        vt.PairType
      )))
    )

    intercept[UnboundVariableException] {
      // Type doesn't exist
      bodyFor("(define-type <another-type> (U <doesnt-exist>))")(scope)
    }
    
    intercept[MalformedExprException] {
      // Using a non-type constructor
      bodyFor("(define-type <another-type> (if <string-cell>))")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Using a native type
      bodyFor("(define-type <another-type> (U <int32>))")(scope)
    }
  }
}
