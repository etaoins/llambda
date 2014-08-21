package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.Implicits._

class ExtractTypeSuite extends FunSuite with testutil.ExprHelpers {
  val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq : _*))
  val nfiScope = new ImmutableScope(testutil.NfiExports(), Some(primitiveScope))
  
  test("define simple type aliases") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("(define-type <custom-type> <native-int32>)")(scope)

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
      bodyFor("(define-type <another-type> <native-int32> <unicode-char>)")(scope)
    }
  }
  
  test("redefining type succeeds in R7RS") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("""
      (define-type <custom-type> <native-int32>)
      (define-type <custom-type> <native-int64>)
    """)(scope, dialect.R7RS)

    assert(scope("<custom-type>") === BoundType(vt.Int64))
  }
  
  test("redefining type fails in Llambda") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[DuplicateDefinitionException] {
      bodyFor("""
        (define-type <custom-type> <native-int32>)
        (define-type <custom-type> <native-int64>)
      """)(scope, dialect.Llambda)
    }
  }

  test("defining union types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("(define-type <custom-type> (U <string> <port>))")(scope)

    assert(scope("<custom-type>") ===
      BoundType(vt.UnionType(Set(vt.PortType, vt.StringType)))
    )

    // Single type unions should degrade to that exact typer
    bodyFor("(define-type <single-type-union> (U <string>))")(scope)
    assert(scope("<single-type-union>") === BoundType(vt.StringType))
    
    // Unions of unions should break down to their member types
    // Also we should ignore duplicate types (<empty-list> in this case)
    bodyFor("(define-type <union-of-union> (U <list-element> <empty-list> <number>))")(scope)
    assert(scope("<union-of-union>") ===
      BoundType(vt.UnionType(Set(
        vt.ExactIntegerType,
        vt.FlonumType,
        vt.EmptyListType,
        vt.AnyPairType
      )))
    )

    intercept[UnboundVariableException] {
      // Type doesn't exist
      bodyFor("(define-type <another-type> (U <doesnt-exist>))")(scope)
    }
    
    intercept[MalformedExprException] {
      // Using a non-type constructor
      bodyFor("(define-type <another-type> (if <string>))")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Using a native type
      bodyFor("(define-type <another-type> (U <native-int32>))")(scope)
    }
  }
 
  test("defining constant boolean types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    bodyFor("(define-type <custom-false> #f)")(scope)
    assert(scope("<custom-false>") === BoundType(vt.ConstantBooleanType(false)))

    bodyFor("(define-type <custom-true> #t)")(scope)
    assert(scope("<custom-true>") === BoundType(vt.ConstantBooleanType(true)))
    
    bodyFor("(define-type <custom-boolean> (U #f #t))")(scope)
    assert(scope("<custom-boolean>") === BoundType(vt.BooleanType))
  }
  
  test("defining pair types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    bodyFor("(define-type <string-pair> (Pairof <string> <string>))")(scope)
    assert(scope("<string-pair>") === BoundType(vt.PairType(vt.StringType, vt.StringType)))

    bodyFor("(define-type <nested-pair> (Pairof <symbol> (Pairof <string> <port>)))")(scope)
    assert(scope("<nested-pair>") === BoundType(
      vt.PairType(
        vt.SymbolType,
        vt.PairType(
          vt.StringType,
          vt.PortType
        )
      )
    ))
    
    intercept[BadSpecialFormException] {
      // Too many arguments
      bodyFor("(define-type <too-many-args> (Pairof <string> <string> <string>))")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Not enough arguments
      bodyFor("(define-type <insufficient-args> (Pairof <string>))")(scope)
    }
  }
  
  test("defining homogeneous list types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    bodyFor("(define-type <string-list> (Listof <string>))")(scope)
    assert(scope("<string-list>") === BoundType(vt.ProperListType(vt.StringType)))
    
    bodyFor("(define-type <string-list-list> (Listof (Listof <string>)))")(scope)
    assert(scope("<string-list-list>") === BoundType(vt.ProperListType(vt.ProperListType(vt.StringType))))

    intercept[BadSpecialFormException] {
      // Too many arguments
      bodyFor("(define-type <too-many-args> (Listof <string> <string>))")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Not enough arguments
      bodyFor("(define-type <insufficient-args> (Listof))")(scope)
    }
  }

  test("defining specified type lists") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    bodyFor("(define-type <other-empty-list> (List))")(scope)
    assert(scope("<other-empty-list>") === BoundType(vt.EmptyListType))
    
    bodyFor("(define-type <string-tuple> (List <string>))")(scope)
    assert(scope("<string-tuple>") === BoundType(
      vt.SpecificPairType(
        vt.StringType,
        vt.EmptyListType
      )
    ))
    
    bodyFor("(define-type <string-symbol-tuple> (List <string> <symbol>))")(scope)
    assert(scope("<string-symbol-tuple>") === BoundType(
      vt.SpecificPairType(
        vt.StringType,
        vt.SpecificPairType(
          vt.SymbolType,
          vt.EmptyListType
        )
      )
    ))
    
    bodyFor("(define-type <recursive-tuple> (Rec T (List <string> <symbol> T)))")(scope)
    assert(scope("<recursive-tuple>") === BoundType(
      vt.SpecificPairType(
        vt.StringType,
        vt.SpecificPairType(
          vt.SymbolType,
          vt.SpecificPairType(
            vt.RecursiveSchemeTypeRef(2),
            vt.EmptyListType
          )
        )
      )
    ))
  }

  test("defining recursive types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    
    bodyFor("(define-type <manual-string-list> (Rec PL (U <empty-list> (Pairof <string> PL))))")(scope)
    assert(scope("<manual-string-list>") === BoundType(vt.ProperListType(vt.StringType)))
    
    bodyFor("(define-type <implicit-recursive> (U <empty-list> (Pairof <string> <implicit-recursive>)))")(scope)
    assert(scope("<implicit-recursive>") === BoundType(vt.ProperListType(vt.StringType)))
    
    bodyFor("(define-type <string-tree> (Rec BT (U <string> (Pairof BT BT))))")(scope)
    assert(scope("<string-tree>") === BoundType(
      vt.UnionType(Set(
        vt.StringType,
        vt.SpecificPairType(
          vt.RecursiveSchemeTypeRef(1),
          vt.RecursiveSchemeTypeRef(1)
        )
      ))
    ))

    // This isn't a meaninful type; don't think about it too hard
    // This is just checking (Listof) can take a type reference
    bodyFor("(define-type <list-list> (Rec LL (Listof LL)))")(scope)
    assert(scope("<list-list>") === BoundType(
      vt.UnionType(Set(
        vt.EmptyListType,
        vt.SpecificPairType(
          vt.RecursiveSchemeTypeRef(1),
          vt.RecursiveSchemeTypeRef(1)
        )
      ))
    ))

    bodyFor("(define-type <list-of-pairs-to-list> (Rec W (Listof (Pairof <boolean> W))))")(scope)
    assert(scope("<list-of-pairs-to-list>") === BoundType(
      vt.UnionType(Set(
        vt.EmptyListType,
        vt.SpecificPairType(
          vt.SpecificPairType(
            vt.BooleanType,
            vt.RecursiveSchemeTypeRef(2)
          ),
          vt.RecursiveSchemeTypeRef(1)
        )
      ))
    ))
    
    intercept[BadSpecialFormException] {
      // Unions can't have recursive types
      bodyFor("(define-type <inside-union> (Rec UT (U <string> UT)))")(scope)
    }
      
    intercept[BadSpecialFormException] {
      bodyFor("(define-type <too-many-args> (Rec BT BT (U <string> (Pairof BT BT))))")(scope)
    }
      
    intercept[BadSpecialFormException] {
      bodyFor("(define-type <insufficient-args> (Rec (U <string> (Pairof BT BT))))")(scope)
    }
  }
}
