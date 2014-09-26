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

  test("defining specific vector types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    bodyFor("(define-type <mixed-vector> (Vector <string> <symbol>))")(scope)
    assert(scope("<mixed-vector>") === BoundType(
      vt.SpecificVectorType(Vector[vt.SchemeTypeRef](
        vt.StringType,
        vt.SymbolType
      ))
    ))
    
    bodyFor("(define-type <empty-vector> (Vector))")(scope)
    assert(scope("<empty-vector>") === BoundType(
      vt.SpecificVectorType(Vector[vt.SchemeTypeRef]())
    ))

  }

  test("definiting uniform vector types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    
    bodyFor("(define-type <string-vector> (Vectorof <string>))")(scope)
    assert(scope("<string-vector>") === BoundType(vt.UniformVectorType(vt.StringType)))
    
    intercept[BadSpecialFormException] {
      // Too many arguments
      bodyFor("(define-type <too-many-args> (Vectorof <string> <string> <string>))")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Not enough arguments
      bodyFor("(define-type <insufficient-args> (Vectorof))")(scope)
    }
  }
  
  test("defining recursive vector types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    
    bodyFor("(define-type <string-vector-tree> (Rec VT (U <string> (Vectorof VT))))")(scope)
    assert(scope("<string-vector-tree>") === BoundType(
      vt.UnionType(Set(
        vt.StringType,
        vt.UniformVectorType(vt.RecursiveSchemeTypeRef(1))
      ))
    ))
  }
  
  test("defining homogeneous list types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val stringListType = vt.UniformProperListType(vt.StringType)

    bodyFor("(define-type <string-list> (Listof <string>))")(scope)
    assert(scope("<string-list>") === BoundType(stringListType))
    
    bodyFor("(define-type <string-list-list> (Listof (Listof <string>)))")(scope)
    assert(scope("<string-list-list>") === BoundType(vt.UniformProperListType(stringListType)))

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

  test("defining procedure types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    bodyFor("(define-type <string-proc> (-> <string>))")(scope)
    assert(scope("<string-proc>") === BoundType(
      vt.ProcedureType(
        fixedArgTypes=Nil,
        restArgMemberTypeOpt=None,
        returnType=vt.ReturnType.SingleValue(vt.StringType)
      )
    ))

    bodyFor("(define-type <values-to-string-proc> (-> <port> <symbol> <string>))")(scope)
    assert(scope("<values-to-string-proc>") === BoundType(
      vt.ProcedureType(
        fixedArgTypes=List(vt.PortType, vt.SymbolType),
        restArgMemberTypeOpt=None,
        returnType=vt.ReturnType.SingleValue(vt.StringType)
      )
    ))
    
    bodyFor("(define-type <symbol-to-values-proc> (-> <symbol> (Values <exact-integer> <flonum>)))")(scope)
    assert(scope("<symbol-to-values-proc>") === BoundType(
      vt.ProcedureType(
        fixedArgTypes=List(vt.SymbolType),
        restArgMemberTypeOpt=None,
        returnType=vt.ReturnType.MultipleValues(
          vt.SpecificProperListType(List(vt.ExactIntegerType, vt.FlonumType))
        )
      )
    ))
    
    bodyFor("(define-type <values-with-rest-to-arbitrary-proc> (-> <port> <symbol> <pair> * *))")(scope)
    assert(scope("<values-with-rest-to-arbitrary-proc>") === BoundType(
      vt.ProcedureType(
        fixedArgTypes=List(vt.PortType, vt.SymbolType),
        restArgMemberTypeOpt=Some(vt.AnyPairType),
        returnType=vt.ReturnType.ArbitraryValues
      )
    ))
    
    intercept[BadSpecialFormException] {
      bodyFor("(define-type <insufficient-args> (->))")(scope)
    }
  }

  test("defining case procedure types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    
    bodyFor("(define-type <zero-case-proc> (case->))")(scope)
    assert(scope("<zero-case-proc>") === BoundType(vt.CaseProcedureType(Nil)))
    
    bodyFor("(define-type <one-case-proc> (case-> (-> <string>)))")(scope)
    assert(scope("<one-case-proc>") === BoundType(
      vt.CaseProcedureType(List(
        vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.StringType)
        )
      ))
    ))

    bodyFor("(define-type <two-case-proc> (case-> (-> <number>) (-> <string> <number>)))")(scope)
    assert(scope("<two-case-proc>") === BoundType(
      vt.CaseProcedureType(List(
        vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.NumberType)
        ),
        vt.ProcedureType(
          fixedArgTypes=List(vt.StringType),
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.NumberType)
        )
      ))
    ))
    
    bodyFor("(define-type <two-case-with-rest-proc> (case-> (-> <number>) (-> <string> * <number>)))")(scope)
    assert(scope("<two-case-with-rest-proc>") === BoundType(
      vt.CaseProcedureType(List(
        vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=None,
          returnType=vt.ReturnType.SingleValue(vt.NumberType)
        ),
        vt.ProcedureType(
          fixedArgTypes=Nil,
          restArgMemberTypeOpt=Some(vt.StringType),
          returnType=vt.ReturnType.SingleValue(vt.NumberType)
        )
      ))
    ))
 
    intercept[BadSpecialFormException] {
      bodyFor("(define-type <non-proc-case-fails> (case-> (-> <string>) <string>))")(scope)
    }
      
    intercept[BadSpecialFormException] {
      bodyFor("(define-type <same-arity-fails> (case-> (-> <string>) (-> <string>)))")(scope)
    }
      
    intercept[BadSpecialFormException] {
      bodyFor("(define-type <decreasing-arity-fails> (case-> (-> <number> <string>) (-> <string>)))")(scope)
    }
      
    intercept[BadSpecialFormException] {
      bodyFor("(define-type <after-rest-fails> (case-> (-> <symbol> * <string>) (-> <number> <string>)))")(scope)
    }
  }

  test("defining recursive types") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    val stringListType = vt.UniformProperListType(vt.StringType)
    
    bodyFor("(define-type <manual-string-list> (Rec PL (U <empty-list> (Pairof <string> PL))))")(scope)
    assert(scope("<manual-string-list>") === BoundType(stringListType))
    
    bodyFor("(define-type <implicit-recursive> (U <empty-list> (Pairof <string> <implicit-recursive>)))")(scope)
    assert(scope("<implicit-recursive>") === BoundType(stringListType))
    
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
      
    intercept[UnboundVariableException] {
      bodyFor("(define-type <cross-procedure-type> (Rec T (-> (Pairof T T) *)))")(scope)
    }
  }
  
  test("defining type constructors") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    // No operands (is this useful?)
    bodyFor("(define-type (Boolean) <boolean>)")(scope)
    bodyFor("(define-type <constructed-boolean> (Boolean))")(scope)
    
    assert(scope("<constructed-boolean>") === BoundType(vt.BooleanType))
    
    intercept[BadSpecialFormException] {
      // Too many operands
      bodyFor("(define-type <too-many-args> (Boolean <pair>))")(scope)
    }
    
    // Non-symbol as argument 
    intercept[BadSpecialFormException] {
      bodyFor("(define-type (NonSymbol 1) <boolean>)")(scope)
    }
    
    // Single operand
    bodyFor("(define-type (Option T) (U T <empty-list>))")(scope)
    
    bodyFor("(define-type <string-option> (Option <string>))")(scope)
    
    assert(scope("<string-option>") === BoundType(
      vt.UnionType(Set(
        vt.EmptyListType,
        vt.StringType
      ))
    ))

    intercept[BadSpecialFormException] {
      // Not enough operands
      bodyFor("(define-type <insufficient-args> (Option))")(scope)
    }
    
    intercept[BadSpecialFormException] {
      // Too many operands
      bodyFor("(define-type <too-many-args> (Option <pair> <string>))")(scope)
    }

    // Multiple operands and recursive types
    bodyFor("(define-type (ListWithTerminator M T) (Rec L (U T (Pairof M L))))")(scope)

    bodyFor("(define-type <string-unit-tlist> (ListWithTerminator <string> <unit>))")(scope)
    assert(scope("<string-unit-tlist>") === BoundType(
      vt.UnionType(Set(
        vt.UnitType,
        vt.SpecificPairType(
          vt.StringType,
          vt.RecursiveSchemeTypeRef(1)
        )
      ))
    ))
    
    // This is identical to a proper list
    bodyFor("(define-type <port-list> (ListWithTerminator <port> <empty-list>))")(scope)
    assert(scope("<port-list>") === BoundType(vt.UniformProperListType(vt.PortType)))

    // Nested type constructors
    bodyFor("(define-type <nested-type> (ListWithTerminator (Option <symbol>) <boolean>))")(scope)
    assert(scope("<nested-type>") === BoundType(
      vt.UnionType(Set(
        vt.BooleanType,
        vt.SpecificPairType(
          vt.UnionType(Set(
            vt.EmptyListType,
            vt.SymbolType
          )),
          vt.RecursiveSchemeTypeRef(1)
        )
      ))
    ))
  }
}
