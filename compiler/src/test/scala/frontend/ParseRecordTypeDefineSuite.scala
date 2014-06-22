package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside}
import llambda.compiler.{valuetype => vt}
import llambda.compiler._

class ParseRecordTypeDefineSuite extends FunSuite with testutil.ExprHelpers with Inside {
  // We need NFI for types and SchemePrimitives for (define-record-type)
  val baseScope = {
    val allBindings = testutil.NfiExports() ++ Primitives.bindings
    new Scope(collection.mutable.Map(allBindings.toSeq : _*))
  }

  private def storageLocFor(scope : Scope, name : String) : StorageLocation = 
    scope(name) match {
      case storageLoc : StorageLocation => storageLoc
      case other => fail("Expected storage location, got " + other.toString)
    }

  implicit class RecordTypeHelpers(recordType : vt.RecordType) {
    def fieldForSourceName(sourceName : String) : vt.RecordField = 
      recordType.fields.find(_.sourceName == sourceName) getOrElse {
        fail("Unable to find field named " + sourceName)
      }
  }

  test("define-record-type with no arguments fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type)""")(scope)
    }
  }
  
  test("define-record-type with 1 argument fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>)""")(scope)
    }
  }
  
  test("define-record-type with 2 argument fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type> 
                 (new-type))""")(scope)
    }
  }
  
  test("fieldless record type") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <new-type>
                           (new-type)
                           new-type?)""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordType : vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")

        assert(recordType.sourceName === "<new-type>")
        assert(recordType.fields === Nil)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set( 
              (consLoc -> et.RecordTypeConstructor(recordType, Nil)),
              (predLoc -> et.TypePredicate(recordType))
            ))
        }
    }
  }

  test("single read-only untyped field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <new-type>
                           (new-type const-datum)
                           new-type?
                           (const-datum new-type-const-datum))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordType : vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constDatumAccessorLoc = storageLocFor(scope, "new-type-const-datum")
        
        assert(recordType.sourceName === "<new-type>")

        val constDatumField = recordType.fieldForSourceName("const-datum")
        // No type defaults to <datum-cell>, the most permissive type
        assert(constDatumField.fieldType === vt.AnySchemeType)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set( 
              (consLoc -> et.RecordTypeConstructor(recordType, List(constDatumField))),
              (predLoc -> et.TypePredicate(recordType)),
              (constDatumAccessorLoc -> et.RecordTypeAccessor(recordType, constDatumField))
            ))
        }
    }
  }
  
  test("specifying types in untyped form fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int)
                 new-type?
                 ((const-int : <exact-integer-cell>) new-type-const-int))""")(scope)
    }
  }
  
  test("single read-only typed field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type: <new-type>
                           (new-type const-int)
                           new-type?
                           ((const-int : <exact-integer-cell>) new-type-const-int))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordType : vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constIntAccessorLoc = storageLocFor(scope, "new-type-const-int")
        
        assert(recordType.sourceName === "<new-type>")

        val constIntField = recordType.fieldForSourceName("const-int")
        assert(constIntField.fieldType === vt.Int64)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set( 
              (consLoc -> et.RecordTypeConstructor(recordType, List(constIntField))),
              (predLoc -> et.TypePredicate(recordType)),
              (constIntAccessorLoc -> et.RecordTypeAccessor(recordType, constIntField))
            ))
        }
    }
  }
  
  test("read-only and mutable field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type: <new-type>
                           (new-type mutable-int const-datum)
                           new-type?
                           (const-datum new-type-const-datum)
                           ((mutable-int : <exact-integer-cell>) new-type-mutable-int set-new-type-mutable-int!))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordType : vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constAccessorLoc = storageLocFor(scope, "new-type-const-datum")
        val mutableAccessorLoc = storageLocFor(scope, "new-type-mutable-int")
        val mutableMutatorLoc = storageLocFor(scope, "set-new-type-mutable-int!")
        
        assert(recordType.sourceName === "<new-type>")
        
        val constDatumField = recordType.fieldForSourceName("const-datum")
        val mutableIntField = recordType.fieldForSourceName("mutable-int")
          
        assert(constDatumField.fieldType === vt.AnySchemeType)
        // <exact-integer-cell> should be implicitly converted to int64 for storage
        assert(mutableIntField.fieldType === vt.Int64)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              (consLoc -> et.RecordTypeConstructor(recordType, List(mutableIntField, constDatumField))),
              (predLoc -> et.TypePredicate(recordType)),
              (constAccessorLoc -> et.RecordTypeAccessor(recordType, constDatumField)),
              (mutableAccessorLoc -> et.RecordTypeAccessor(recordType, mutableIntField)),
              (mutableMutatorLoc -> et.RecordTypeMutator(recordType, mutableIntField))
            ))
        }
    }
  }
  
  test("nested record types") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val innerExprs = bodyFor("""(define-record-type <inner-type>
                                (inner-type)
                                inner-type?)""")(scope)

    inside(scope("<inner-type>")) {
      case BoundType(innerType : vt.RecordType) =>
        val innerConsLoc = storageLocFor(scope, "inner-type")
        val innerPredLoc = storageLocFor(scope, "inner-type?")
        
        assert(innerType.sourceName === "<inner-type>")
        assert(innerType.fields === Nil)

        inside(innerExprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set( 
              (innerConsLoc -> et.RecordTypeConstructor(innerType, List())),
              (innerPredLoc -> et.TypePredicate(innerType))
            ))
        }
    
        val outerExprs = bodyFor("""(define-record-type: <outer-type>
                                    (outer-type inner-field)
                                    outer-type?
                                    ((inner-field : <inner-type>) outer-type-inner-field))""")(scope)

        inside(scope("<outer-type>")) {
          case BoundType(outerType : vt.RecordType) =>
            val outerConsLoc = storageLocFor(scope, "outer-type")
            val outerPredLoc = storageLocFor(scope, "outer-type?")
            val innerFieldAccessorLoc = storageLocFor(scope, "outer-type-inner-field")

            val innerField = outerType.fieldForSourceName("inner-field")
            assert(innerField.fieldType === innerType)

            inside(outerExprs) {
              case et.TopLevelDefine(bindings) :: Nil =>
                assert(bindings.toSet === Set( 
                  (outerConsLoc -> et.RecordTypeConstructor(outerType, List(innerField))),
                  (outerPredLoc -> et.TypePredicate(outerType)),
                  (innerFieldAccessorLoc -> et.RecordTypeAccessor(outerType, innerField))
                ))
            }
        }
    }
  }
  
  test("duplicate field name fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int)
                 new-type?
                 ((const-int : <int64>) new-type-const-int)
                 ((const-int : <exact-integer-cell>) new-type-mutable-int set-new-type-mutable-int!))""")(scope)
    }
  }
  
  test("duplicate initializer fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int const-int)
                 new-type?
                 ((const-int : <int64>) new-type-const-int))""")(scope)
    }
  }
  
  test("invalid type reference fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[UnboundVariableException] {
      bodyFor("""(define-record-type: <new-type>
                 (new-type const-int)
                 new-type?
                 ((const-int : <not-a-type>) new-type-const-int))""")(scope)
    }
  }
  
  test("initializer for non-field fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int not-a-field)
                 new-type?
                 ((const-int : <int64>) new-type-const-int))""")(scope)
    }
  }
  
  test("lack of initializer for non-defaultable type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      // Everything but datum-cell and unit-cell have no default value
      bodyFor("""(define-record-type <new-type>
                 (new-type)
                 new-type?
                 ((const-int : <int64>) new-type-const-int))""")(scope)
    }
  }
  
  test("duplicate procedure name fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int)
                 new-type?
                 ((const-int : <int64>) new-type-const-int new-type-const-int))""")(scope)
    }
  }
}
