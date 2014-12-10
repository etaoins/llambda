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
    def fieldForName(name : String) : vt.RecordField = 
      recordType.fields.find(_.name == name) getOrElse {
        fail("Unable to find field named " + name)
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
      case BoundType(recordInstance : vt.RecordTypeInstance) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val recordType = recordInstance.recordType

        assert(recordInstance.typeVars.values.isEmpty)
        assert(recordType.sourceName === "<new-type>")
        assert(recordType.fields === Nil)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(consLoc, et.RecordConstructor(recordType, Nil)),
              et.SingleBinding(predLoc, et.TypePredicate(recordInstance))
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
      case BoundType(recordInstance : vt.RecordTypeInstance) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constDatumAccessorLoc = storageLocFor(scope, "new-type-const-datum")
        val recordType = recordInstance.recordType

        assert(recordInstance.typeVars.values.isEmpty)
        assert(recordType.sourceName === "<new-type>")

        val constDatumField = recordType.fieldForName("const-datum")
        // No type defaults to <any>, the most permissive type
        assert(recordInstance.schemeTypeForField(constDatumField) === vt.AnySchemeType)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(consLoc, et.RecordConstructor(recordType, List(constDatumField))),
              et.SingleBinding(predLoc, et.TypePredicate(recordInstance)),
              et.SingleBinding(constDatumAccessorLoc, et.RecordAccessor(recordType, constDatumField))
            ))
        }
    }
  }

  test("single read-only typed field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <new-type>
                           (new-type const-int)
                           new-type?
                           ((const-int : <exact-integer>) new-type-const-int))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordInstance : vt.RecordTypeInstance) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constIntAccessorLoc = storageLocFor(scope, "new-type-const-int")
        val recordType = recordInstance.recordType

        assert(recordInstance.typeVars.values.isEmpty)
        assert(recordType.sourceName === "<new-type>")

        val constIntField = recordType.fieldForName("const-int")
        assert(recordInstance.schemeTypeForField(constIntField) === vt.ExactIntegerType)
        assert(recordInstance.recordType.storageTypeForField(constIntField) === vt.Int64)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(consLoc, et.RecordConstructor(recordType, List(constIntField))),
              et.SingleBinding(predLoc, et.TypePredicate(recordInstance)),
              et.SingleBinding(constIntAccessorLoc, et.RecordAccessor(recordType, constIntField))
            ))
        }
    }
  }
  
  test("field with unstable type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type unstable-list)
                 new-type?
                 ((unstable-list : (Listof <any>)) new-type-unstable-list))""")(scope, dialect.R7RS)
    }
  }
  
  test("read-only and mutable field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <new-type>
                           (new-type mutable-int const-datum)
                           new-type?
                           (const-datum new-type-const-datum)
                           ((mutable-int : <exact-integer>) new-type-mutable-int set-new-type-mutable-int!))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordInstance : vt.RecordTypeInstance) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constAccessorLoc = storageLocFor(scope, "new-type-const-datum")
        val mutableAccessorLoc = storageLocFor(scope, "new-type-mutable-int")
        val mutableMutatorLoc = storageLocFor(scope, "set-new-type-mutable-int!")
        val recordType = recordInstance.recordType

        assert(recordInstance.typeVars.values.isEmpty)
        assert(recordType.sourceName === "<new-type>")

        val constDatumField = recordType.fieldForName("const-datum")
        val mutableIntField = recordType.fieldForName("mutable-int")

        assert(recordInstance.schemeTypeForField(constDatumField) === vt.AnySchemeType)
        assert(recordInstance.schemeTypeForField(mutableIntField) === vt.ExactIntegerType)

        // <exact-integer> should be implicitly converted to int64 for storage
        assert(recordInstance.recordType.storageTypeForField(mutableIntField) === vt.Int64)

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(consLoc, et.RecordConstructor(recordType, List(mutableIntField, constDatumField))),
              et.SingleBinding(predLoc, et.TypePredicate(recordInstance)),
              et.SingleBinding(constAccessorLoc, et.RecordAccessor(recordType, constDatumField)),
              et.SingleBinding(mutableAccessorLoc, et.RecordAccessor(recordType, mutableIntField)),
              et.SingleBinding(mutableMutatorLoc, et.RecordMutator(recordType, mutableIntField))
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
      case BoundType(innerInstance : vt.RecordTypeInstance) =>
        val innerConsLoc = storageLocFor(scope, "inner-type")
        val innerPredLoc = storageLocFor(scope, "inner-type?")
        val innerType = innerInstance.recordType

        assert(innerInstance.typeVars.values.isEmpty)
        assert(innerType.sourceName === "<inner-type>")
        assert(innerType.fields === Nil)

        inside(innerExprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(innerConsLoc, et.RecordConstructor(innerType, List())),
              et.SingleBinding(innerPredLoc, et.TypePredicate(innerInstance))
            ))
        }
    
        val outerExprs = bodyFor("""(define-record-type <outer-type>
                                    (outer-type inner-field)
                                    outer-type?
                                    ((inner-field : <inner-type>) outer-type-inner-field))""")(scope)

        inside(scope("<outer-type>")) {
          case BoundType(outerInstance : vt.RecordTypeInstance) =>
            val outerConsLoc = storageLocFor(scope, "outer-type")
            val outerPredLoc = storageLocFor(scope, "outer-type?")
            val innerFieldAccessorLoc = storageLocFor(scope, "outer-type-inner-field")
            val outerType = outerInstance.recordType

            val innerField = outerType.fieldForName("inner-field")
            assert(outerInstance.schemeTypeForField(innerField) === innerInstance)

            inside(outerExprs) {
              case et.TopLevelDefine(bindings) :: Nil =>
                assert(bindings.toSet === Set(
                  et.SingleBinding(outerConsLoc, et.RecordConstructor(outerType, List(innerField))),
                  et.SingleBinding(outerPredLoc, et.TypePredicate(outerInstance)),
                  et.SingleBinding(innerFieldAccessorLoc, et.RecordAccessor(outerType, innerField))
                ))
            }
        }
    }
  }

  test("inherting record types") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val parentExprs = bodyFor("""(define-record-type <parent>
                                   (parent const-int)
                                   parent?
                                   ((const-int : <exact-integer>) parent-const-int))""")(scope)

    val childExprs = bodyFor("""(define-record-type <child> <parent>
                                  (child const-int const-flonum)
                                  child?
                                  ((const-flonum : <flonum>) child-const-flonum))""")(scope)

    inside((scope("<parent>"), scope("<child>"))) {
      case (BoundType(parentInstance : vt.RecordTypeInstance), BoundType(childInstance : vt.RecordTypeInstance)) =>
        val parentConsLoc = storageLocFor(scope, "parent")
        val parentPredLoc = storageLocFor(scope, "parent?")
        val constIntAccessorLoc = storageLocFor(scope, "parent-const-int")
        val parentType = parentInstance.recordType

        assert(parentInstance.typeVars.values.isEmpty)
        assert(parentType.sourceName === "<parent>")
        assert(parentType.parentRecordOpt === None)

        val constIntField = parentType.fieldForName("const-int")
        assert(parentInstance.schemeTypeForField(constIntField) === vt.ExactIntegerType)

        inside(parentExprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(parentConsLoc, et.RecordConstructor(parentType, List(constIntField))),
              et.SingleBinding(parentPredLoc, et.TypePredicate(parentInstance)),
              et.SingleBinding(constIntAccessorLoc, et.RecordAccessor(parentType, constIntField))
            ))
        }

        val childConsLoc = storageLocFor(scope, "child")
        val childPredLoc = storageLocFor(scope, "child?")
        val constFlonumAccessorLoc = storageLocFor(scope, "child-const-flonum")
        val childType = childInstance.recordType

        assert(childInstance.typeVars.values.isEmpty)
        assert(childType.sourceName === "<child>")
        assert(childType.parentRecordOpt === Some(parentInstance))

        val constFlonumField = childType.fieldForName("const-flonum")
        assert(childInstance.schemeTypeForField(constFlonumField) === vt.FlonumType)

        inside(childExprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(childConsLoc, et.RecordConstructor(childType, List(constIntField, constFlonumField))),
              et.SingleBinding(childPredLoc, et.TypePredicate(childInstance)),
              et.SingleBinding(constFlonumAccessorLoc, et.RecordAccessor(childType, constFlonumField))
            ))
        }
    }
  }

  test("duplicate field name fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int)
                 new-type?
                 ((const-int : <native-int64>) new-type-const-int)
                 ((const-int : <exact-integer>) new-type-mutable-int set-new-type-mutable-int!))""")(scope)
    }
  }
  
  test("duplicate initializer fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int const-int)
                 new-type?
                 ((const-int : <native-int64>) new-type-const-int))""")(scope)
    }
  }
  
  test("invalid type reference fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[UnboundVariableException] {
      bodyFor("""(define-record-type <new-type>
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
                 ((const-int : <native-int64>) new-type-const-int))""")(scope)
    }
  }
  
  test("lack of initializer for non-defaultable type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      // Everything but datum-cell and unit-cell have no default value
      bodyFor("""(define-record-type <new-type>
                 (new-type)
                 new-type?
                 ((const-int : <native-int64>) new-type-const-int))""")(scope)
    }
  }
  
  test("duplicate procedure name fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int)
                 new-type?
                 ((const-int : <native-int64>) new-type-const-int new-type-const-int))""")(scope)
    }
  }

  test("inheriting non-record type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <child> <exact-integer>
                   (child const-int)
                   child?
                   ((const-int : <exact-integer>) child-const-int))""")(scope)
    }
  }

  test("inheriting record non-existent type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[UnboundVariableException] {
      bodyFor("""(define-record-type <child> <does-not-exist>>
                   (child const-int)
                   child?
                   ((const-int : <exact-integer>) child-const-int))""")(scope)
    }
  }

  test("duplicate field name in child record") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    bodyFor("""(define-record-type <parent>
                 (parent const-int)
                 parent?
                 ((const-int : <exact-integer>) parent-const-int))""")(scope)

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <child> <parent>
                   (child const-int)
                   child?
                   ((const-int : <exact-integer>) child-const-int))""")(scope)
    }
  }

  test("child not initialising non-defaultable parent strucutre field fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    bodyFor("""(define-record-type <parent>
                 (parent const-int)
                 parent?
                 ((const-int : <exact-integer>) parent-const-int))""")(scope)

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <child> <parent>
                   (child const-flonum)
                   child?
                   ((const-flonum : <flonum>) child-const-flonum))""")(scope)
    }
  }

  test("recursive record type") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <recursive-type>
                           (recursive-type parent)
                           recursive-type?
                           ([parent : (U <recursive-type> <unit>)] recursive-type-parent))""")(scope)

    inside(scope("<recursive-type>")) {
      case BoundType(recordInstance : vt.RecordTypeInstance) =>
        val consLoc = storageLocFor(scope, "recursive-type")
        val predLoc = storageLocFor(scope, "recursive-type?")
        val parentAccessorLoc = storageLocFor(scope, "recursive-type-parent")
        val recordType = recordInstance.recordType

        assert(recordInstance.typeVars.values.isEmpty)
        assert(recordType.sourceName === "<recursive-type>")

        val parentField = recordType.fieldForName("parent")
        assert(recordInstance.schemeTypeForField(parentField) === vt.UnionType(Set(recordInstance, vt.UnitType)))

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(consLoc, et.RecordConstructor(recordType, List(parentField))),
              et.SingleBinding(predLoc, et.TypePredicate(recordInstance)),
              et.SingleBinding(parentAccessorLoc, et.RecordAccessor(recordType, parentField))
            ))
        }
    }
  }

  test("polymorphic record type constructor") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type (PolyType [A : <number>] B)
                             (poly-type field-a field-b)
                             poly-type?
                             ([field-a : A] poly-type-field-a)
                             ([field-b : B] poly-type-field-b))""")(scope)

    inside(scope("PolyType")) {
      case RecordTypeConstructor(recordType : vt.RecordType) =>
        val consLoc = storageLocFor(scope, "poly-type")
        val predLoc = storageLocFor(scope, "poly-type?")
        val fieldAAccessorLoc = storageLocFor(scope, "poly-type-field-a")
        val fieldBAccessorLoc = storageLocFor(scope, "poly-type-field-b")

        val fieldA = recordType.fieldForName("field-a")
        val fieldB = recordType.fieldForName("field-b")

        inside(recordType.typeVars) {
          case List(typeVarA, typeVarB) =>
            assert(typeVarA.sourceName === "A")
            assert(typeVarA.upperBound === vt.NumberType)

            assert(typeVarB.sourceName === "B")
            assert(typeVarB.upperBound === vt.AnySchemeType)

            assert(recordType.sourceName === "PolyType")

            assert(fieldA.typeTemplate === typeVarA)
            assert(fieldB.typeTemplate === typeVarB)
        }

        inside(exprs) {
          case et.TopLevelDefine(bindings) :: Nil =>
            assert(bindings.toSet === Set(
              et.SingleBinding(consLoc, et.RecordConstructor(recordType, List(fieldA, fieldB))),
              et.SingleBinding(predLoc, et.TypePredicate(recordType.upperBound)),
              et.SingleBinding(fieldAAccessorLoc, et.RecordAccessor(recordType, fieldA)),
              et.SingleBinding(fieldBAccessorLoc, et.RecordAccessor(recordType, fieldB))
            ))
        }
    }
  }

  test("polymorphic mutable record field fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type (PolyType [A : <number>] B)
                   (poly-type field-a field-b)
                   poly-type?
                   ([field-a : A] poly-type-field-a set-poly-type-field-a!)
                   ([field-b : B] poly-type-field-b))""")(scope)
    }
  }
}
