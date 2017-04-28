package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite,Inside}
import llambda.compiler.{valuetype => vt}
import llambda.compiler._

class ParseRecordTypeDefineSuite extends FunSuite with testutil.ExprHelpers with Inside {
  // We need NFI for types and SchemePrimitives for (define-record-type)
  val baseScope = {
    val allBindings = testutil.NfiExports() ++ Primitives.bindings
    new Scope(collection.mutable.Map(allBindings.toSeq: _*))
  }

  private def storageLocFor(scope: Scope, name: String): StorageLocation =
    scope(name) match {
      case storageLoc: StorageLocation => storageLoc
      case other => fail("Expected storage location, got " + other.toString)
    }

  implicit class RecordTypeHelpers(recordType: vt.RecordType) {
    def fieldForName(name: String): vt.RecordField =
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
      case BoundType(recordType: vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")

        assert(recordType.sourceName === "<new-type>")
        assert(recordType.fields === Nil)

        inside(consLoc) {
          case boundCons: BoundRecordConstructor =>
        }

        inside(exprs) {
          case List(
            et.TopLevelDefine(et.Binding(`consLoc`, et.RecordConstructor(`recordType`, Nil))),
            et.TopLevelDefine(et.Binding(`predLoc`, et.TypePredicate(`recordType`)))
          ) =>
        }
    }

    // The constructor binding should be immutable so it can be used for pattern matching unapplication
    intercept[BadSpecialFormException] {
      bodyFor("(set! new-type 4)")(scope)
    }
  }

  test("single read-only untyped field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <new-type>
                           (new-type const-datum)
                           new-type?
                           (const-datum new-type-const-datum))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordType: vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constDatumAccessorLoc = storageLocFor(scope, "new-type-const-datum")

        assert(recordType.sourceName === "<new-type>")

        inside(consLoc) {
          case boundCons: BoundRecordConstructor =>
        }

        val constDatumField = recordType.fieldForName("const-datum")
        // No type defaults to <any>, the most permissive type
        assert(recordType.typeForField(constDatumField) === vt.AnySchemeType)
        assert(constDatumField.mutable === false)

        inside(exprs) {
          case List(
            et.TopLevelDefine(
              et.Binding(`consLoc`, et.RecordConstructor(`recordType`, List(`constDatumField`)))
            ),
            et.TopLevelDefine(et.Binding(`predLoc`, et.TypePredicate(`recordType`))),
            et.TopLevelDefine(
              et.Binding(`constDatumAccessorLoc`, et.RecordAccessor(recordType, constDatumField))
            )
          ) =>
        }
    }
  }

  test("single read-only typed field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <new-type>
                           (new-type const-int)
                           new-type?
                           ([const-int : <integer>] new-type-const-int))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordType: vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constIntAccessorLoc = storageLocFor(scope, "new-type-const-int")

        assert(recordType.sourceName === "<new-type>")

        inside(consLoc) {
          case boundCons: BoundRecordConstructor =>
        }

        val constIntField = recordType.fieldForName("const-int")
        assert(recordType.typeForField(constIntField) === vt.Int64)
        assert(constIntField.mutable === false)

        inside(exprs) {
          case List(
            et.TopLevelDefine(et.Binding(`consLoc`, et.RecordConstructor(`recordType`, List(`constIntField`)))),
            et.TopLevelDefine(et.Binding(`predLoc`, et.TypePredicate(`recordType`))),
            et.TopLevelDefine(
              et.Binding(`constIntAccessorLoc`, et.RecordAccessor(`recordType`, `constIntField`))
            )
          ) =>
        }
    }
  }

  test("read-only and mutable field") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val exprs = bodyFor("""(define-record-type <new-type>
                           (new-type mutable-int const-datum)
                           new-type?
                           (const-datum new-type-const-datum)
                           ([mutable-int : <integer>] new-type-mutable-int set-new-type-mutable-int!))""")(scope)

    inside(scope("<new-type>")) {
      case BoundType(recordType: vt.RecordType) =>
        val consLoc = storageLocFor(scope, "new-type")
        val predLoc = storageLocFor(scope, "new-type?")
        val constAccessorLoc = storageLocFor(scope, "new-type-const-datum")
        val mutableAccessorLoc = storageLocFor(scope, "new-type-mutable-int")
        val mutableMutatorLoc = storageLocFor(scope, "set-new-type-mutable-int!")

        assert(recordType.sourceName === "<new-type>")

        inside(consLoc) {
          case boundCons: BoundRecordConstructor =>
        }

        val constDatumField = recordType.fieldForName("const-datum")
        assert(recordType.typeForField(constDatumField) === vt.AnySchemeType)
        assert(constDatumField.mutable === false)

        val mutableIntField = recordType.fieldForName("mutable-int")
        // <integer> should be implicitly converted to int64 for storage
        assert(recordType.typeForField(mutableIntField) === vt.Int64)
        assert(mutableIntField.mutable === true)

        inside(exprs) {
          case List(
            et.TopLevelDefine(
              et.Binding(`consLoc`, et.RecordConstructor(`recordType`, List(`mutableIntField`, `constDatumField`)))
            ),
            et.TopLevelDefine(
              et.Binding(`predLoc`, et.TypePredicate(`recordType`))
            ),
            et.TopLevelDefine(
              et.Binding(`constAccessorLoc`, et.RecordAccessor(`recordType`, `constDatumField`))
            ),
            et.TopLevelDefine(
              et.Binding(`mutableAccessorLoc`, et.RecordAccessor(`recordType`, `mutableIntField`))
            ),
            et.TopLevelDefine(
              et.Binding(`mutableMutatorLoc`, et.RecordMutator(`recordType`, `mutableIntField`))
            )
          ) =>
        }
    }
  }

  test("nested record types") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val innerExprs = bodyFor("""(define-record-type <inner-type>
                                (inner-type)
                                inner-type?)""")(scope)

    inside(scope("<inner-type>")) {
      case BoundType(innerType: vt.RecordType) =>
        val innerConsLoc = storageLocFor(scope, "inner-type")
        val innerPredLoc = storageLocFor(scope, "inner-type?")

        assert(innerType.sourceName === "<inner-type>")
        assert(innerType.fields === Nil)

        inside(innerExprs) {
          case List(
            et.TopLevelDefine(et.Binding(`innerConsLoc`, et.RecordConstructor(`innerType`, List()))),
            et.TopLevelDefine(et.Binding(`innerPredLoc`, et.TypePredicate(`innerType`)))
          ) =>
        }

        val outerExprs = bodyFor("""(define-record-type <outer-type>
                                    (outer-type inner-field)
                                    outer-type?
                                    ([inner-field : <inner-type>] outer-type-inner-field))""")(scope)

        inside(scope("<outer-type>")) {
          case BoundType(outerType: vt.RecordType) =>
            val outerConsLoc = storageLocFor(scope, "outer-type")
            val outerPredLoc = storageLocFor(scope, "outer-type?")
            val innerFieldAccessorLoc = storageLocFor(scope, "outer-type-inner-field")

            val innerField = outerType.fieldForName("inner-field")
            assert(outerType.typeForField(innerField) === innerType)
            assert(innerField.mutable === false)

            inside(outerExprs) {
              case List(
                et.TopLevelDefine(
                  et.Binding(`outerConsLoc`, et.RecordConstructor(`outerType`, List(`innerField`)))
                ),
                et.TopLevelDefine(
                  et.Binding(`outerPredLoc`, et.TypePredicate(`outerType`))
                ),
                et.TopLevelDefine(
                  et.Binding(`innerFieldAccessorLoc`, et.RecordAccessor(`outerType`, `innerField`))
                )
              ) =>
            }
        }
    }
  }

  test("inherting record types") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    val parentExprs = bodyFor("""(define-record-type <parent>
                                   (parent const-int)
                                   parent?
                                   ([const-int : <integer>] parent-const-int))""")(scope)

    val childExprs = bodyFor("""(define-record-type <child> <parent>
                                  (child const-int const-flonum)
                                  child?
                                  ([const-flonum : <flonum>] child-const-flonum))""")(scope)

    inside((scope("<parent>"), scope("<child>"))) {
      case (BoundType(parentType: vt.RecordType), BoundType(childType: vt.RecordType)) =>
        val parentConsLoc = storageLocFor(scope, "parent")
        val parentPredLoc = storageLocFor(scope, "parent?")
        val constIntAccessorLoc = storageLocFor(scope, "parent-const-int")

        assert(parentType.sourceName === "<parent>")
        assert(parentType.parentRecordOpt === None)

        val constIntField = parentType.fieldForName("const-int")
        assert(parentType.typeForField(constIntField) === vt.Int64)
        assert(constIntField.mutable === false)

        inside(parentExprs) {
          case List(
            et.TopLevelDefine(
              et.Binding(`parentConsLoc`, et.RecordConstructor(`parentType`, List(`constIntField`)))
            ),
            et.TopLevelDefine(
              et.Binding(`parentPredLoc`, et.TypePredicate(`parentType`))
            ),
            et.TopLevelDefine(
              et.Binding(`constIntAccessorLoc`, et.RecordAccessor(`parentType`, `constIntField`))
            )
          ) =>
        }

        val childConsLoc = storageLocFor(scope, "child")
        val childPredLoc = storageLocFor(scope, "child?")
        val constFlonumAccessorLoc = storageLocFor(scope, "child-const-flonum")

        assert(childType.sourceName === "<child>")
        assert(childType.parentRecordOpt === Some(parentType))

        val constFlonumField = childType.fieldForName("const-flonum")
        assert(childType.typeForField(constFlonumField) === vt.Double)
        assert(constFlonumField.mutable === false)

        inside(childExprs) {
          case List(
            et.TopLevelDefine(
              et.Binding(
                `childConsLoc`,
                et.RecordConstructor(`childType`, List(`constIntField`, `constFlonumField`))
              )
            ),
            et.TopLevelDefine(
              et.Binding(`childPredLoc`, et.TypePredicate(`childType`))
            ),
            et.TopLevelDefine(
              et.Binding(`constFlonumAccessorLoc`, et.RecordAccessor(`childType`, `constFlonumField`))
            )
          ) =>
        }
    }
  }

  test("duplicate field name fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <new-type>
                 (new-type const-int)
                 new-type?
                 ([const-int : <native-int64>] new-type-const-int)
                 ((const-int : <integer>) new-type-mutable-int set-new-type-mutable-int!))""")(scope)
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
      bodyFor("""(define-record-type <child> <integer>
                   (child const-int)
                   child?
                   ((const-int : <integer>) child-const-int))""")(scope)
    }
  }

  test("inheriting record non-existent type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    intercept[UnboundVariableException] {
      bodyFor("""(define-record-type <child> <does-not-exist>>
                   (child const-int)
                   child?
                   ((const-int : <integer>) child-const-int))""")(scope)
    }
  }

  test("duplicate field name in child record") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    bodyFor("""(define-record-type <parent>
                 (parent const-int)
                 parent?
                 ((const-int : <integer>) parent-const-int))""")(scope)

    intercept[BadSpecialFormException] {
      bodyFor("""(define-record-type <child> <parent>
                   (child const-int)
                   child?
                   ((const-int : <integer>) child-const-int))""")(scope)
    }
  }

  test("child not initialising non-defaultable parent strucutre field fails") {
    val scope = new Scope(collection.mutable.Map(), Some(baseScope))

    bodyFor("""(define-record-type <parent>
                 (parent const-int)
                 parent?
                 ((const-int : <integer>) parent-const-int))""")(scope)

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
      case BoundType(recordType: vt.RecordType) =>
        val consLoc = storageLocFor(scope, "recursive-type")
        val predLoc = storageLocFor(scope, "recursive-type?")
        val parentAccessorLoc = storageLocFor(scope, "recursive-type-parent")

        assert(recordType.sourceName === "<recursive-type>")

        inside(consLoc) {
          case boundCons: BoundRecordConstructor =>
        }

        val parentField = recordType.fieldForName("parent")
        assert(recordType.typeForField(parentField) === vt.UnionType(Set(recordType, vt.UnitType)))
        assert(parentField.mutable === false)

        inside(exprs) {
          case List(
            et.TopLevelDefine(
              et.Binding(`consLoc`, et.RecordConstructor(`recordType`, List(`parentField`)))
            ),
            et.TopLevelDefine(
              et.Binding(`predLoc`, et.TypePredicate(`recordType`))
            ),
            et.TopLevelDefine(
              et.Binding(`parentAccessorLoc`, et.RecordAccessor(`recordType`, `parentField`))
            )
          ) =>
        }
    }
  }
}
