package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite, Inside}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

class ExtractExternalRecordTypeSuite extends FunSuite with Inside with testutil.ExprHelpers {
  val primitiveScope = new ImmutableScope(collection.mutable.Map(Primitives.bindings.toSeq: _*))
  val nfiScope = new ImmutableScope(testutil.NfiExports(), Some(primitiveScope))

  test("external record type without name or predicate") {
    val namelessType = ExtractExternalRecordType(NoSourceLocation, None, Nil)

    assert(namelessType.sourceNameOpt === None)
    assert(namelessType.predicateOpt === None)
  }

  test("external record type with name and without predicate") {
    val namedType = ExtractExternalRecordType(NoSourceLocation, Some("<type-name>"), Nil)

    assert(namedType.sourceNameOpt === Some("<type-name>"))
    assert(namedType.predicateOpt === None)
  }

  test("external record type with name and predicate of correct type") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))
    bodyFor("""
      (define-type <e-r-t> (ExternalRecord (native-function system-library "pred_func" (-> <any> <native-bool>))))
    """)(scope)


    inside(scope("<e-r-t>")) {
      case BoundType(predType: vt.ExternalRecordType) =>
        assert(predType.sourceNameOpt === Some("<e-r-t>"))
        assert(predType.predicateOpt === Some(vt.ExternalRecordTypePredicate(
          library=NativeSystemLibrary,
          nativeSymbol="pred_func"
        )))
    }
  }

  test("external record type with name and predicate of incorrect type fails") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor("""
        (define-type <e-r-t> (ExternalRecord (native-function system-library "pred_func" (-> <any> <native-int32>))))
      """)(scope)
    }
  }

  test("external record type incorrect number of args") {
    val scope = new Scope(collection.mutable.Map(), Some(nfiScope))

    intercept[BadSpecialFormException] {
      bodyFor("""
        (define-type <e-r-t> (ExternalRecord (native-function system-library "pred_func" (-> <any> <native-int32>)) "foo"))
      """)(scope)
    }
  }
}
