package io.llambda.compiler.valuetype
import io.llambda

import org.scalatest.FunSuite

import llambda.compiler.{celltype => ct}
import llambda.compiler.valuetype.{polymorphic => pm}
import Implicits._

class RecordTypeSuite extends SchemeTypeSuite {
  protected val recordInstance1Child1 = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(),
    new RecordType("record1Child1", Nil, parentRecordOpt=Some(recordInstance1))
  )

  protected val recordInstance1Child2 = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(),
    new RecordType("record1Child2", Nil, parentRecordOpt=Some(recordInstance1))
  )

  val polyRecord1TypeVar = new pm.TypeVar("A")
  val polyRecord1 = new RecordType(
    "poly-record",
    List(
      new RecordField("polyField", polyRecord1TypeVar),
      new RecordField("monoField", PortType)
    ),
    typeVars=List(polyRecord1TypeVar)
  )

  val polyRecord1NumberInstance = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(Map(polyRecord1TypeVar -> NumberType)),
    polyRecord1
  )

  val polyRecord1ExactIntInstance = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(Map(polyRecord1TypeVar -> ExactIntegerType)),
    polyRecord1
  )

  val polyRecord1FlonumInstance = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(Map(polyRecord1TypeVar -> FlonumType)),
    polyRecord1
  )

  val polyRecord1NumberInstanceChild1 = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(),
    new RecordType("polyRecord1NumberChild1", Nil, parentRecordOpt=Some(polyRecord1NumberInstance))
  )

  val polyRecord1ExactIntInstanceChild1 = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(),
    new RecordType("polyRecord1ExactIntChild1", Nil, parentRecordOpt=Some(polyRecord1ExactIntInstance))
  )

  val recursiveRecord1SelfVar = new pm.TypeVar("recursive-record")
  val recursiveRecord1 = new RecordType(
    sourceName="recursive-record",
    fields=List(
      new RecordField("recursive-field", UnionType(Set(EmptyListType, recursiveRecord1SelfVar)))
    ),
    selfTypeVarOpt=Some(recursiveRecord1SelfVar)
  )

  val recursiveRecord1Instance = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(),
    recursiveRecord1
  )

  val polyRecord2TypeVar = new pm.TypeVar("F")
  val polyRecord2 = new RecordType(
    "poly-record2",
    List(
      new RecordField("procField", ProcedureType(
        List(polyRecord2TypeVar),
        None,
        ReturnType.SingleValue(UnitType)
      ))
    ),
    typeVars=List(polyRecord2TypeVar)
  )

  val polyRecord2ExactIntInstance = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(Map(polyRecord2TypeVar -> ExactIntegerType)),
    polyRecord2
  )

  val polyRecord2NumberInstance = RecordTypeInstance(
    pm.ReconcileTypeVars.Result(Map(polyRecord2TypeVar -> NumberType)),
    polyRecord2
  )

  test("record types definitely satisfy themselves") {
    assert(SatisfiesType(recordInstance1, recordInstance1) ===
      Some(true)
    )
  }
  
  test("record types definitely don't satisfy other record types") {
    assert(SatisfiesType(recordInstance1, recordInstance2) ===
      Some(false)
    )
  }
  
  test("record types definitely satisfy the record atom") {
    assert(SatisfiesType(recordAtomType, recordInstance1) ===
      Some(true)
    )
  }

  test("the record atom may satisfy a record type") {
    assert(SatisfiesType(recordInstance2, recordAtomType) ===
      None
    )
  }

  test("child record types satisfy parent types") {
    assert(SatisfiesType(recordInstance1, recordInstance1Child1) === Some(true))
  }

  test("parent record types may satisfy child record types") {
    assert(SatisfiesType(recordInstance1Child1, recordInstance1) === None)
  }

  test("sibling record types do not satisfy each other") {
    assert(SatisfiesType(recordInstance1Child1, recordInstance1Child2) === Some(false))
  }

  test("distinct record types intersected with each other is an empty union") {
    assertIntersection(recordInstance1, recordInstance2, EmptySchemeType)
  }
  
  test("union of record types intersected with the record type atom is the original union") {
    assertIntersection(UnionType(Set(recordInstance1, recordInstance2)), recordAtomType, UnionType(Set(recordInstance1, recordInstance2)))
  }

  test("polymorphc record type satisfies itself") {
    assert(SatisfiesType(polyRecord1FlonumInstance, polyRecord1FlonumInstance) === Some(true))
  }

  test("polymorphc record type satisfies distinct polymorph with satisfied fields") {
    assert(SatisfiesType(polyRecord1NumberInstance, polyRecord1FlonumInstance) === Some(true))
  }

  test("polymorphc record type may satisfy distinct polymorph with possibly satisfied fields") {
    assert(SatisfiesType(polyRecord1FlonumInstance, polyRecord1NumberInstance) === None)
  }

  test("polymorphc record type does not satisfy distinct polymorph with unsatisfied fields") {
    assert(SatisfiesType(polyRecord1FlonumInstance, polyRecord1ExactIntInstance) === Some(false))
  }

  test("polymorphic record type may satisfy child type with compatible type") {
    assert(SatisfiesType(polyRecord1NumberInstanceChild1, polyRecord1FlonumInstance) === None)
  }

  test("polymorphic record type does not satisfy child type with incompatible type") {
    assert(SatisfiesType(polyRecord1ExactIntInstanceChild1, polyRecord1FlonumInstance) === Some(false))
  }

  test("polymorphic record type with type variable in contravariant position") {
    assert(SatisfiesType(polyRecord2NumberInstance, polyRecord2ExactIntInstance) === None)
    assert(SatisfiesType(polyRecord2ExactIntInstance, polyRecord2NumberInstance) === Some(true))
  }

  test("recursive record instance satisfies itself") {
    assert(SatisfiesType(recursiveRecord1Instance, recursiveRecord1Instance) === Some(true))
  }

  test("recursive record instance satisfies union containing itself") {
    val recursiveRecordUnion = UnionType(Set(EmptyListType, recursiveRecord1Instance))

    assert(SatisfiesType(recursiveRecordUnion, recursiveRecord1Instance) === Some(true))
  }
}
