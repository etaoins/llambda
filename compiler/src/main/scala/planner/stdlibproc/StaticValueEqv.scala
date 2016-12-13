package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}

private[stdlibproc] object StaticValueEqv {
  private def elementsAreEqual(elems1: Seq[iv.IntermediateValue], elems2: Seq[iv.IntermediateValue]): Option[Boolean] = {
    if (elems1.length != elems2.length) {
      // Nope
      return Some(false)
    }

    val equalityResult = elems1.zip(elems2).map { case (elem1, elem2) =>
      valuesAreEqual(elem1, elem2)
    }

    if (equalityResult.contains(Some(false))) {
      // Definitely inequal
      Some(false)
    }
    else if (equalityResult.forall(_ == Some(true))) {
      // Definitely equal
      Some(true)
    }
    else {
      // Unknown
      None
    }
  }

  type EqvFunction = (iv.IntermediateValue, iv.IntermediateValue) => Option[Boolean]

  def valuesAreEqv(val1: iv.IntermediateValue, val2: iv.IntermediateValue): Option[Boolean] = {
    if (val1 eq val2) {
      // Definitely the same
      return Some(true)
    }

    if ((val1.schemeType & val2.schemeType) == vt.EmptySchemeType) {
      // Types are completely disjoint - they can't be equivalent
      return Some(false)
    }

    (val1, val2) match {
      case (iv.ConstantFlonumValue(val1), iv.ConstantFlonumValue(val2)) =>
        // This needs to be special to deal with NaN
        Some(val1.equals(val2))

      case (constVal1: iv.ConstantValue, constVal2: iv.ConstantValue) =>
        Some(constVal1 == constVal2)

      case _ =>
        None
    }
  }

  def valuesAreEqual(val1: iv.IntermediateValue, val2: iv.IntermediateValue): Option[Boolean] = {
    (val1, val2) match {
      case (knownPair1: iv.KnownPair, knownPair2: iv.KnownPair) =>
       elementsAreEqual(List(knownPair1.car, knownPair1.cdr), List(knownPair2.car, knownPair2.cdr))

      case (iv.ConstantVectorValue(elements1), iv.ConstantVectorValue(elements2)) =>
        elementsAreEqual(elements1, elements2)

      case (iv.ConstantBytevectorValue(elements1), iv.ConstantBytevectorValue(elements2)) =>
        Some(elements1 == elements2)

      case (iv.ConstantRecordValue(recordType1, fieldValues1, false),
            iv.ConstantRecordValue(recordType2, fieldValues2, false)) =>
        if (recordType1 == recordType2) {
          val fields = recordType1.fields

          elementsAreEqual(fields.map(fieldValues1), fields.map(fieldValues2))
        }
        else {
          Some(false)
        }

      case _ =>
        valuesAreEqv(val1, val2)
    }
  }
}
