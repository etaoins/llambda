package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}

private[reportproc] object StaticValueEqv { 
  private def elementsAreEqual(elems1 : Seq[iv.IntermediateValue], elems2 : Seq[iv.IntermediateValue]) : Option[Boolean] = {
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

  def valuesAreEqv(val1 : iv.IntermediateValue, val2 : iv.IntermediateValue) : Option[Boolean] = {
    if (val1 eq val2) {
      // Definitely the same
      return Some(true)
    }

    if ((val1.schemeType & val2.schemeType) == vt.EmptySchemeType) {
      // Types are completely disjoint - they can't be equivalent
      return Some(false)
    }

    (val1, val2) match {
      case (constBool1 : iv.ConstantBooleanValue, constBool2 : iv.ConstantBooleanValue) =>
        Some(constBool1.value == constBool2.value)

      case (constSymbol1 : iv.ConstantSymbolValue, constSymbol2 : iv.ConstantSymbolValue) =>
        Some(constSymbol1.value == constSymbol2.value)
      
      case (constString1 : iv.ConstantStringValue, constString2 : iv.ConstantStringValue) =>
        Some(constString1.value == constString2.value)
      
      case (constInteger1 : iv.ConstantExactIntegerValue, constInteger2 : iv.ConstantExactIntegerValue) =>
        Some(constInteger1.value == constInteger2.value)
      
      case (constRational1 : iv.ConstantInexactRationalValue, constRational2 : iv.ConstantInexactRationalValue) =>
        if (constRational1.value.isNaN && constRational2.value.isNaN) {
          // Both values are NaN
          Some(true)
        }
        else {
          Some(constRational1.value == constRational2.value)
        }
      
      case (constCharacter1 : iv.ConstantCharacterValue, constCharacter2 : iv.ConstantCharacterValue) =>
        Some(constCharacter1.value == constCharacter2.value)
      
      case (iv.UnitValue, iv.UnitValue) =>
        Some(true)
      
      case (iv.EmptyListValue, iv.EmptyListValue) =>
        Some(true)

      case _ =>
        None
    }
  }
  
  def valuesAreEqual(val1 : iv.IntermediateValue, val2 : iv.IntermediateValue) : Option[Boolean] = {
    (val1, val2) match {
      case (knownPair1 : iv.KnownPair, knownPair2 : iv.KnownPair) =>
       elementsAreEqual(List(knownPair1.car, knownPair1.cdr), List(knownPair2.car, knownPair2.cdr))

      case (constVector1 : iv.ConstantVectorValue, constVector2 : iv.ConstantVectorValue) =>
        elementsAreEqual(constVector1.elements, constVector2.elements)
      
      case (constBytevector1 : iv.ConstantBytevectorValue, constBytevector2 : iv.ConstantBytevectorValue) =>
        Some(constBytevector1.elements == constBytevector2.elements)

      case _ =>
        valuesAreEqv(val1, val2)
    }
  }
}
