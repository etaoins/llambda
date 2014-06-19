package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler.reducer.{partialvalue => pv}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object PartialValueSatisfiesType {
  /** Returns if the given partial value satisfies the passed Scheme type
    *
    * This is used to ensure typed lambdas are being invoked with correctly typed arguments before reduction
    */
  def apply(partialValue : pv.PartialValue, schemeType : vt.CellValueType) = schemeType match {
    case vt.IntrinsicCellType(ct.DatumCell) =>
      // Everything matches this
      true

    case vt.IntrinsicCellType(expectedCellType) =>
      val partialCellTypeOpt = partialValue match {
        case _ : pv.PartialPair =>
          Some(ct.PairCell)

        case _ : pv.PartialVector =>
          Some(ct.VectorCell)

        case pv.LiteralLeaf(literalValue) =>
          Some(literalValue.cellType)

        case _ => 
          // We don't know the types of unreduced expressions yet
          None
      }

      partialCellTypeOpt.map(expectedCellType.isTypeOrSupertypeOf(_)).getOrElse(false)

    case _ : vt.RecordLikeType =>
      // There is no way for us to determine record types at reduction time
      false
  }
}
