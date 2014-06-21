package io.llambda.compiler.reducer
import io.llambda

import llambda.compiler.reducer.{partialvalue => pv}
import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

object PartialValueHasType {
  /** Returns if the given partial value satisfies the passed Scheme type
    *
    * If the value definitely satifies the type then Some(true) is returned. If the value definitely does not satisfy
    * the type then Some(false) is returned. If static determination cannot be made then None is returned
    */
  def apply(partialValue : pv.PartialValue, schemeType : vt.CellValueType) : Option[Boolean] = schemeType match {
    case vt.IntrinsicCellType(ct.DatumCell) =>
      // Everything matches this
      Some(true)

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

      partialCellTypeOpt.map(expectedCellType.isTypeOrSupertypeOf(_))

    case _ : vt.RecordLikeType =>
      // There is no way for us to determine record types at reduction time
      None
  }
}
