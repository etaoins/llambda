package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.{celltype => ct}

/** Value object representing a boxed value
  *
  * @param  cellType   Cell type for the boxed value. This tracks which type tempValue is current cast to
  * @param  tempValue  TempValue for the boxed value pointer
  */
case class BoxedValue(
  cellType: ct.CellType,
  tempValue: ps.TempValue
) {
  /** Casts this value to a TempValue of the given type
    *
    * If the value already has the requested cell type it will be returned directly
    *
    * The type checker must ensure that the the value satisfies the passed target type before accessing the cast value.
    * Otherwise TBAA rules may be violated with undefined results.
    */
  def castToCellTempValue(targetType: ct.CellType)(implicit plan: PlanWriter): ps.TempValue = {
    if (targetType == cellType) {
      tempValue
    }
    else {
      val castTemp = new ps.TempValue(tempValue.isGcManaged)
      plan.steps += ps.CastCellToTypeUnchecked(castTemp, tempValue, targetType)
      castTemp
    }
  }
}

