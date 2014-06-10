package io.llambda.compiler.planner.intermediatevalue
import io.llambda

import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{PlanWriter}
import llambda.compiler.{celltype => ct}
import llambda.compiler.RuntimeErrorMessage

trait IntermediateCellValue extends IntermediateValue {
  val cellType : ct.CellType   
  val tempValue : ps.TempValue

  lazy val typeDescription = possibleTypes.toList match {
    case single :: Nil =>
      s"cell of type ${single.schemeName}"

    case multiple =>
      "cell with possible types {" + multiple.map(_.schemeName).mkString(", ") + "}" 
  }

  def toCellTempValue(targetType : ct.CellType, errorMessageOpt : Option[RuntimeErrorMessage])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : ps.TempValue = {
    val targetConcreteTypes = targetType.concreteTypes

    // Are our possible concrete types a subset of the target types?
    if (possibleTypes.subsetOf(targetConcreteTypes)) {
      // Need to cast to the right type
      // We've confirmed that no checking is needed because all of our possible types are equal to or supertypes of the
      // target type
      cellTempToSupertype(tempValue, cellType, targetType) 
    }
    else if (!possibleTypes.intersect(targetConcreteTypes).isEmpty) {
      val errorMessage = errorMessageOpt getOrElse {
        RuntimeErrorMessage(
          name=s"subcastTo${targetType.llvmName.capitalize}Failed",
          text=s"Runtime cast to subtype '${targetType.schemeName}' failed"
        )
      }

      // This is possible but not guaranteed. Verify the type at runtime.
      val castTemp = new ps.TempValue(tempValue.isGcManaged)
      plan.steps += ps.CastCellToSubtypeChecked(castTemp, worldPtr, tempValue, targetType, errorMessage, possibleTypes)
      castTemp
    }
    else {
      // Not possible
      impossibleConversion(s"Unable to convert ${typeDescription} to ${targetType.schemeName}") 
    }
  }

}

