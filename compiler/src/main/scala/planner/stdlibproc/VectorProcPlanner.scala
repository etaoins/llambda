package io.llambda.compiler.planner.stdlibproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._
import llambda.compiler.{celltype => ct}

object VectorProcPlanner extends StdlibProcPlanner with StdlibProcPlannerHelpers {
  private def makeFilledVector(state: PlannerState)(
      length: (ContextLocated, iv.IntermediateValue),
      fillValue: iv.IntermediateValue
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    val lengthValue = length._2

    val knownLengthOpt = lengthValue match {
      case iv.ConstantIntegerValue(knownLength) =>
        plan.withContextLocation(length._1) {
          assertLengthValid("(make-vector)", "vector length", vt.Int64.maxIntValue, knownLength)
        }

        Some(knownLength)

      case _ =>
        None
    }

    if (knownLengthOpt == Some(0L)) {
      // We can use q constant vector - there's no way to mutate a zero length vector
      return iv.ConstantVectorValue(Vector())
    }

    val lengthTemp = plan.withContextLocation(length._1) {
      lengthValue.toTempValue(vt.Int64)
    }

    val fillTemp = fillValue.toTempValue(vt.AnySchemeType)

    val vectorTemp = ps.CellTemp(ct.VectorCell)
    plan.steps += ps.InitFilledVector(vectorTemp, lengthTemp, fillTemp)

    knownLengthOpt match {
      case Some(knownLength) =>
        new iv.KnownVectorCellValue(knownLength, vectorTemp)

      case None =>
        new iv.CellValue(vt.VectorType, BoxedValue(ct.VectorCell, vectorTemp))
    }
  }

  override def planWithValue(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("make-vector", List((_, iv.ConstantIntegerValue(0)))) |
         ("make-vector", List((_, iv.ConstantIntegerValue(0)), _)) |
         ("vector", Nil) =>
      Some(new iv.ConstantVectorValue(Vector()))

    case ("make-vector", List(length)) =>
      Some(makeFilledVector(state)(length, iv.UnitValue))

    case ("make-vector", List(length, (_, fillValue))) =>
      Some(makeFilledVector(state)(length, fillValue))

    case ("vector", initialElements) =>
      val initialElementValues = initialElements.map(_._2)

      val elementTemps = initialElementValues.map(_.toTempValue(vt.AnySchemeType)).toVector

      val vectorTemp = ps.CellTemp(ct.VectorCell)
      plan.steps += ps.InitVector(vectorTemp, elementTemps)

      Some(new iv.KnownVectorCellValue(initialElements.length, vectorTemp))

    case ("vector-length", List((_, knownVector: iv.KnownVector))) =>
      Some(iv.ConstantIntegerValue(knownVector.vectorLength))

    case ("vector-length", List((located, vectorValue))) =>
      val vectorTemp = plan.withContextLocation(located) {
        vectorValue.toTempValue(vt.VectorType)
      }

      val resultTemp = ps.Temp(vt.Int64)
      plan.steps += ps.LoadVectorLength(resultTemp, vectorTemp)

      Some(TempValueToIntermediate(vt.Int64, resultTemp))

    case ("vector-ref", List((_, iv.ConstantVectorValue(elements)), (_, iv.ConstantIntegerValue(index)))) =>
      assertIndexValid("(vector-ref)", elements.size, index)

      Some(elements(index.toInt))

    case ("vector-ref", List((_, knownVector: iv.KnownVector), (_, constantInt: iv.ConstantIntegerValue))) =>
      val index = constantInt.value

      assertIndexValid("(vector-ref)", knownVector.vectorLength, index)

      val vectorTemp = knownVector.toTempValue(knownVector.schemeType)

      // Load the vector elements pointer
      val elementsTemp = ps.VectorElementsTemp()
      plan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)

      // Load the element
      val resultTemp = ps.Temp(vt.AnySchemeType)
      val indexTemp = constantInt.toTempValue(vt.Int64)

      plan.steps += ps.LoadVectorElement(resultTemp, vectorTemp, elementsTemp, indexTemp)

      Some(TempValueToIntermediate(vt.AnySchemeType, resultTemp))

    case ("vector-set!", List(
        (_, vectorCellValue: iv.KnownVectorCellValue),
        (_, constantInt: iv.ConstantIntegerValue),
        (_, objectValue)
    )) =>
      val index = constantInt.value

      assertIndexValid("(vector-set!)", vectorCellValue.vectorLength, index)

      val vectorTemp = vectorCellValue.toTempValue(vectorCellValue.schemeType)

      // Load the vector elements pointer
      val elementsTemp = ps.VectorElementsTemp()

      // Store the element
      plan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)
      val indexTemp = constantInt.toTempValue(vt.Int64)

      // Convert the object to a temp value
      val objectTemp = objectValue.toTempValue(vt.AnySchemeType)

      plan.steps += ps.StoreVectorElement(vectorTemp, elementsTemp, indexTemp, objectTemp)

      Some(iv.UnitValue)

    case ("list->vector", List((_, knownList: iv.KnownListElement))) =>
      knownList.toValueListOpt map { initialElements =>
        val elementTemps = initialElements.map(_.toTempValue(vt.AnySchemeType)).toVector

        val vectorTemp = ps.CellTemp(ct.VectorCell)
        plan.steps += ps.InitVector(vectorTemp, elementTemps)

        new iv.KnownVectorCellValue(initialElements.length, vectorTemp)
      }

    case ("vector->list", List((_, iv.ConstantVectorValue(elements)))) =>
      Some(ValuesToList(elements.toList))

    case _ =>
      None
  }
}
