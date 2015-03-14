package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.RangeException
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._
import llambda.compiler.{celltype => ct}

import llambda.compiler.valuetype.Implicits._

object VectorProcPlanner extends ReportProcPlanner with ReportProcPlannerHelpers {
  /** Maximum length of vector type to create
    *
    * This prevents (make-vector) from creating pathologically large vector types that cause the compiler to consume
    * significant amounts of memory and CPU time.
    */
  private val maximumVectorTypeSize = 1024

  private def makeFilledVector(state : PlannerState)(
      length : (ContextLocated, iv.IntermediateValue),
      fillValue : iv.IntermediateValue
  )(implicit plan : PlanWriter) : iv.IntermediateValue = {
    val lengthValue = length._2

    val unstableType = lengthValue match {
      case iv.ConstantExactIntegerValue(knownLength) if knownLength <= maximumVectorTypeSize =>
        vt.SpecificVectorType(Vector.fill(knownLength.toInt)(fillValue.schemeType))

      case _ =>
        vt.VectorOfType(fillValue.schemeType)
    }

    val stableType = vt.StabiliseType(unstableType, plan.config.schemeDialect)

    val lengthTemp = plan.withContextLocation(length._1) {
      lengthValue.toTempValue(vt.Int64)
    }

    val fillTemp = fillValue.toTempValue(vt.AnySchemeType)

    val vectorTemp = ps.CellTemp(ct.VectorCell)
    plan.steps += ps.InitFilledVector(vectorTemp, lengthTemp, fillTemp)

    new iv.CellValue(stableType, BoxedValue(ct.VectorCell, vectorTemp), knownAllocated=true)
  }

  override def planWithValue(state : PlannerState)(
      reportName : String,
      args : List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan : PlanWriter) : Option[iv.IntermediateValue] = (reportName, args) match {
    case ("make-vector", List(length)) =>
      Some(makeFilledVector(state)(length, iv.UnitValue))
    
    case ("make-vector", List(length, (_, fillValue))) =>
      Some(makeFilledVector(state)(length, fillValue))
    
    case ("vector", initialElements) =>
      val initialElementValues = initialElements.map(_._2)
      val vectorType = vt.SpecificVectorType(initialElementValues.map({ elementValue =>
        vt.DirectSchemeTypeRef(elementValue.schemeType)
      }).toVector)

      val elementTemps = initialElementValues.map(_.toTempValue(vt.AnySchemeType)).toVector

      val vectorTemp = ps.CellTemp(ct.VectorCell)
      plan.steps += ps.InitVector(vectorTemp, elementTemps)

      Some(TempValueToIntermediate(vectorType, vectorTemp)(plan.config))

    case ("vector-length", List((_, iv.ConstantVectorValue(elements)))) =>
      Some(iv.ConstantExactIntegerValue(elements.length))

    case ("vector-length", List((located, vectorValue))) =>
      val vectorTemp = plan.withContextLocation(located) {
        vectorValue.toTempValue(vt.VectorOfType(vt.AnySchemeType))
      }

      val resultTemp = ps.Temp(vt.Int64)
      plan.steps += ps.LoadVectorLength(resultTemp, vectorTemp)

      Some(TempValueToIntermediate(vt.Int64, resultTemp)(plan.config))

    case ("vector-ref", List((_, iv.ConstantVectorValue(elements)), (_, iv.ConstantExactIntegerValue(index)))) =>
      assertIndexValid("(vector-ref)", elements.size, index)

      Some(elements(index.toInt))

    case ("vector-ref", List((vectorLocated, vectorValue), (_, constantInt : iv.ConstantExactIntegerValue))) =>
      vectorValue.schemeType match {
        case vectorType @ vt.SpecificVectorType(elementTypes) =>
          val index = constantInt.value

          assertIndexValid("(vector-ref)", elementTypes.size, index)

          val vectorTemp = plan.withContextLocation(vectorLocated) {
            vectorValue.toTempValue(vt.VectorOfType(vt.AnySchemeType))
          }
          
          // Load the vector elements pointer
          val elementsTemp = ps.VectorElementsTemp()
          plan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)

          // Load the element
          val resultTemp = ps.Temp(vt.AnySchemeType)
          val indexTemp = constantInt.toTempValue(vt.Int64)

          plan.steps += ps.LoadVectorElement(resultTemp, vectorTemp, elementsTemp, indexTemp)

          val elementType = vectorType.unrollChildTypeRef(elementTypes(index.toInt))
          Some(new iv.CellValue(elementType, BoxedValue(ct.AnyCell, resultTemp)))

        case _ =>
          None
      }
    
    case ("vector-set!", List(
        (vectorLocated, vectorCellValue : iv.CellValue),
        (_, constantInt : iv.ConstantExactIntegerValue),
        (_, objectValue)
    )) if vectorCellValue.knownAllocated =>
      vectorCellValue.schemeType match {
        case vectorType @ vt.SpecificVectorType(elementTypes) =>
          val index = constantInt.value

          assertIndexValid("(vector-ref)", elementTypes.size, index)

          val vectorTemp = plan.withContextLocation(vectorLocated) {
            vectorCellValue.toTempValue(vt.VectorOfType(vt.AnySchemeType))
          }

          // Load the vector elements pointer
          val elementsTemp = ps.VectorElementsTemp()

          // Store the element
          plan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)
          val indexTemp = constantInt.toTempValue(vt.Int64)

          // Convert the object to a temp value
          val objectTemp = objectValue.toTempValue(vt.AnySchemeType)
          
          plan.steps += ps.StoreVectorElement(vectorTemp, elementsTemp, indexTemp, objectTemp) 

          Some(iv.UnitValue)

        case _ =>
          None
      }

    case _ =>
      None
  }
}
