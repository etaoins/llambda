package io.llambda.compiler.planner.reportproc
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.OutOfBoundsException
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._
import llambda.compiler.{celltype => ct}

import llambda.compiler.valuetype.Implicits._

object VectorProcPlanner extends ReportProcPlanner {
  private def makeFilledVector(state : PlannerState)(
      length : (ContextLocated, iv.IntermediateValue),
      fillValue : iv.IntermediateValue
  )(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : iv.IntermediateValue = {
    val lengthValue = length._2

    val unstableType = lengthValue match {
      case constantInt : iv.ConstantExactIntegerValue =>
        val knownLength = constantInt.value
        vt.SpecificVectorType(Vector.fill(knownLength.toInt)(fillValue.schemeType)) 

      case _ =>
        vt.VectorOfType(fillValue.schemeType)
    }

    val stableType = vt.StabiliseType(unstableType, plan.config.schemeDialect)
    
    // Get the fill before be allocate the vector
    val fillTemp = fillValue.toTempValue(vt.AnySchemeType)

    // Allocate the vector
    val vectorTemp = ps.CellTemp(ct.VectorCell)
    val elementsTemp = ps.VectorElementsTemp()

    val lengthTemp = plan.withContextLocation(length._1) {
      lengthValue.toTempValue(vt.UInt32)
    }

    plan.steps += ps.InitVector(vectorTemp, elementsTemp, lengthTemp)

    // Create a "true" predicate for our loop exit condition
    val trueTemp = ps.Temp(vt.Predicate)
    plan.steps += ps.CreateNativeInteger(trueTemp, 1, vt.Predicate.bits)

    // Initialise it
    val loopBodyPlan = plan.forkPlan()

    val vectorIndexTemp = ps.Temp(vt.UInt32)
    loopBodyPlan.steps += ps.StoreVectorElement(vectorTemp, elementsTemp, vectorIndexTemp, fillTemp)

    val unusedResultTemp = ps.Temp(vt.Predicate)
    
    plan.steps += ps.ForAll(
      result=unusedResultTemp,
      loopCountValue=lengthTemp,
      loopIndexValue=vectorIndexTemp,
      loopSteps=loopBodyPlan.steps.toList,
      loopResultPred=trueTemp
    )

    new iv.CellValue(unstableType, BoxedValue(ct.VectorCell, vectorTemp), knownAllocated=true) 
  }

  def apply(state : PlannerState)(reportName : String, operands : List[(ContextLocated, iv.IntermediateValue)])(implicit plan : PlanWriter, worldPtr : ps.WorldPtrValue) : Option[iv.IntermediateValue] = (reportName, operands) match {
    case ("make-vector", List(length)) =>
      Some(makeFilledVector(state)(length, iv.UnitValue))
    
    case ("make-vector", List(length, (_, fillValue))) =>
      Some(makeFilledVector(state)(length, fillValue))
    
    case ("vector", initialElements) =>
      val initialElementValues = initialElements.map(_._2)
      val vectorType = vt.SpecificVectorType(initialElementValues.map({ elementValue =>
        vt.DirectSchemeTypeRef(elementValue.schemeType)
      }).toVector)

      // Create temp values before we init the vector so there's no GC barrier while the vector is uninitialised
      val elementTemps = initialElementValues.map(_.toTempValue(vt.AnySchemeType))

      val lengthTemp = ps.Temp(vt.UInt32)
      plan.steps += ps.CreateNativeInteger(lengthTemp, initialElements.length, vt.UInt32.bits)

      val vectorTemp = ps.CellTemp(ct.VectorCell)
      val elementsTemp = ps.VectorElementsTemp()
      plan.steps += ps.InitVector(vectorTemp, elementsTemp, lengthTemp)
      
      elementTemps.zipWithIndex.map { case (elementTemp, index) =>
        val indexTemp = ps.Temp(vt.UInt32)
        plan.steps += ps.CreateNativeInteger(indexTemp, index, vt.UInt32.bits)
        plan.steps += ps.StoreVectorElement(vectorTemp, elementsTemp, indexTemp, elementTemp)
      }

      Some(TempValueToIntermediate(vectorType, vectorTemp)(plan.config))

    case ("vector-length", List((_, constantVector : iv.ConstantVectorValue))) =>
      Some(new iv.ConstantExactIntegerValue(constantVector.elements.length))
      
    case ("vector-length", List((located, vectorValue))) =>
      val vectorTemp = plan.withContextLocation(located) {
        vectorValue.toTempValue(vt.VectorOfType(vt.AnySchemeType))
      }

      val resultTemp = ps.Temp(vt.UInt32)
      plan.steps += ps.LoadVectorLength(resultTemp, vectorTemp)

      Some(TempValueToIntermediate(vt.UInt32, resultTemp)(plan.config))
    
    case ("vector-ref", List((_, constantVector : iv.ConstantVectorValue), (_, constantInt : iv.ConstantExactIntegerValue))) =>
      val index = constantInt.value

      if ((index < 0) || (index >= constantVector.elements.length)) {
        throw new OutOfBoundsException(
          plan.activeContextLocated,
          s"Vector index ${index} out of bounds"
        )
      }

      Some(constantVector.elements(index.toInt))

    case ("vector-ref", List((vectorLocated, vectorValue), (_, constantInt : iv.ConstantExactIntegerValue))) =>
      vectorValue.schemeType match {
        case vectorType @ vt.SpecificVectorType(elementTypes) =>
          val index = constantInt.value

          if (index >= elementTypes.size) {
            throw new OutOfBoundsException(
              plan.activeContextLocated,
              s"Vector index ${index} out of bounds"
            )
          }
          
          val vectorTemp = plan.withContextLocation(vectorLocated) {
            vectorValue.toTempValue(vt.VectorOfType(vt.AnySchemeType))
          }
          
          // Load the vector elements pointer
          val elementsTemp = ps.VectorElementsTemp()
          plan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)

          // Load the element
          val resultTemp = ps.Temp(vt.AnySchemeType)
          val indexTemp = constantInt.toTempValue(vt.UInt32)
          
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

          if (index >= elementTypes.size) {
            throw new OutOfBoundsException(
              plan.activeContextLocated,
              s"Vector index ${index} out of bounds"
            )
          }

          val vectorTemp = plan.withContextLocation(vectorLocated) {
            vectorCellValue.toTempValue(vt.VectorOfType(vt.AnySchemeType))
          }

          // Load the vector elements pointer
          val elementsTemp = ps.VectorElementsTemp()

          // Store the element
          plan.steps += ps.LoadVectorElementsData(elementsTemp, vectorTemp)
          val indexTemp = constantInt.toTempValue(vt.UInt32)
          
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
