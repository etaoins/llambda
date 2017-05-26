package io.llambda.compiler.planner.stdlibproc
import io.llambda

import scala.io.Codec

import llambda.compiler.{valuetype => vt}
import llambda.compiler.ContextLocated
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.planner._
import llambda.compiler.{celltype => ct}


object BytevectorProcPlanner extends StdlibProcPlanner with StdlibProcPlannerHelpers {
  private def makeFilledBytevector(state: PlannerState)(
      length: (ContextLocated, iv.IntermediateValue),
      fillValue: iv.IntermediateValue
  )(implicit plan: PlanWriter): iv.IntermediateValue = {
    val lengthValue = length._2

    val knownLengthOpt = lengthValue match {
      case iv.ConstantIntegerValue(knownLength) =>
        plan.withContextLocation(length._1) {
          assertLengthValid("(make-bytevector)", "bytevector length", vt.Int64.maxIntValue, knownLength)
        }

        Some(knownLength)

      case _ =>
        None
    }

    if (knownLengthOpt == Some(0L)) {
      // We can use a constant bytevector; there's no way to mutate a zero length bytevector
      return iv.ConstantBytevectorValue(Vector())
    }

    val lengthTemp = plan.withContextLocation(length._1) {
      lengthValue.toTempValue(vt.Int64)
    }

    val fillTemp = fillValue.toTempValue(vt.UInt8)

    val bytevectorTemp = ps.TempValue()
    plan.steps += ps.InitFilledBytevector(bytevectorTemp, lengthTemp, fillTemp)

    knownLengthOpt match {
      case Some(knownLength) =>
        new iv.KnownBytevectorCellValue(knownLength, bytevectorTemp)

      case None =>
        new iv.CellValue(vt.BytevectorType, BoxedValue(ct.BytevectorCell, bytevectorTemp))
    }
  }

  override def planWithValue(state: PlannerState)(
      reportName: String,
      args: List[(ContextLocated, iv.IntermediateValue)]
  )(implicit plan: PlanWriter): Option[iv.IntermediateValue] = (reportName, args) match {
    case ("make-bytevector", List(length)) =>
      Some(makeFilledBytevector(state)(length, iv.ConstantIntegerValue(0)))

    case ("make-bytevector", List(length, (_, fillValue))) =>
      Some(makeFilledBytevector(state)(length, fillValue))

    case ("bytevector", Nil) =>
      Some(new iv.ConstantBytevectorValue(Vector()))

    case ("bytevector", initialElements) =>
      val initialElementValues = initialElements.map(_._2)

      // If we know all of the element values we can create a constant shared byte array instaed of setting the values
      // in executable code
      val constantElementValues = initialElementValues.collect({
        case iv.ConstantIntegerValue(value) if (value >= 0) && (value <= 255) =>
          value.toByte
      }).toVector

      val bytevectorTemp = ps.TempValue()
      if (initialElementValues.length == constantElementValues.length) {
        plan.steps += ps.InitStaticBytevector(bytevectorTemp, constantElementValues)
      }
      else {
        val elementTemps = initialElementValues.map(_.toTempValue(vt.UInt8)).toVector
        plan.steps += ps.InitDynamicBytevector(bytevectorTemp, elementTemps)
      }

      Some(new iv.KnownBytevectorCellValue(initialElements.length, bytevectorTemp))

    case ("bytevector-length", List((_, knownBytevector: iv.KnownBytevector))) =>
      Some(iv.ConstantIntegerValue(knownBytevector.bytevectorLength))

    case ("bytevector-length", List((located, bytevectorValue))) =>
      val bytevectorTemp = plan.withContextLocation(located) {
        bytevectorValue.toTempValue(vt.BytevectorType)
      }

      val resultTemp = ps.TempValue()
      plan.steps += ps.LoadBytevectorLength(resultTemp, bytevectorTemp)

      Some(TempValueToIntermediate(vt.Int64, resultTemp))

    case ("bytevector-u8-ref", List((_, iv.ConstantBytevectorValue(elements)), (_, iv.ConstantIntegerValue(index)))) =>
      assertIndexValid("(bytevector-ref)", elements.size, index)

      // Java bytes are signed while Scheme bytes are unsigned
      // Manually zero extend the Java value
      val byteValue = elements(index.toInt)
      val longValue = if (byteValue < 0) byteValue.toLong + 256 else byteValue.toLong

      Some(new iv.ConstantIntegerValue(longValue))

    case ("bytevector-u8-ref", List((_, knownBytevector: iv.KnownBytevector), (_, constantInt: iv.ConstantIntegerValue))) =>
      val index = constantInt.value

      assertIndexValid("(bytevector-u8-ref)", knownBytevector.bytevectorLength, index)

      val bytevectorTemp = knownBytevector.toTempValue(knownBytevector.schemeType)

      // Load the element
      val resultTemp = ps.TempValue()
      val indexTemp = constantInt.toTempValue(vt.Int64)

      plan.steps += ps.LoadBytevectorElement(resultTemp, bytevectorTemp, indexTemp)

      Some(TempValueToIntermediate(vt.UInt8, resultTemp))

    case ("bytevector-copy", List((_, iv.ConstantBytevectorValue(elements)))) =>
      val bytevectorTemp = ps.TempValue()
      plan.steps += ps.InitStaticBytevector(bytevectorTemp, elements)

      Some(new iv.KnownBytevectorCellValue(elements.length, bytevectorTemp))

    case ("string->utf8", List((_, iv.ConstantStringValue("")))) =>
      Some(new iv.ConstantBytevectorValue(Vector()))

    case ("string->utf8", List((_, iv.ConstantStringValue(str)))) =>
      val utf8Data = Codec.toUTF8(str).toVector

      val bytevectorTemp = ps.TempValue()
      plan.steps += ps.InitStaticBytevector(bytevectorTemp, utf8Data)

      Some(new iv.KnownBytevectorCellValue(utf8Data.length, bytevectorTemp))

    case _ =>
      None
  }
}
