package io.llambda.compiler.planner
import io.llambda

import scala.io.Codec

import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{step => ps}
import llambda.compiler.planner.{intermediatevalue => iv}
import llambda.compiler.codegen.RuntimeFunctions
import llambda.compiler.InternalCompilerErrorException

object PlanSymbolEquality {
  private def planRuntimeEqualityFallback(
      symbolTemp1: ps.TempValue,
      symbolTemp2: ps.TempValue
  )(implicit plan: PlanWriter): ps.TempValue = {
    val entryTemp = ps.EntryPointTemp()
    plan.steps += ps.CreateNamedEntryPoint(
      entryTemp,
      RuntimeFunctions.symbolIsEqvSignature,
      RuntimeFunctions.symbolIsEqvSymbol
    )

    val resultPred = ps.Temp(vt.Predicate)
    plan.steps += ps.Invoke(
      result=Some(resultPred),
      signature=RuntimeFunctions.symbolIsEqvSignature,
      entryPoint=entryTemp,
      arguments=List(symbolTemp1, symbolTemp2)
    )

    resultPred
  }

  private def planMinimalTest(
      staticName: String,
      otherNames: Set[String],
      dynamicTemp: ps.TempValue
  )(implicit plan: PlanWriter): ps.TempValue = {
    // Convert everything to UTF-8 first
    val staticUtf8Data = Codec.toUTF8(staticName)
    val otherUtf8Data = otherNames.map(Codec.toUTF8)

    // Calculate all possible lengths to pass to codegen
    val possibleLengths = otherUtf8Data.map(_.length) + staticUtf8Data.length

    // Find our static byte length
    val staticByteLength = staticUtf8Data.length

    val lengthMatchPred = if (possibleLengths == Set(staticByteLength)) {
      // All symbol have the same length; skip length test
      val staticTrue = ps.Temp(vt.Predicate)
      plan.steps += ps.CreateNativeInteger(staticTrue, 1, vt.Predicate.bits)

      staticTrue
    }
    else {
      // Load our dynamic length
      val dynamicLengthTemp = ps.Temp(vt.UInt16)
      plan.steps += ps.LoadSymbolByteLength(dynamicLengthTemp, dynamicTemp, Some(possibleLengths))

      // Create our static length
      val staticLengthTemp = ps.Temp(vt.UInt16)
      plan.steps += ps.CreateNativeInteger(staticLengthTemp, staticByteLength, 32)

      // Compare them
      val lengthMatchPred = ps.Temp(vt.Predicate)
      plan.steps += ps.IntegerCompare(lengthMatchPred, ps.CompareCond.Equal, None, staticLengthTemp, dynamicLengthTemp)

      lengthMatchPred
    }

    // We only care about symbols with the same length as ourselves as we always do a length check
    val sameLengthMatches = otherUtf8Data.filter(_.length == staticByteLength)

    if (sameLengthMatches.isEmpty) {
      // We have no symbols of the same length - the lengthMatchPred is enough
      lengthMatchPred
    }
    else {
      plan.buildCondBranch(test=lengthMatchPred,
        trueBuilder={ truePlan =>
          // Find the first byte offset that's unique between all of the possible symbol names
          // This must exist - staticName can't appear in otherNames
          val firstDifferentOffset = ((0 until staticByteLength) find { offset =>
            sameLengthMatches.forall(_(offset) != staticUtf8Data(offset))
          }).get

          // Find the possible values for this byte
          val possibleValues = sameLengthMatches.map(_(firstDifferentOffset)) + staticUtf8Data(firstDifferentOffset)

          // Create a temp value and store the offset to read
          val offsetTemp = ps.Temp(vt.UInt32)
          truePlan.steps += ps.CreateNativeInteger(offsetTemp, firstDifferentOffset, 32)

          // Load the byte from the symbol
          val actualByteTemp = ps.Temp(vt.UInt8)
          truePlan.steps += ps.LoadSymbolByte(
            result=actualByteTemp,
            boxed=dynamicTemp,
            offset=offsetTemp,
            symbolByteLength=staticByteLength,
            possibleValuesOpt=Some(possibleValues)
          )

          // Create a temp for the expected byte
          val expectedByteTemp = ps.Temp(vt.UInt8)
          truePlan.steps += ps.CreateNativeInteger(expectedByteTemp, staticUtf8Data(firstDifferentOffset), 8)

          // Make sure the byte matches
          val matchTemp = ps.Temp(vt.Predicate)
          truePlan.steps += ps.IntegerCompare(matchTemp, ps.CompareCond.Equal, None, actualByteTemp, expectedByteTemp)

          matchTemp
        },
        falseBuilder={ falsePlan =>
          val falseTemp = ps.Temp(vt.Predicate)
          falsePlan.steps += ps.CreateNativeInteger(falseTemp, 0, vt.Predicate.bits)

          falseTemp
        }
      )
    }
  }

  private def extractLiteralSymbolNames(schemeType: vt.SchemeType): Set[String] = schemeType match {
    case vt.LiteralSymbolType(symbolName) =>
      Set(symbolName)

    case vt.UnionType(memberTypes) =>
      memberTypes.flatMap(extractLiteralSymbolNames)

    case _ =>
      Set()
  }

  /** Plans a symbol equality test between a known symbol name and a dynamic symbol value
    *
    * This assumes that vt.SatisfiesType has been already consulted to remove statically impossible comparison
    * results. If the dynamicSymbolType is a union of literal symbol types then an optimal test using a combination of
    * a length check and a single byte load will be constructed. This is intended to optimise the usage of literal
    * symbol union types as enumerations.
    *
    * @param  staticSymbolName   Name of the statically known symbol to test
    * @param  dynamicSymbolType  Scheme type of the dynamic symbol
    * @param  dynamicSymbolTemp  Temp value of the dynamic symbol. This must be of type ct.SymbolCell
    */
  def compareStatic(
      staticSymbolName: String,
      dynamicSymbolType: vt.SchemeType,
      dynamicSymbolTemp: ps.TempValue
  )(implicit plan: PlanWriter): ps.TempValue =
    if (vt.SatisfiesType(dynamicSymbolType, vt.SymbolType) == Some(true)) {
      // This can be any symbol - we can't do anything clever here
      val staticSymbolTemp = ps.Temp(vt.SymbolType, knownConstant=true)
      plan.steps += ps.CreateSymbolCell(staticSymbolTemp, staticSymbolName)

      planRuntimeEqualityFallback(staticSymbolTemp, dynamicSymbolTemp)
    }
    else {
      val dynamicNames = extractLiteralSymbolNames(dynamicSymbolType)

      if (!dynamicNames.contains(staticSymbolName)) {
        // This means the caller didn't use SatisfiesType
        throw new InternalCompilerErrorException("Attempted PlanSymbolEquality with impossible symbol match")
      }

      planMinimalTest(staticSymbolName, dynamicNames - staticSymbolName, dynamicSymbolTemp)
    }

  /** Plans a symbol equality test between two dynamic symbol value
    *
    * This assumes that vt.SatisfiesType has been already consulted to remove statically impossible comparison
    * results.
    */
  def compareDynamic(
    val1: iv.IntermediateValue,
    val2: iv.IntermediateValue
  )(implicit plan: PlanWriter): ps.TempValue =
    (val1.schemeType, val2.schemeType) match {
      case (vt.LiteralSymbolType(staticName1), dynamicType2) =>
        val dynamicTemp2 = val2.toTempValue(vt.SymbolType)
        compareStatic(staticName1, dynamicType2, dynamicTemp2)

      case (dynamicType1, vt.LiteralSymbolType(staticName2)) =>
        val dynamicTemp1 = val1.toTempValue(vt.SymbolType)
        compareStatic(staticName2, dynamicType1, dynamicTemp1)

      case _ =>
        planRuntimeEqualityFallback(val1.toTempValue(vt.SymbolType), val2.toTempValue(vt.SymbolType))
    }

}
