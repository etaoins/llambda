package io.llambda.compiler.codegen
import io.llambda

import llambda.llvmir._
import llambda.compiler.{valuetype => vt}

object GenTestRecordLikeClass {
  def apply(block: IrBlockBuilder)(
      recordCellIr: IrValue,
      generatedType: GeneratedType,
      possibleTypesOpt: Option[Set[GeneratedType]] = None
  ): IrValue = {
    // Find the cell type of our record cell
    val cellType = generatedType.recordLikeType.cellType

    val rangeMetadataOpt = possibleTypesOpt flatMap { possibleTypes =>
      // Account for the record type itself and any possible derived types
      val possibleClassIds = possibleTypes flatMap { possibleType =>
        (possibleType.classId to possibleType.lastChildClassId)
      }

      RangeMetadata.fromPossibleValues(
        integerType=cellType.recordClassIdIrType,
        possibleClassIds
      )
    }

    val loadMetadata = rangeMetadataOpt match {
      case Some(rangeMetadata) => Map("range" -> rangeMetadata)
      case _ =>                   Map[String, Metadata]()
    }

    // Load the actual class ID
    val actualClassIdIr = cellType.genLoadFromRecordClassId(block)(recordCellIr, loadMetadata)

    // Now compare
    if (generatedType.classId == generatedType.lastChildClassId) {
      val testClassIdIr = IntegerConstant(cellType.recordClassIdIrType, generatedType.classId)
      block.icmp("classMatches")(IComparisonCond.Equal, None, testClassIdIr, actualClassIdIr)
    }
    else {
      // The type tree that this type is the root of is guaranteed to have contiguous type IDs
      // Test if it's in the expected range
      val minClassIdIr = IntegerConstant(cellType.recordClassIdIrType, generatedType.classId)
      val hasMinClassIdIr = block.icmp("hasMinClassId")(IComparisonCond.GreaterThanEqual, Some(false), actualClassIdIr, minClassIdIr)

      val maxClassIdIr = IntegerConstant(cellType.recordClassIdIrType, generatedType.lastChildClassId)
      val hasMaxClassIdIr = block.icmp("hasMaxClassId")(IComparisonCond.LessThanEqual, Some(false), actualClassIdIr, maxClassIdIr)

      block.and("classMatches")(hasMinClassIdIr, hasMaxClassIdIr)
    }
  }
}

