package io.llambda.llvmir

import collection.mutable.ListBuffer

trait IrBranchTarget {
  val label : String
}

private[llvmir] abstract class IrInstrBuilder(protected val nameSource : LocalNameSource) {
  val function : IrFunctionBuilder

  private var terminated : Boolean = false

  // This contains our instructions as they're built
  private[llvmir] val irLines = new ListBuffer[String]
  
  private def metadataMapToIr(metadataMap : Map[String, Metadata]) =
    metadataMap.toSeq.sortBy(_._1).map({ case (tagName, metadata) =>
      s", !${tagName} ${metadata.toIr}"
    }).mkString("")

  protected def addInstruction(instruction : String, metadata : Map[String, Metadata] = Map()) {
    if (terminated) {
      throw new InconsistentIrException("Attempted to add instruction to terminated block")
    }

    // Include any metadata currently active from a withMetadata call
    val allMetadata = function.activeMetadata ++ metadata
    irLines += instruction + metadataMapToIr(allMetadata)
  }

  protected def terminateBlock() {
    terminated = true
  }
}

abstract class IrBlockBuilder(nameSource : LocalNameSource, val label : String) extends IrInstrBuilder(nameSource) with Irable with TerminatorInstrs with MemoryInstrs with BitwiseInstrs with ConversionInstrs with OtherInstrs with BinaryInstrs with AggregateInstrs {
  def comment(text : String) {
    irLines += s"; ${text}"
  }

  def toIr : String = {
    // Tab indent and join with newlines
    s"${label}:\n" + irLines.map("\t" + _).mkString("\n")
  }
}

class IrEntryBlockBuilder(val function : IrFunctionBuilder, nameSource : LocalNameSource) extends IrBlockBuilder(nameSource, "entry")

class IrChildBlockBuilder(val function : IrFunctionBuilder, nameSource : LocalNameSource, label : String) extends IrBlockBuilder(nameSource, label) with IrBranchTarget with PhiInstr
