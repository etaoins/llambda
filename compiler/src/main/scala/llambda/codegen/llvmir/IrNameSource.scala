package llambda.codegen.llvmir

private sealed abstract class IrNameSource {
  private val nextNumberForBaseName = new collection.mutable.HashMap[String, Int]()

  def allocate(baseName : String) : String = {
    // Allocate another number
    val nextNumber = nextNumberForBaseName.getOrElse(baseName, 1)
    nextNumberForBaseName.put(baseName, nextNumber + 1)

    // Return the new name
    s"${baseName}${nextNumber}"
  }
}

// These are different just so the type system can distinguish them
private[llvmir] class GlobalNameSource extends IrNameSource
private[llvmir] class LocalNameSource extends IrNameSource
