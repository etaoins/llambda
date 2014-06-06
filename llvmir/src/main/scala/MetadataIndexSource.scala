package io.llambda.llvmir

class MetadataIndexSource(var nextIndex : Long = 0) {
  def allocate() : Long = {
    val result = nextIndex
    nextIndex = nextIndex + 1
    result
  }
}
