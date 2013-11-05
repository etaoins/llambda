package llambda.frontend

import llambda._

class LibraryLoader {
  private val exprBuffer = collection.mutable.ListBuffer[et.Expression]()
  private val loadedFiles = collection.mutable.Map.empty[String, Map[String, BoundValue]]

  private def loadLibraryFile(filename : String, libraryName : Seq[LibraryNameComponent])(implicit includePath : IncludePath) : Map[String, BoundValue] = {
    val searchRoots = includePath.librarySearchRoots

    val library = IncludeLoader(searchRoots, filename) match {
      case Some(IncludeLoadResult(libraryIncludePath, datum :: Nil)) =>
        ExtractLibrary(datum)(this, libraryIncludePath)

      case Some(IncludeLoadResult(_, data)) =>
        throw new BadSpecialFormException("Multiple top-level data in library file: " + filename)

      case None =>
        throw new LibraryNotFoundException(filename)
    }

    exprBuffer ++= library.expressions

    if (library.name != libraryName) {
      throw new LibraryNameMismatchException(libraryName, library.name)
    }
    else {
      library.exports
    }
  }

  private def loadLibraryFileOnce(filename : String, libraryName : Seq[LibraryNameComponent])(implicit includePath : IncludePath) : Map[String, BoundValue] = {
    loadedFiles.getOrElse(filename, {
      val newBindings = loadLibraryFile(filename, libraryName)
      loadedFiles += (filename -> newBindings)
      newBindings
    })
  }

  def load(libraryName : Seq[LibraryNameComponent])(implicit includePath : IncludePath) : Map[String, BoundValue] = {
    libraryName match {
      case StringComponent("llambda") :: StringComponent("primitives") :: Nil =>
        SchemePrimitives.bindings
      
      case StringComponent("llambda") :: StringComponent("nfi") :: Nil =>
        NativeFunctionPrimitives.bindings

      case StringComponent("llambda") :: StringComponent("internal") :: Nil =>
        InternalPrimitives.bindings

      case fileComponents =>
        val filename = (libraryName map {
          case StringComponent(str) => 
            // These are reserved characters for POSIX paths
            if (str.contains('\0') || str.contains('/')) {
              throw new DubiousLibraryNameComponentException(str)
            }
            else {
              str
            }
          case IntegerComponent(int) => 
            int.toString
        }).mkString("/") + ".scm"

        loadLibraryFileOnce(filename, libraryName)
    }
  }

  def loadSchemeBase =
    load(List("scheme", "base").map(StringComponent(_)))(IncludePath())

  def libraryExpressions : List[et.Expression] = 
    exprBuffer.toList
}
