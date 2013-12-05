package llambda.frontend

import llambda._

class LibraryLoader(targetPlatform : platform.TargetPlatform) {
  private val exprBuffer = collection.mutable.ListBuffer[et.Expression]()
  private val loadedFiles = collection.mutable.Map.empty[String, Map[String, BoundValue]]

  private def loadLibraryFile(filename : String, libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated)(implicit includePath : IncludePath) : Map[String, BoundValue] = {
    val searchRoots = includePath.librarySearchRoots

    val library = IncludeLoader(searchRoots, filename) match {
      case Some(IncludeLoadResult(libraryIncludePath, datum :: Nil)) =>
        ExtractLibrary(datum, Some(libraryName))(this, libraryIncludePath)

      case Some(IncludeLoadResult(_, data)) =>
        throw new BadSpecialFormException(loadLocation, "Multiple top-level data in library file: " + filename)

      case None =>
        throw new LibraryNotFoundException(loadLocation, filename)
    }

    exprBuffer ++= library.expressions

    library.exports
  }

  private def loadLibraryFileOnce(filename : String, libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated)(implicit includePath : IncludePath) : Map[String, BoundValue] = {
    loadedFiles.getOrElse(filename, {
      val newBindings = loadLibraryFile(filename, libraryName, loadLocation)
      loadedFiles += (filename -> newBindings)
      newBindings
    })
  }

  def load(libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated = NoSourceLocation)(implicit includePath : IncludePath) : Map[String, BoundValue] = {
    libraryName match {
      case StringComponent("llambda") :: StringComponent("primitives") :: Nil =>
        SchemePrimitives.bindings
      
      case StringComponent("llambda") :: StringComponent("nfi") :: Nil =>
        // Our NFI types depend on our target platform
        NativeFunctionPrimitives.bindings ++
          IntrinsicTypes(targetPlatform).mapValues(BoundType.apply)

      case StringComponent("llambda") :: StringComponent("internal") :: Nil =>
        InternalPrimitives.bindings

      case fileComponents =>
        val filename = (libraryName map {
          case StringComponent(str) => 
            // These are reserved characters for POSIX paths
            if (str.contains('\0') || str.contains('/')) {
              throw new DubiousLibraryNameComponentException(loadLocation, str)
            }
            else {
              str
            }
          case IntegerComponent(int) => 
            int.toString
        }).mkString("/") + ".scm"

        loadLibraryFileOnce(filename, libraryName, loadLocation)
    }
  }

  def loadSchemeBase =
    load(List("scheme", "base").map(StringComponent(_)), NoSourceLocation)(IncludePath())

  def libraryExpressions : List[et.Expression] = 
    exprBuffer.toList
}
