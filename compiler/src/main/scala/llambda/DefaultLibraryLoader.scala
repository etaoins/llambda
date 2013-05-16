package llambda

class DubiousLibraryNameComponentException(val name : String) extends SemanticException(name)
class LibraryNotFoundException(val filename : String) extends SemanticException(filename)
class LibraryNameMismatchException(val loadedName : List[LibraryNameComponent], val definedName : List[LibraryNameComponent]) extends
  Exception(loadedName.mkString(" ") + " doesn't match " + definedName.mkString(" "))

class DefaultLibraryLoader {
  private val exprBuffer = collection.mutable.ListBuffer[et.Expression]()
  private val loadedFiles = collection.mutable.Map.empty[String, Map[String, BoundValue]]

  private def loadLibraryFile(filename : String, libraryName : List[LibraryNameComponent]) : Map[String, BoundValue] = {
    // Just look at our resources for now
    val stream = getClass.getClassLoader.getResourceAsStream("libraries/" + filename)

    if (stream == null) {
      throw new LibraryNotFoundException(filename)
    }

    val libraryString = io.Source.fromInputStream(stream).mkString
    val libraryData = SchemeParser.parseStringAsData(libraryString)

    val library = libraryData match {
      case datum :: Nil => ExtractLibrary(datum)(this.load)
      case _ =>
        throw new BadSpecialFormException("Multiple top-level data in library file: " + filename)
    }

    exprBuffer ++= library.expressions

    if (library.name != libraryName) {
      throw new LibraryNameMismatchException(libraryName, library.name)
    }
    else {
      library.exports
    }
  }

  private def loadLibraryFileOnce(filename : String, libraryName : List[LibraryNameComponent]) : Map[String, BoundValue] = {
    loadedFiles.getOrElse(filename, {
      val newBindings = loadLibraryFile(filename, libraryName)
      loadedFiles += (filename -> newBindings)
      newBindings
    })
  }

  def load(libraryName : List[LibraryNameComponent]) : Map[String, BoundValue] = {
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

  def loadSchemeCore = load(StringComponent("scheme") :: StringComponent("core") :: Nil)

  def libraryExpressions : List[et.Expression] = 
    exprBuffer.toList
}
