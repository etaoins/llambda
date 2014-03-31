package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

class LibraryLoader(targetPlatform : platform.TargetPlatform) {
  private val exprBuffer = collection.mutable.ListBuffer[et.Expression]()
  private val loadedFiles = collection.mutable.Map.empty[String, Map[String, BoundValue]]

  private def loadLibraryFile(filename : String, libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated)(implicit frontendConfig : FrontendConfig) : Map[String, BoundValue] = {
    implicit val includePath = frontendConfig.includePath
    val searchRoots = includePath.librarySearchRoots

    val library = IncludeLoader(searchRoots, filename) match {
      case Some(IncludeLoadResult(libraryIncludePath, datum :: Nil)) =>
        ExtractLibrary(datum, Some(libraryName))(this, frontendConfig)

      case Some(IncludeLoadResult(_, data)) =>
        throw new BadSpecialFormException(loadLocation, "Multiple top-level data in library file: " + filename)

      case None =>
        throw new LibraryNotFoundException(loadLocation, filename)
    }

    exprBuffer ++= library.expressions

    library.exports
  }

  private def loadLibraryFileOnce(filename : String, libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated)(implicit frontendConfig : FrontendConfig) : Map[String, BoundValue] = {
    loadedFiles.getOrElse(filename, {
      val newBindings = loadLibraryFile(filename, libraryName, loadLocation)
      loadedFiles += (filename -> newBindings)
      newBindings
    })
  }

  private def builtinLibraryBindings : PartialFunction[Seq[LibraryNameComponent], Map[String, BoundValue]] = {
    case List(StringComponent("llambda"), StringComponent("internal"), StringComponent("primitives")) =>
      PrimitiveExpressions.bindings
    
    case List(StringComponent("llambda"), StringComponent("nfi")) =>
      // Our NFI types depend on our target platform
      IntrinsicTypes(targetPlatform).mapValues(BoundType.apply) +
        ("world-function" -> PrimitiveExpressions.WorldFunction) +
        ("native-function" -> PrimitiveExpressions.NativeFunction)
  }

  private def filenameForLibrary(libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated) : String = 
    (libraryName map {
      case StringComponent(str) => 
        // These are reserved characters for POSIX paths
        if (str.contains(0) || str.contains('/')) {
          throw new DubiousLibraryNameComponentException(loadLocation, str)
        }
        else {
          str
        }
      case IntegerComponent(int) => 
        int.toString
    }).mkString("/") + ".scm"

  def load(libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated = NoSourceLocation)(implicit frontendConfig : FrontendConfig) : Map[String, BoundValue] =
    if (builtinLibraryBindings.isDefinedAt(libraryName)) { 
      // This is an builtin library
      // Return the bindings directly
      builtinLibraryBindings(libraryName)
    }
    else {
      // Load this as a file
      val filename = filenameForLibrary(libraryName, loadLocation)
      loadLibraryFileOnce(filename, libraryName, loadLocation)
    }

  def exists(libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated = NoSourceLocation)(implicit frontendConfig : FrontendConfig) : Boolean = {
    implicit val includePath = frontendConfig.includePath

    if (builtinLibraryBindings.isDefinedAt(libraryName)) {
      // This is a builtin
      true
    }
    else {
      val filename = filenameForLibrary(libraryName, loadLocation)

      if (loadedFiles.contains(filename)) {
        // We've already loaded this; don't bother searching
        true
      }
      else {
        // Don't attempt to parse the result for two reasons:
        // 1) This is potentially slow and we'll need to do it again at include
        //    time
        // 2) It seems more natural for any parse etc. error to be reported at
        //    (import) time than (cond-expand) time
        IncludeLoader(includePath.librarySearchRoots, filename).isDefined
      }
    }
  }

  def loadSchemeBase(implicit frontendConfig : FrontendConfig) =
    load(List("scheme", "base").map(StringComponent(_)), NoSourceLocation)

  def libraryExpressions : List[et.Expression] = 
    exprBuffer.toList
}
