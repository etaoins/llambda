package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

class LibraryLoader(targetPlatform : platform.TargetPlatform) {
  private val exprBuffer = collection.mutable.ListBuffer[et.Expression]()
  private val loadedFiles = collection.mutable.Map.empty[String, Map[String, BoundValue]]
  private var featuresStorageLoc : Option[StorageLocation] = None

  private def loadLibraryFile(filename : String, libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated)(implicit frontendConfig : FrontendConfig) : Map[String, BoundValue] = {
    implicit val includePath = frontendConfig.includePath
    val searchRoots = includePath.librarySearchRoots

    val library = IncludeLoader(searchRoots, filename) match {
      case Some(IncludeLoadResult(libraryIncludePath, datum :: Nil)) =>
        val libraryFrontendConfig = frontendConfig.copy(
          includePath=libraryIncludePath
        )

        ExtractLibrary(datum, Some(libraryName))(this, libraryFrontendConfig)

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

  private def builtinLibraryBindings(libraryName : Seq[LibraryNameComponent])(implicit frontendConfig : FrontendConfig) : Option[Map[String, BoundValue]] = libraryName match {
    case List(StringComponent("llambda"), StringComponent("internal"), StringComponent("primitives")) =>
      Some(PrimitiveExpressions.bindings)
    
    case List(StringComponent("llambda"), StringComponent("internal"), StringComponent("features")) =>
      if (!featuresStorageLoc.isDefined) {
        // Create this on demand
        val storageLoc = new StorageLocation("features")
        exprBuffer += et.TopLevelDefinition(List(storageLoc -> FeaturesProcedure()))

        featuresStorageLoc = Some(storageLoc)
      }

      Some(
        Map("features" -> featuresStorageLoc.get)
      )
    
    case List(StringComponent("llambda"), StringComponent("nfi")) =>
      // Our NFI types depend on our target platform
      Some(
        IntrinsicTypes(targetPlatform).mapValues(BoundType.apply) +
          ("world-function" -> PrimitiveExpressions.WorldFunction) +
          ("native-function" -> PrimitiveExpressions.NativeFunction)
      )

    case _ =>
      None
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
    builtinLibraryBindings(libraryName) getOrElse {
      // Load this as a file
      val filename = filenameForLibrary(libraryName, loadLocation)
      loadLibraryFileOnce(filename, libraryName, loadLocation)
    }

  def exists(libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated = NoSourceLocation)(implicit frontendConfig : FrontendConfig) : Boolean = {
    implicit val includePath = frontendConfig.includePath

    if (builtinLibraryBindings(libraryName).isDefined) {
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
