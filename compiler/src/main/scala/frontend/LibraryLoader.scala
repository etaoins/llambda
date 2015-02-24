package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.{celltype => ct}

import llambda.compiler.valuetype.Implicits._

class LibraryLoader(targetPlatform : platform.TargetPlatform) {
  private val exprBuffer = collection.mutable.ListBuffer[et.Expr]()
  private val loadedFiles = collection.mutable.Map.empty[String, Map[String, BoundValue]]
  private var featuresStorageLoc : Option[StorageLocation] = None

  private def loadLibraryFile(filename : String, libraryName : Seq[LibraryNameComponent], loadLocation : SourceLocated)(implicit frontendConfig : FrontendConfig) : Map[String, BoundValue] = {
    implicit val includePath = frontendConfig.includePath
    val searchRoots = includePath.librarySearchRoots

    val library = IncludeLoader(searchRoots, filename) match {
      case Some(IncludeLoadResult(filename, libraryIncludePath, List(datum))) =>
        val libraryFrontendConfig = frontendConfig.copy(
          includePath=libraryIncludePath
        )

        ExtractLibrary(Some(filename), datum, Some(libraryName))(this, libraryFrontendConfig)

      case Some(IncludeLoadResult(_, _, data)) =>
        throw new BadSpecialFormException(loadLocation, "Multiple top-level data in library file: " + filename)

      case None =>
        throw new LibraryNotFoundException(loadLocation, filename)
    }

    exprBuffer ++= library.exprs

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
      Some(Primitives.bindings)
    
    case List(StringComponent("llambda"), StringComponent("internal"), StringComponent("features")) =>
      if (!featuresStorageLoc.isDefined) {
        // Create this on demand
        val storageLoc = new ReportProcedure("features")
        exprBuffer += et.TopLevelDefine(List(et.SingleBinding(storageLoc, FeaturesProcedure(frontendConfig))))

        featuresStorageLoc = Some(storageLoc)
      }

      Some(
        Map("features" -> featuresStorageLoc.get)
      )
    
    case List(StringComponent("llambda"), StringComponent("nfi")) =>
      Some(
        // Our NFI types depend on our target platform
        IntrinsicTypes(targetPlatform).mapValues(BoundType.apply) +
          ("<list>" -> BoundType(vt.UniformProperListType(vt.AnySchemeType))) +
          ("<pair>" -> BoundType(vt.AnyPairType)) +
          ("<procedure>" -> BoundType(vt.SchemeTypeAtom(ct.ProcedureCell))) +
          ("<vector>" -> BoundType(vt.VectorOfType(vt.AnySchemeType))) +
          ("->" -> Primitives.ProcedureType) +
          ("define-native-library" -> Primitives.DefineNativeLibrary) +
          ("static-library" -> Primitives.StaticLibrary) +
          ("native-function" -> Primitives.NativeFunction) +
          ("world-function" -> Primitives.WorldFunction) +
          ("noreturn" -> Primitives.NoReturnAttr) +
          ("system-library" -> NativeSystemLibrary) +
          ("All" -> Primitives.PolymorphicType)
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
        // 1) This is potentially slow and we'll need to do it again at include time
        // 2) It seems more natural for any parse etc. error to be reported at (import) time than (cond-expand) time
        IncludeLoader(includePath.librarySearchRoots, filename).isDefined
      }
    }
  }

  def loadSchemeBase(implicit frontendConfig : FrontendConfig) =
    load(List("scheme", "base").map(StringComponent(_)), NoSourceLocation)

  def libraryExprs : List[et.Expr] = 
    exprBuffer.toList
}
