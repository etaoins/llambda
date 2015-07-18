package io.llambda.llvmir

object IrFunction {
  sealed abstract class FunctionAttribute(val toIr : String) extends Irable

  case object Cold extends FunctionAttribute("cold")
  case object NoReturn extends FunctionAttribute("noreturn")
  case object NoUnwind extends FunctionAttribute("nounwind")
  case object ReadNone extends FunctionAttribute("readnone")
  case object ReadOnly extends FunctionAttribute("readonly")

  sealed abstract class ParameterAttribute(val toIr : String) extends Irable

  case object ZeroExt extends ParameterAttribute("zeroext")
  case object SignExt extends ParameterAttribute("signext")
  case object NoAlias extends ParameterAttribute("noalias")
  case object NoCapture extends ParameterAttribute("nocapture")
  case object NonNull extends ParameterAttribute("nonnull")
  case class Dereferenceable(bytes : Int) extends ParameterAttribute(s"dereferenceable(${bytes})")

  case class Result(irType : ReturnableType, attributes : Set[ParameterAttribute] = Set()) {
    def toIr : String = {
      (attributes.map(_.toIr).toList.sorted ++ (irType.toIr :: Nil)).mkString(" ")
    }
  }

  case class Argument(irType : FirstClassType, attributes : Set[ParameterAttribute] = Set()) {
    def toIr : String = {
      (irType.toIr :: attributes.map(_.toIr).toList.sorted).mkString(" ")
    }
  }
}

sealed abstract trait IrSignatureLike {
  val callingConv : CallingConv
  val result : IrFunction.Result
  val arguments : List[IrFunction.Argument]
  val hasVararg : Boolean
  val attributes : Set[IrFunction.FunctionAttribute]

  def irType = FunctionType(result.irType, arguments.map(_.irType), hasVararg)
}

case class IrSignature(
  result : IrFunction.Result,
  arguments : List[IrFunction.Argument],
  hasVararg : Boolean = false,
  attributes : Set[IrFunction.FunctionAttribute] = Set(),
  callingConv : CallingConv = CallingConv.Default
) extends IrSignatureLike

sealed abstract trait IrFunctionDeclLike extends Irable with IrSignatureLike with IrNamedGlobal {
  val linkage : Linkage
  val visibility : Visibility
  val name : String
  val unnamedAddr : Boolean
  val gc : Option[String]

  protected def fixedArgIr : List[String]

  protected def irDecl : String = {
    val escapedName = EscapeIdentifier(name)
    
    val irArgParts = fixedArgIr ++ (if (hasVararg) {
      // Add ... at the end of the arg list to indicate varargs
      List("...")
    }
    else {
      Nil
    })

    val irArgList = irArgParts.mkString(", ")
    
    val declParts = List(linkage, visibility, callingConv).flatMap(_.toOptIr) ++
                    List(s"${result.toIr} @${escapedName}(${irArgList})") ++
                    (unnamedAddr match {
                      case true => List("unnamed_addr")
                      case false => Nil
                    }) ++
                    attributes.map(_.toIr).toList.sorted ++
                    gc.map("gc \"" + _ + "\"").toList

    declParts.mkString(" ")
  }

  def irValue : GlobalVariable = {
    GlobalVariable(name, PointerType(irType))
  }
}

case class IrFunctionDecl(
  result : IrFunction.Result,
  name : String,
  arguments : List[IrFunction.Argument],
  hasVararg : Boolean = false,
  attributes : Set[IrFunction.FunctionAttribute] = Set(),
  linkage : Linkage = Linkage.Default,
  visibility : Visibility = Visibility.Default,
  callingConv : CallingConv = CallingConv.Default,
  unnamedAddr : Boolean = false,
  gc : Option[String] = None
) extends IrFunctionDeclLike {
  protected def fixedArgIr : List[String] =
    arguments.map(_.toIr)

  def toIr = "declare " + irDecl
}

class IrFunctionBuilder(
  val module : IrModuleBuilder,
  val result : IrFunction.Result,
  val name : String,
  val namedArguments : List[(String, IrFunction.Argument)],
  val hasVararg : Boolean = false,
  val attributes : Set[IrFunction.FunctionAttribute] = Set(),
  val linkage : Linkage = Linkage.Default,
  val visibility : Visibility = Visibility.Default,
  val callingConv : CallingConv = CallingConv.Default,
  val unnamedAddr : Boolean = false,
  val gc : Option[String] = None
) extends IrFunctionDeclLike {
  // This generates names for the function body
  val nameSource = new LocalNameSource

  private val childBlocks = new collection.mutable.ListBuffer[IrChildBlockBuilder]
  private val metadataStack = new collection.mutable.Stack[Map[String, Metadata ]]

  private[llvmir] def activeMetadata : Map[String, Metadata] = {
    metadataStack.toList.reverse.foldLeft(Map[String, Metadata]()) { (prevMetadata, newMetadata) =>
      prevMetadata ++ newMetadata
    }
  }

  // This is needed for IrSignatureLike
  val arguments = namedArguments.map(_._2)
  
  val argumentValues = (namedArguments map { case (argName, argument) =>
    (argName -> LocalVariable(argName, argument.irType))
  }).toMap

  val entryBlock = new IrEntryBlockBuilder(this, nameSource)

  protected def fixedArgIr : List[String] = {
    namedArguments map { case(argName, argument) =>
      argument.toIr + " %" + EscapeIdentifier(argName)
    }
  }

  def toIr : String = {
    val blocks = entryBlock :: childBlocks.toList
    val blocksIr = blocks.map(_.toIr).mkString("\n")

    "define " + irDecl + " {\n" +
    blocksIr + "\n" +
    "}"
  }
  
  def startChildBlock(baseName : String) : IrChildBlockBuilder = {
    val label = nameSource.allocate(baseName)
    val block = new IrChildBlockBuilder(this, nameSource, label)

    childBlocks += block

    block
  }

  /** Pushes metadata on the active metadata stack and executes the passed block
    *
    * This will implicitly attach the passed metadata to every instruction emitted in this function for the duration of
    * the block. This is useful for emitting source location debug metadata that may have to span many instructions. If
    * any metadata of the same name was already passed to an outer withMetadata() invokation it will be overridden for
    * the duration of the block and restored once the block completes 
    */
  def withMetadata[T](metadata : Map[String, Metadata])(block : => T) : T = {
    metadataStack.push(metadata)

    try {
      block
    }
    finally {
      metadataStack.pop()
    }
  }
}
