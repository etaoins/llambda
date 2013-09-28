package llambda.codegen.llvmir

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

sealed abstract trait IrCallableLike {
  val callingConv : CallingConv.CallingConv
  val result : IrFunction.Result
  val arguments : List[IrFunction.Argument]
  val attributes : Set[IrFunction.FunctionAttribute]
  
  def irType = FunctionType(result.irType, arguments.map(_.irType))
}

case class IrCallable(
  callingConv : CallingConv.CallingConv,
  result : IrFunction.Result,
  arguments : List[IrFunction.Argument],
  attributes : Set[IrFunction.FunctionAttribute]
) extends IrCallableLike

sealed abstract trait IrFunctionDeclLike extends Irable with IrCallableLike {
  val linkage : Linkage.Linkage
  val visibility : Visibility.Visibility
  val name : String
  val unnamedAddr : Boolean
  val gc : Option[String]

  protected def irArgList : String

  protected def irDecl : String = {
    val declParts = List(linkage, visibility, callingConv).flatMap(_.toOptIr) ++
                    (unnamedAddr match {
                      case true => List("unnamed_addr")
                      case false => Nil
                    }) ++
                    List(s"${result.toIr} @${name}(${irArgList})") ++
                    attributes.map(_.toIr).toList.sorted ++
                    gc.map("gc \"" + _ + "\"").toList

    declParts.mkString(" ")
  }

  def irValue : IrValue = {
    GlobalVariable(name, PointerType(irType))
  }
}

case class IrFunctionDecl(
  result : IrFunction.Result,
  name : String,
  arguments : List[IrFunction.Argument],
  attributes : Set[IrFunction.FunctionAttribute] = Set(),
  linkage : Linkage.Linkage = Linkage.Default,
  visibility : Visibility.Visibility = Visibility.Default,
  callingConv : CallingConv.CallingConv = CallingConv.Default,
  unnamedAddr : Boolean = false,
  gc : Option[String] = None
) extends IrFunctionDeclLike {
  protected def irArgList : String = arguments.map(_.toIr).mkString(", ")

  def toIr = "declare " + irDecl
}

protected class IrFunctionBuilder(
  val result : IrFunction.Result,
  val name : String,
  val namedArguments : List[(String, IrFunction.Argument)],
  val attributes : Set[IrFunction.FunctionAttribute] = Set(), 
  val linkage : Linkage.Linkage = Linkage.Default,
  val visibility : Visibility.Visibility = Visibility.Default,
  val callingConv : CallingConv.CallingConv = CallingConv.Default,
  val unnamedAddr : Boolean = false,
  val gc : Option[String] = None
) extends IrFunctionDeclLike {
  // This generates names for the function body
  private val nameSource = new LocalNameSource

  // This is needed for IrCallableLike
  val arguments = namedArguments.map(_._2)
  
  val argumentValues = (namedArguments map { case (argName, argument) =>
    (argName -> LocalVariable(argName, argument.irType))
  }).toMap

  private val blocks = new collection.mutable.ListBuffer[IrBlockBuilder]

  def startBlock(baseName : String) : IrBlockBuilder = {
    val label = nameSource.allocate(baseName)
    val block = new IrBlockBuilder(nameSource, label)

    blocks += block

    block
  }
  
  protected def irArgList : String = {
    namedArguments map { case(argName, argument) =>
      argument.toIr + " %" + argName
    } mkString(", ")
  }

  def toIr : String = {
    val blocksIr = blocks.map(_.toIr).mkString("\n")

    "define " + irDecl + " {\n" +
    blocksIr + "\n" +
    "}"
  }
}
