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

  protected def irDecl : String = {
    val argList = arguments.map(_.toIr).mkString(", ")

    val declParts = List(linkage, visibility, callingConv).flatMap(_.toOptIr) ++
                    (unnamedAddr match {
                      case true => List("unnamed_addr")
                      case false => Nil
                    }) ++
                    List(s"${result.toIr} @${name}(${argList})") ++
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
  def toIr = "declare " + irDecl
}

abstract class IrFunctionDef(
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
  protected implicit val nameSource = new LocalNameSource

  // This is needed for IrCallableLike
  val arguments = namedArguments.map(_._2)

  private val blocks = new collection.mutable.ListBuffer[(String, IrBlock)]
  protected val argumentValues = namedArguments

  final def declareBlock(name : String) : IrLabel = {
    IrLabel(nameSource.allocate(name))
  }

  final def defineBlock(label : IrLabel)(block : IrBlock) {
    blocks.append((label.name, block))
  }

  final def addBlock(name : String)(block : IrBlock) : IrLabel = {
    val label = declareBlock(name)
    defineBlock(label)(block)
    label
  }

  final def toIr : String = {
    val blocksIr = blocks.map { case (name, block) =>
      s"${name}:\n" + block.toIr
    } mkString("\n")

    "define " + irDecl + " {\n" +
    blocksIr + "\n" +
    "}"
  }
}
