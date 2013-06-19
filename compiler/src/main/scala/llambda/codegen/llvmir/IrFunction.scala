package llambda.codegen.llvmir

object IrFunction {
  sealed abstract class ParameterAttribute extends Irable

  case object ZeroExt extends ParameterAttribute {
    def toIr = "zeroext"
  }
  
  case object SignExt extends ParameterAttribute {
    def toIr = "signext"
  }
  
  case object NoAlias extends ParameterAttribute {
    def toIr = "noalias"
  }
  
  case object NoCapture extends ParameterAttribute {
    def toIr = "nocapture"
  }

  case class Result(irType : ReturnableType, attributes : Set[ParameterAttribute]) {
    def toIr : String = {
      (attributes.map(_.toIr).toList.sorted ++ (irType.toIr :: Nil)).mkString(" ")
    }
  }

  case class Argument(irType : FirstClassType, attributes : Set[ParameterAttribute]) {
    def toIr : String = {
      (irType.toIr :: attributes.map(_.toIr).toList.sorted).mkString(" ")
    }
  }
}

abstract trait IrFunction extends Irable {
  val linkage : Linkage.Linkage
  val visibility : Visibility.Visibility
  val callingConv : CallingConv.CallingConv
  val unnamedAddr : Boolean
  val result : IrFunction.Result
  val name : String
  val arguments : List[IrFunction.Argument]
  val gc : Option[String]

  def irType = FunctionType(result.irType, arguments.map(_.irType))

  protected def irDecl(attributes : List[String]) : String = {
    val argList = arguments.map(_.toIr).mkString(", ")

    val declParts = List(linkage, visibility, callingConv).flatMap(_.toOptIr) ++
                    (unnamedAddr match {
                      case true => List("unnamed_addr")
                      case false => Nil
                    }) ++
                    List(s"${result.toIr} @${name}(${argList})") ++
                    attributes ++
                    gc.map("gc \"" + _ + "\"").toList

    declParts.mkString(" ")
  }
}

case class IrFunctionDecl(
  result : IrFunction.Result,
  name : String,
  arguments: List[IrFunction.Argument],
  linkage : Linkage.Linkage = Linkage.Default,
  visibility : Visibility.Visibility = Visibility.Default,
  callingConv : CallingConv.CallingConv = CallingConv.Default,
  unnamedAddr : Boolean = false,
  gc : Option[String] = None
) extends IrFunction {
  def toIr = "declare " + irDecl(Nil)
}
