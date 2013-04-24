package llambda.et
import llambda._

sealed abstract class AbstractVar {
  val name : String
  override def toString = name
}

case class UnresolvedVar(name : String) extends AbstractVar
class ResolvedVar(val name : String) extends AbstractVar

sealed abstract class Expression[T <: AbstractVar] 

trait IndentedSubexpr {
  def indentSubexpr[T <: AbstractVar](subexpr : String) = {
    (subexpr.split("\n").map { subexprLine =>
      "  " + subexprLine
    }).mkString("\n")
  }
}

case class Application[T <: AbstractVar](function : T, operands : List[Expression[T]]) extends Expression[T] {
  override def toString = function.name + "(" + operands.map(_.toString).mkString(", ") + ")"
}

case class VarReference[T <: AbstractVar](variable : T) extends Expression[T] {
  override def toString = variable.name
}

case class SetVar[T <: AbstractVar](variable : T, value : Expression[T]) extends Expression[T] {
  override def toString = variable.name + " = " + value.toString
}

case class Literal[T <: AbstractVar](value : ast.Datum) extends Expression[T] {
  override def toString = "'" + value.toString
}

case class Conditional[T <: AbstractVar](test : Expression[T], trueExpr : Expression[T], falseExpr : Option[Expression[T]]) extends Expression[T] with IndentedSubexpr {
  override def toString = {
    "if " + test + "\n" +
    "  " + trueExpr +
    falseExpr.map("\nelse\n  " + _).getOrElse("")
  }
}

case class Procedure[T <: AbstractVar](fixedArgs : List[T], restArg : Option[T], body : List[Expression[T]]) extends Expression[T] with IndentedSubexpr {
  private def signatureString = 
    "(" + (fixedArgs.map(_.name) ++ restArg.map(_.name + "...").toList).mkString(" ") + ")"

  override def toString = 
    signatureString + " ->\n" + (body.map { subexpr =>
      indentSubexpr(subexpr.toString)
    }).mkString("\n")
}

case class SyntaxRule[T <: AbstractVar](pattern : List[ast.Datum], template : Expression[T]) extends IndentedSubexpr {
  override def toString = s"case ${pattern.mkString(", ")} =>\n" +
    indentSubexpr(template.toString)
}

case class DefineSyntax[T <: AbstractVar](keyword : T, literals : List[String], rules : List[SyntaxRule[T]]) extends Expression[T] with IndentedSubexpr {
  override def toString = s"define ${keyword} with literal (${literals.mkString(", ")})\n" +
    rules.map { rule =>
      indentSubexpr(rule.toString)
    }.mkString("\n")
}
