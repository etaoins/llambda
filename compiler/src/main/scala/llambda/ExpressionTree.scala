package llambda.et
import llambda._

sealed abstract class Expression 

trait IndentedSubexpr {
  def indentSubexpr(subexpr : Expression) = {
    (subexpr.toString.split("\n").map { subexprLine =>
      "  " + subexprLine
    }).mkString("\n")
  }
}

case class Application(name : String, operands : List[Expression]) extends Expression {
  override def toString = name + "(" + operands.map(_.toString).mkString(", ") + ")"
}

case class VarReference(varName : String) extends Expression {
  override def toString = varName
}

case class SetVar(varName : String, value : Expression) extends Expression {
  override def toString = varName + " = " + value.toString
}

case class Literal(value : ast.Datum) extends Expression {
  override def toString = "'" + value.toString
}

case class Conditional(test : Expression, trueExpr : Expression, falseExpr : Option[Expression]) extends Expression with IndentedSubexpr {
  override def toString = {
    "if " + test + "\n" +
    "  " + trueExpr +
    falseExpr.map("\nelse\n  " + _).getOrElse("")
  }
}

case class Procedure(fixedArgs : List[String], restArg : Option[String], body : List[Expression]) extends Expression with IndentedSubexpr {
  private def signatureString = 
    "(" + (fixedArgs.map(_.toString) ++ restArg.map(_.toString + "...").toList).mkString(" ") + ")"

  override def toString = 
    signatureString + " ->\n" + (body.map { subexpr =>
      indentSubexpr(subexpr)
    }).mkString("\n")
}
