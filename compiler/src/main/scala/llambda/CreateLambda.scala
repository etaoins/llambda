package llambda

object CreateLambda {
  def apply(fixedArgData : List[ast.Datum], restArgName : Option[String], body : List[ast.Datum])(implicit parentScope : Scope) : et.Procedure = {
    // Create our actual procedure arguments
    // These unique identify the argument independently of its binding at a
    // given time
    val fixedArgNames = fixedArgData.map { datum =>
      datum match {
        case ast.Symbol(name) => name
        case _ => throw new BadSpecialFormException("Symbol expected, found " + datum)
      }
    }

    val fixedArgs : List[ProcedureArg] = fixedArgNames.map { _ =>
      new ProcedureArg
    }

    val restArg : Option[ProcedureArg] = restArgName.map { _ =>
      new ProcedureArg
    }

    // Create a new scope with the args bound to their names
    val binding : Map[String, ProcedureArg] =
      ((fixedArgNames zip fixedArgs) ++ (restArgName zip restArg).toList).toMap

    val initialScope = new Scope(binding, Some(parentScope))

    // Parse the body - we can discard the scope after
    val (expressions, _) = ExtractBody(body)(initialScope)

    et.Procedure(fixedArgs, restArg, expressions)
  }
}

