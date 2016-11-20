package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.et
import llambda.compiler.sst
import llambda.compiler.{valuetype => vt}
import llambda.compiler._
import llambda.compiler.frontend.syntax.ExpandMacro

import annotation.tailrec

private object FindBodyDefines {
  case class Result(extractedDefines : List[ExtractedVarsDefine], bodyData : List[sst.ScopedDatum])

  /** Splits a body in to definitions and body expressions
    *
    * This will extract definitions from the body until a non-define is encountered. It will then return all parsed
    * defines and the remaining data.
    */
  def apply(
      data : List[sst.ScopedDatum],
      definesAcc : List[ExtractedVarsDefine] = Nil
  )(implicit context : FrontendContext) : Result = data match {
    case (pairDatum @ sst.ScopedPair(appliedSymbol : sst.ScopedSymbol, cdr)) :: restData =>
      (appliedSymbol.resolveOpt, cdr) match {
        case (Some(syntax : BoundSyntax), _) =>
          // This is a macro - expand it
          val expandedDatum = ExpandMacro(syntax, cdr, pairDatum, trace=context.config.traceMacroExpansion)
          apply(expandedDatum :: restData, definesAcc)

        case (Some(Primitives.Begin), sst.ScopedProperList(innerExprData)) =>
          // This is a (begin) - flatten it
          apply(innerExprData ++ restData, definesAcc)

        case (Some(Primitives.Include), sst.ScopedProperList(includeNameData)) =>
          val unscopedIncludeNames = includeNameData.map(_.unscope)
          val includeData = ResolveIncludeList(appliedSymbol, unscopedIncludeNames)(context.config.includePath)

          val scopedData = includeData.map(sst.ScopedDatum(appliedSymbol.scope, _))
          apply(scopedData ++ restData, definesAcc)

        case (Some(definePrimitive : PrimitiveDefineExpr), sst.ScopedProperList(operands)) =>
          val extractedDefines = ExtractDefine(pairDatum, definePrimitive, operands)
          apply(restData, extractedDefines ++ definesAcc)

        case _ =>
          // Not a define
          Result(definesAcc.reverse, data)
      }

    case otherData =>
      // Not a define
      Result(definesAcc.reverse, otherData)
  }
}

private[frontend] object ExtractBodyDefinition {
  /** Extracts the definition of a body with the given arguments
    *
    * The primary type of body definition in Scheme occurs in a procedure. However, these also occuring in other
    * contexts such as inside (parameterize).
    *
    * @param  args        Mapping of argument symbols to their bound value
    * @param  definition  Definition of the body. This will be split in to its bindings and body expressions. The
    *                     bindings will be applied to a new scope and then the body expressions will be extracted in
    *                     that scope.
    * @return Expression representing the body
    */
  def apply(
      args : List[(sst.ScopedSymbol, BoundValue)],
      definition : List[sst.ScopedDatum]
  )(implicit context : FrontendContext) : et.Expr = {
    // Find all the scopes in the definition
    val definitionScopes = definition.foldLeft(Set[Scope]()) { (scopes, datum) =>
      scopes ++ UniqueScopesForDatum(datum)
    }

    // Introduce new scopes with our arguments injected in to them
    val argsForScope = args groupBy(_._1.scope)

    val scopeMapping = (definitionScopes map { outerScope =>
      val scopeArgs = argsForScope.getOrElse(outerScope, List())

      // Check for duplicate args within this scope
      scopeArgs.foldLeft(Set[String]()) { case (names, (symbol, _)) =>
        val name = symbol.name

        if (names.contains(name)) {
          throw new BadSpecialFormException(symbol, "Duplicate formal parameter: " + name)
        }

        names + name
      }

      val binding = collection.mutable.Map(scopeArgs.map { case (symbol, boundValue) =>
        (symbol.name -> boundValue)
      } : _*) : collection.mutable.Map[String, BoundValue]

      val innerScope = new Scope(binding, Some(outerScope))

      (outerScope -> innerScope)
    }).toMap

    // Rescope our definition
    val scopedDefinition = definition.map(_.rescoped(scopeMapping))

    // Split our definition is to (define)s and a body
    val foundDefines = FindBodyDefines(scopedDefinition)

    // Expand our scopes with all of the defines
    val bindingBlocks = foundDefines.extractedDefines flatMap {
      case ExtractedVarsDefine(fixedValueTargets, restValueTargetOpt, exprBlock) =>
        val fixedLocs = fixedValueTargets.map(_.bindStorageLoc(vt.AnySchemeType))
        val restLocOpt = restValueTargetOpt.map(_.bindStorageLoc(vt.UniformProperListType(vt.AnySchemeType)))

        List(
          { () => et.Binding(fixedLocs, restLocOpt, exprBlock()) }
        )
    } : List[() => et.Binding]

    // Execute the expression blocks now that the scopes are prepared
    val bindings = bindingBlocks.map(_.apply)

    // Find the expressions in our body
    val bodyExpr = et.Expr.fromSequence(
      foundDefines.bodyData.map(ExtractExpr(_))
    )

    // Finish the inner scopes
    for((_, innerScope) <- scopeMapping) {
      FinishScope(innerScope)
    }

    bindings match {
      case Nil => bodyExpr
      case _ => et.InternalDefine(bindings, bodyExpr)
    }
  }

}
