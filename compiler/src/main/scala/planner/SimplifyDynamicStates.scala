package io.llambda.compiler.planner
import io.llambda

import llambda.compiler.{StorageLocation, StdlibProcedure}
import llambda.compiler.et
import llambda.compiler.{valuetype => vt}


/** Simplifies the usage of dynamic states and parameters
  *
  * This performs three main simplifications:
  *
  * 1. Parameters that are completely unused or only parameterized are removed
  * 2. Parameters that are only applied and never parameterized are replaced with their value
  * 3. Empty parameterizations are removed
  *
  * There are further optimisations in the planner for tracking parameter values via `KnownParamterProc`. These can be
  * more effective as they're performed after value propagation, dead code elimination, etc. However, they are purely
  * intra-procedural optimisations while this pass works at a full program level. In particular it can replace usages of
  * (current-input-port) and (current-output-port) with their values if they're never parameterized.
  */
private[planner] object SimplifyDynamicStates {
  private sealed abstract class ParameterUsage
  private object ParameterUsage {
    /** The parameter is completely unused */
    case object Unused extends ParameterUsage
    /** The parameter is only parameterized and its value is never taken */
    case object ParameterizedOnly extends ParameterUsage
    /** The parameter only has it value taken through application */
    case object AppliedOnly extends ParameterUsage
    /** The parameter has any other usage */
    case object NonTrivial extends ParameterUsage
  }

  private type ParameterUsages = Map[StorageLocation, ParameterUsage]

  private case class RewriteRules(
    removeLocs: Set[StorageLocation] = Set(),
    lowerLocToValue: Map[StorageLocation, StorageLocation] = Map()
  )

  private sealed abstract class RewriteBindingResult
  private case class RewrittenBinding(binding: et.Binding) extends RewriteBindingResult
  private case class RewrittenExpr(expr: et.Expr) extends RewriteBindingResult

  private def findBindingParameterUsage(binding: et.Binding, acc: ParameterUsages): ParameterUsages = binding match {
    case et.Binding(paramLoc, et.Apply(et.VarRef(makeParameterLoc: StdlibProcedure), List(initialValueExpr)))
        if makeParameterLoc.stdlibName == "make-parameter" =>
      val usage = if (vt.SatisfiesType(paramLoc.schemeType, vt.TopProcedureType) == Some(true)) {
        ParameterUsage.Unused
      }
      else {
        // This could fail typechecking
        ParameterUsage.NonTrivial
      }

      // This ordering is important because it's possible to have recursive parameter definitions
      val accWithParam = acc + (paramLoc -> usage)
      findExprParameterUsage(initialValueExpr, accWithParam)

    case et.Binding(_, valueExpr) =>
      findExprParameterUsage(valueExpr, acc)
  }

  private def findExprParameterUsage(expr: et.Expr, acc: ParameterUsages): ParameterUsages = expr match {
    case et.TopLevelDefine(binding) =>
      findBindingParameterUsage(binding, acc)

    case et.InternalDefine(bindings, bodyExpr) =>
      val postBindingsAcc = bindings.foldLeft(acc) { case (previousAcc, binding) =>
        findBindingParameterUsage(binding, previousAcc)
      }

      findExprParameterUsage(bodyExpr, postBindingsAcc)

    case et.Parameterize(parameterValues, bodyExpr) =>
      val postParamsAcc = parameterValues.foldLeft(acc) { case (previousAcc, (paramExpr, valueExpr)) =>
        val accWithParam = paramExpr match {
          case et.VarRef(paramLoc) =>
            previousAcc.get(paramLoc) match {
              case Some(ParameterUsage.Unused | ParameterUsage.ParameterizedOnly) =>
                previousAcc + (paramLoc -> ParameterUsage.ParameterizedOnly)

              case Some(_) =>
                previousAcc + (paramLoc -> ParameterUsage.NonTrivial)

              case None =>
                previousAcc
            }

          case other =>
            findExprParameterUsage(other, previousAcc)
        }

        findExprParameterUsage(valueExpr, accWithParam)
      }

      findExprParameterUsage(bodyExpr, postParamsAcc)

    case et.Apply(et.VarRef(paramLoc), Nil) =>
      acc.get(paramLoc) match {
        case Some(ParameterUsage.Unused | ParameterUsage.AppliedOnly) =>
          acc + (paramLoc -> ParameterUsage.AppliedOnly)

        case Some(_) =>
          acc + (paramLoc -> ParameterUsage.NonTrivial)

        case None =>
          acc
      }

    case et.MutateVar(storageLoc, valueExpr) =>
      val newAcc = findExprParameterUsage(valueExpr, acc)

      newAcc.get(storageLoc) match {
        case Some(_) => newAcc + (storageLoc -> ParameterUsage.NonTrivial)
        case None =>    newAcc
      }

    case et.VarRef(storageLoc) =>
      acc.get(storageLoc) match {
        case Some(_) => acc + (storageLoc -> ParameterUsage.NonTrivial)
        case None =>    acc
      }

    case other =>
      // Iterate over our subexprs
      other.subexprs.foldLeft(acc) { case (previousAcc, expr) =>
        findExprParameterUsage(expr, previousAcc)
      }
  }

  private def rewriteBinding(rewriteRules: RewriteRules)(binding: et.Binding): RewriteBindingResult = binding match {
    case et.Binding(paramLoc, et.Apply(et.VarRef(makeParameterLoc: StdlibProcedure), List(initialValueExpr)))
        if makeParameterLoc.stdlibName == "make-parameter" =>
      if (rewriteRules.removeLocs.contains(paramLoc)) {
        // We can remove the binding completely but we must keep the initialiser for any side effects
        RewrittenExpr(rewriteExpr(rewriteRules)(initialValueExpr))
      }
      else {
        rewriteRules.lowerLocToValue.get(paramLoc) match {
          case Some(valueLoc) =>
            // Bind directly to value location
            RewrittenBinding(et.Binding(valueLoc, rewriteExpr(rewriteRules)(initialValueExpr)))

          case None =>
            // Nothing to rewrite
            RewrittenBinding(binding)
        }
      }

    case et.Binding(storageLoc, initialiserExpr) =>
      val rewrittenInitialiserExpr = rewriteExpr(rewriteRules)(initialiserExpr)
      RewrittenBinding(et.Binding(storageLoc, rewrittenInitialiserExpr))
  }

  private def rewriteExpr(rewriteRules: RewriteRules)(expr: et.Expr): et.Expr = expr match {
    case et.Apply(et.Apply(et.VarRef(makeParameterLoc: StdlibProcedure), List(valueExpr)), Nil)
        if makeParameterLoc.stdlibName == "make-parameter" =>
      rewriteExpr(rewriteRules)(valueExpr)

    case et.TopLevelDefine(binding) =>
      rewriteBinding(rewriteRules)(binding) match {
        case RewrittenBinding(newBinding) =>
          et.TopLevelDefine(newBinding).assignLocationFrom(expr)

        case RewrittenExpr(expr) =>
          expr
      }

    case et.InternalDefine(bindings, bodyExpr) =>
      val rewrittenBindings = bindings.map(rewriteBinding(rewriteRules)_)

      val newExprs = rewrittenBindings.collect {
        case RewrittenExpr(expr) => expr
      }

      val newBindings = rewrittenBindings.collect {
        case RewrittenBinding(binding) => binding
      }

      val rewrittenBodyExpr = rewriteExpr(rewriteRules)(bodyExpr)

      if (newBindings.isEmpty) {
        et.Expr.fromSequence(newExprs ++ rewrittenBodyExpr.toSequence)
      }
      else {
        et.Expr.fromSequence(
          newExprs :+ et.InternalDefine(newBindings, rewrittenBodyExpr).assignLocationFrom(expr)
        )
      }

    case et.Parameterize(parameterValues, bodyExpr) =>
      val (unusedParams, keepParams) = parameterValues.partition {
        case (et.VarRef(unusedLoc), _) if rewriteRules.removeLocs.contains(unusedLoc) =>
          true

        case _ =>
          false
      }

      // This must not be parameterized but we need to preserve its value expr for its side effects
      val newExprs = unusedParams.map { case (_, valueExpr) =>
        rewriteExpr(rewriteRules)(valueExpr)
      }

      val newParams = keepParams.map { case (paramExpr, valueExpr) =>
        (rewriteExpr(rewriteRules)(paramExpr), rewriteExpr(rewriteRules)(valueExpr))
      }

      val rewrittenBodyExpr = rewriteExpr(rewriteRules)(bodyExpr)

      if (newParams.isEmpty) {
        et.Expr.fromSequence(newExprs ++ rewrittenBodyExpr.toSequence)
      }
      else {
        et.Expr.fromSequence(
          newExprs :+ et.Parameterize(newParams, rewrittenBodyExpr).assignLocationFrom(expr)
        )
      }

    case et.Apply(et.VarRef(paramLoc: StorageLocation), Nil) =>
      rewriteRules.lowerLocToValue.get(paramLoc) match {
        case Some(valueLoc) =>
          // This must be replaced with a reference to rewritten value
          et.VarRef(valueLoc).assignLocationFrom(expr)

        case None =>
          expr.map(rewriteExpr(rewriteRules)_)
      }


    case other =>
      other.map(rewriteExpr(rewriteRules)_)
  }

  def apply(exprs: List[et.Expr]): List[et.Expr] = {
    val parameterUsages = exprs.foldLeft(Map[StorageLocation, ParameterUsage]()) { case (previousAcc, expr) =>
      findExprParameterUsage(expr, previousAcc)
    }

    val rewriteRules = parameterUsages.foldLeft(RewriteRules()) {
      case (previousRules, (paramLoc, ParameterUsage.Unused | ParameterUsage.ParameterizedOnly)) =>
        previousRules.copy(
          removeLocs=previousRules.removeLocs + paramLoc
        )

      case (previousRules, (paramLoc, ParameterUsage.AppliedOnly)) =>
        val valueLoc = new StorageLocation(s"${paramLoc.sourceName}-value")
        previousRules.copy(
          lowerLocToValue=previousRules.lowerLocToValue + (paramLoc -> valueLoc)
        )

      case (previousRules, (paramLoc, ParameterUsage.NonTrivial)) =>
        previousRules
    }

    exprs.map(rewriteExpr(rewriteRules)_)
  }
}
