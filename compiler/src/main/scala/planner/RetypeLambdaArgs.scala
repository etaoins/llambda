package io.llambda.compiler.planner
  
import io.llambda

import scala.collection.breakOut

import llambda.compiler._
import llambda.compiler.{valuetype => vt}
import llambda.compiler.planner.{intermediatevalue => iv}

private[planner] object RetypeLambdaArgs {
  private case class AppliedProcData(
    fixedArgTypes : List[vt.SchemeType],
    hasRestArg : Boolean,
    canTerminate : Boolean
  ) {
    def compatibleArity(argCount : Int) =
      (argCount == fixedArgTypes.length) ||
        ((argCount > fixedArgTypes.length) && hasRestArg)
  }

  private type ArgTypes = Map[StorageLocation, vt.SchemeType]

  private class CollectionAborted(val argTypes : ArgTypes) extends Exception

  private def attributeTypeToStorageLoc(storageLoc : StorageLocation, schemeType : vt.SchemeType, argTypes : ArgTypes) : ArgTypes = {
    if (schemeType != vt.AnySchemeType) {
      argTypes.get(storageLoc) match {
        case Some(existingType) =>
          // We control this storage loc!
          argTypes + (storageLoc -> (existingType & schemeType))

        case None if vt.SatisfiesType(storageLoc.schemeType, schemeType) == Some(true) =>
          // This type conversion can't fail - continue
          argTypes

        case None =>
          // We don't control this and the type cast can fail - abort
          throw new CollectionAborted(argTypes)
      }
    }
    else {
      // Nothing to attribute
      argTypes
    }
  }

  private def attributeTypeToExpr(expr : et.Expr, schemeType : vt.SchemeType, argTypes : ArgTypes)(implicit state : PlannerState) : ArgTypes = {
    expr match {
      case et.VarRef(storageLoc) =>
        // Simple re-assignment to another location
        attributeTypeToStorageLoc(storageLoc, schemeType, argTypes)

      case otherExpr =>
        // This will abort if it encounters a terminating expression
        collectTypeEvidence(otherExpr, argTypes)
    }
  }
  
  private def collectTypeEvidence(expr : et.Expr, argTypes : ArgTypes)(implicit state : PlannerState) : ArgTypes = expr match {
    case et.Begin(exprs) =>
      exprs.foldLeft(argTypes) { case (currentArgTypes, subexpr) =>
        collectTypeEvidence(subexpr, currentArgTypes)
      }

    case et.MutateVar(mutateLoc, valueExpr) =>
      attributeTypeToExpr(valueExpr, mutateLoc.schemeType, argTypes)

    case et.InternalDefine(bindings, bodyExpr) =>
      val postBindArgTypes = bindings.foldLeft(argTypes) {
        case (currentArgTypes, et.SingleBinding(bindLoc, bindExpr)) =>
          attributeTypeToExpr(bindExpr, bindLoc.schemeType, currentArgTypes)

        case (currentArgTypes, _) =>
          // We don't support annotating multiple values
          currentArgTypes
      }

      collectTypeEvidence(bodyExpr, postBindArgTypes)

    case et.Apply(et.VarRef(procLoc), argExprs) =>
      // Do the proc first
      val knownProcOpt = state.values.get(procLoc) match {
        case Some(ImmutableValue(knownCaseLambda : iv.KnownCaseLambdaProc)) =>
          knownCaseLambda.clauseForArityOpt(argExprs.length).map(_.knownProc)

        case Some(ImmutableValue(knownProc : iv.KnownProc)) =>
          Some(knownProc)

        case _ =>
          None
      }

      for(knownProc <- knownProcOpt) {
        val signature = knownProc.polySignature.upperBound

        val postFixedArgTypes = argExprs.zip(signature.fixedArgTypes).foldLeft(argTypes) {
          case (currentArgTypes, (argExpr, argValueType)) =>
            attributeTypeToExpr(argExpr, argValueType.schemeType, currentArgTypes)
        }

        // Do we have a typed rest arg?
        val finalArgTypes = signature.restArgMemberTypeOpt match { 
          case Some(memberType) =>
            // Attribute the rest arg member type to all of the rest args
            val restArgExprs = argExprs.drop(signature.fixedArgTypes.length)

            restArgExprs.foldLeft(postFixedArgTypes) {
              case (currentArgTypes, restArgExpr) =>
                attributeTypeToExpr(restArgExpr, memberType, currentArgTypes)
            }

          case None =>
            postFixedArgTypes
        } 

        if (signature.hasWorldArg) {
          // This might terminate - abort retyping
          throw new CollectionAborted(finalArgTypes)
        }
        else {
          // Keep going!
          return finalArgTypes
        }
      }

      // Abort retyping completely
      throw new CollectionAborted(argTypes)

    case et.Cond(testExpr, trueExpr, falseExpr) =>
      val postTestArgTypes = collectTypeEvidence(testExpr, argTypes)

      val (trueAborted, trueArgTypes) = try {
        (false, collectTypeEvidence(trueExpr, postTestArgTypes))
      }
      catch {
        case aborted : CollectionAborted =>
          (true, aborted.argTypes)
      }
      
      val (falseAborted, falseArgTypes) = try {
        (false, collectTypeEvidence(falseExpr, postTestArgTypes))
      }
      catch {
        case aborted : CollectionAborted =>
          (true, aborted.argTypes)
      }

      // Now union the type argTypes from both branches
      val mergedArgTypes = trueArgTypes.keys.map({ storageLoc =>
        storageLoc -> (trueArgTypes(storageLoc) + falseArgTypes(storageLoc))
      })(breakOut) : ArgTypes

      // If either branch aborted we have to abort
      if (trueAborted || falseAborted) {
        throw new CollectionAborted(mergedArgTypes)
      }
      else {
        mergedArgTypes
      }

    case et.Cast(valueExpr, targetType, _) =>
      attributeTypeToExpr(valueExpr, targetType, argTypes)

    case _ : et.Lambda | _ : et.CaseLambda | _ : et.NativeFunction | _ : et.Literal | _ : et.ArtificialProcedure |
         _ : et.VarRef  =>
      // Ignore these
      argTypes

    case _ : et.Return | _ : et.Parameterize | _ : et.TopLevelDefine | _ : et.Apply =>
      // These can terminate (or in the case of TopLevelDefine, shouldn't exist)
      // Abort!
      throw new CollectionAborted(argTypes)
  }

  def apply(
      lambdaExpr : et.Lambda,
      procType : vt.ProcedureType
  )(implicit state : PlannerState, planConfig : PlanConfig) : ArgTypes = {
    // OPTTODO: Handle optional arguments
    val initialArgTypes = (lambdaExpr.fixedArgs zip procType.mandatoryArgTypes).filter({
      case (storageLoc, fixedArgType) =>
        !planConfig.analysis.mutableVars.contains(storageLoc)
    }).toMap

    try {
      collectTypeEvidence(
        expr=lambdaExpr.body,
        argTypes=initialArgTypes
      )
    }
    catch {
      case aborted : CollectionAborted =>
        aborted.argTypes
    }
  }
}
