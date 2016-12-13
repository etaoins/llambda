package io.llambda.compiler.valuetype.polymorphic
import io.llambda

import llambda.compiler.valuetype._
import llambda.compiler.SourceLocated

case class PolymorphicProcedureType(typeVars: Set[TypeVar], template: ProcedureType) {
  private def instantiate(reconciled: ReconcileTypeVars.Result): ProcedureType = {
    template.copy(
      mandatoryArgTypes=template.mandatoryArgTypes.map(InstantiateType(reconciled, _)),
      optionalArgTypes=template.optionalArgTypes.map(InstantiateType(reconciled, _)),
      restArgMemberTypeOpt=template.restArgMemberTypeOpt.map(InstantiateType(reconciled, _)),
      returnType=InstantiateType.instantiateReturnType(reconciled, template.returnType)
    )
  }

  def typeForArgs(args: List[SchemeType]): ProcedureType = {
    if (typeVars.isEmpty) {
      // Skip!
      return template
    }

    val fixedArgTypes = template.mandatoryArgTypes ++ template.optionalArgTypes

    val fixedArgResults = (fixedArgTypes zip args) map { case (polyArg, evidenceArg) =>
      ResolveTypeVars(typeVars, polyArg, evidenceArg)
    }

    val restArgResults = template.restArgMemberTypeOpt match {
      case Some(polyMemberType) =>
        args.drop(fixedArgTypes.length) map { evidenceMemberType =>
          ResolveTypeVars(typeVars, polyMemberType, evidenceMemberType)
        }

      case None =>
        Nil
    }

    val allResults = fixedArgResults ++ restArgResults
    val reducedResult = allResults.foldLeft(ResolveTypeVars.Result())(_ ++ _)

    // Reconcile our type vars with their upper bounds
    val reconciled = ReconcileTypeVars(typeVars, reducedResult)
    instantiate(reconciled)
  }

  lazy val upperBound: ProcedureType = {
    // Use our upper type bounds for everything
    val reconciled = ReconcileTypeVars(typeVars)
    instantiate(reconciled)
  }
}
