package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.et
import llambda.compiler.sst
import llambda.compiler.{valuetype => vt}
import llambda.compiler._

import collection.mutable.ListBuffer

private[frontend] object ExtractBodyDefinition {
  private def parseDefineDatum(datum : sst.ScopedDatum)(implicit context : FrontendContext) : Option[ParsedDefine] =
    datum match {
      case sst.ScopedProperList((appliedSymbol : sst.ScopedSymbol) :: operands) =>
        appliedSymbol.resolveOpt match {
          case Some(otherBoundValue) =>
            ParseDefine(appliedSymbol, otherBoundValue, operands)

          case _ => None
        }

      case _ => None
    }

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

    // Expand any macros or (begin)s recursively
    val expandedDefinition = scopedDefinition.flatMap(ExpandBodyDatum.apply)

    // Split our definition is to (define)s and a body
    val defineBuilder = new ListBuffer[ParsedDefine]

    val bodyData = expandedDefinition.dropWhile { datum =>
      parseDefineDatum(datum) match {
        case Some(define) =>
          defineBuilder += define
          true
        case None => false
      }
    }

    // Expand our scopes with all of the defines
    val bindingBlocks = defineBuilder.toList flatMap {
      case ParsedVarDefine(symbol, providedTypeOpt, exprBlock, storageLocConstructor) =>
        val schemeType = SchemeTypeForSymbol(symbol, providedTypeOpt)
        val boundValue = storageLocConstructor(symbol.name, schemeType)

        symbol.scope += (symbol.name -> boundValue)

        List(
          { () => et.SingleBinding(boundValue, exprBlock()) }
        )
      case ParsedMultipleValueDefine(fixedValueTargets, restValueTargetOpt, exprBlock) =>
        val fixedLocs = fixedValueTargets.map(_.createStorageLoc(vt.AnySchemeType))
        val restLocOpt = restValueTargetOpt.map(_.createStorageLoc(vt.UniformProperListType(vt.AnySchemeType)))

        List(
          { () => et.MultipleValueBinding(fixedLocs, restLocOpt, exprBlock()) }
        )

      case ParsedSimpleDefine(symbol, boundValue) =>
        symbol.scope += (symbol.name -> boundValue)
        Nil

      case ParsedRecordTypeDefine(typeSymbol, recordType, procedures) =>
        typeSymbol.scope += (typeSymbol.name -> BoundType(recordType))

        procedures.map { case (procedureSymbol, expr) =>
          val schemeType = SchemeTypeForSymbol(procedureSymbol)
          val storageLoc = new StorageLocation(procedureSymbol.name, schemeType)

          procedureSymbol.scope += (procedureSymbol.name -> storageLoc)

          { () => et.SingleBinding(storageLoc, expr) }
        }
      case ParsedTypeAnnotation =>
        Nil
    } : List[() => et.Binding]

    // Execute the expression blocks now that the scopes are prepared
    val bindings = bindingBlocks.map(_.apply)

    // Find the expressions in our body
    val bodyExpr = et.Expr.fromSequence(
      bodyData.map(ExtractExpr(_, false))
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
