package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.ast

object NameForType {
  private def stackedNameForTypeRef(
      schemeTypeRef : SchemeTypeRef,
      typeStack : SchemeType.Stack,
      recurseVarNames : Map[SchemeType, Char]
  ) = schemeTypeRef match {
    case RecursiveSchemeTypeRef(depth) =>
      if (depth >= typeStack.length) {
        '?'
      }
      else {
        recurseVarNames(typeStack(depth))
      }

    case DirectSchemeTypeRef(directType) =>
      stackedNameForType(directType :: typeStack, recurseVarNames)
  }

  private def nameForProcedureType(procType : ProcedureType) : String = procType match {
    case ProcedureType(mandatoryArgTypes, Nil, restArgMemberTypeOpt, returnType) =>
      val mandatoryArgNames = mandatoryArgTypes.map(apply(_))

      val restArgNames = restArgMemberTypeOpt map { restArgMemberType =>
        apply(restArgMemberType) + " *"
      }

      val returnTypeName = NameForReturnType(returnType)

      "(-> " + (mandatoryArgNames ++ restArgNames :+ returnTypeName).mkString(" ") + ")"

    case ProcedureType(mandatoryArgTypes, optionalArgTypes, restArgMemberTypeOpt, returnType) =>
      val mandatoryArgList = "(" + mandatoryArgTypes.map(apply(_)).mkString(" ") + ")"
      val optionalArgList = "(" + optionalArgTypes.map(apply(_)).mkString(" ") + ")"

      val restArgList = (restArgMemberTypeOpt map { restArgMemberType =>
        " " + apply(restArgMemberType) + " *"
      }).getOrElse("")

      val returnTypeName = NameForReturnType(returnType)

      s"(->* ${mandatoryArgList} ${optionalArgList}${restArgList} ${returnTypeName})"
  }

  private[valuetype] def stackedNameForNonRecurseType(typeStack : SchemeType.Stack, recurseVarNames : Map[SchemeType, Char]) : String = {
    typeStack.head match {
      case LiteralBooleanType(false) =>
        "#f"

      case LiteralBooleanType(true) =>
        "#t"

      case LiteralSymbolType(name) =>
        "'" + ast.Symbol(name).toString

      case SchemeTypeAtom(concreteCellType) =>
        concreteCellType.schemeName

      case pairType : PairType =>
        val carName = stackedNameForTypeRef(pairType.carTypeRef, typeStack, recurseVarNames)
        val cdrName = stackedNameForTypeRef(pairType.cdrTypeRef, typeStack, recurseVarNames)

        s"(Pairof ${carName} ${cdrName})"

      case HashMapType(keyType, valueType) =>
        val keyName = stackedNameForType(keyType :: typeStack, recurseVarNames)
        val valueName = stackedNameForType(valueType :: typeStack, recurseVarNames)

        s"(HashMap ${keyName} ${valueName})"

      case recordType : RecordType =>
        recordType.sourceName

      case externalType : ExternalRecordType =>
        externalType.sourceNameOpt getOrElse "<external-record-type>"

      case procType : ProcedureType =>
        nameForProcedureType(procType)

      case caseProcType : CaseProcedureType =>
        val signatureNames = caseProcType.signatures.map(nameForProcedureType).mkString(" ")

        s"(case-> ${signatureNames})"

      case unionType @ UnionType(memberTypes) =>
        unionType.exactCellTypeOpt match {
          case Some(exactCellType) =>
            // This is useful for named unions like <number> and <list-element>
            exactCellType.schemeName

          case _ =>
            val memberNames = memberTypes map { memberType =>
              stackedNameForType(memberType :: typeStack, recurseVarNames)
            }

            memberNames.toList.sorted match {
              case List(singleTypeName) =>
                singleTypeName

              case multipleTypeNames =>
                "(U" + multipleTypeNames.map(" " + _).mkString("")  + ")"
            }
        }
    }
  }

  private[valuetype] def stackedNameForType(typeStack : SchemeType.Stack, recurseVarNames : Map[SchemeType, Char]) : String = {
    val schemeType = typeStack.head

    schemeType match {
      case unionType @ UnionType(memberTypes) if memberTypes.size == 2 =>
        (memberTypes - EmptyListType).toList match {
          case List(SpecificPairType(DirectSchemeTypeRef(memberType), RecursiveSchemeTypeRef(1))) =>
            // Special case (Listof) which constructs a particular recursive type structure
            return s"(Listof ${apply(memberType)})"

          case _ =>
        }

      case _ =>
    }

    if (HasRecursiveRef(schemeType)) {
      // Allocate a type variable
      val typeVariable = ('A' + recurseVarNames.size).toChar
      val newRecurseVarNames = recurseVarNames + (schemeType -> typeVariable)
      val recursiveResult = stackedNameForNonRecurseType(typeStack, newRecurseVarNames) 

      // Wrap this in a recursive type constructor
      s"(Rec ${typeVariable} ${recursiveResult})"
    }
    else {
      stackedNameForNonRecurseType(typeStack, recurseVarNames)
    }
  }

  /** Returns the Scheme name for the passed type
    *
    * Note that this is not aware of any renaming or aliasing of types that occured in the frontend. Instead this
    * assumes that all types have their default binding.
    */
  def apply(valueType : ValueType) : String = valueType match {
    case Predicate =>
      "<native-bool>"

    case signedIntType : IntType if signedIntType.signed =>
      s"<native-int${signedIntType.bits}>"

    case unsignedIntType : IntType if !unsignedIntType.signed =>
      s"<native-uint${unsignedIntType.bits}>"

    case Float =>
      "<native-float>"
    
    case Double =>
      "<native-double>"

    case UnicodeChar =>
      "<native-unicode-char>"

    case schemeType : SchemeType =>
      // We need to deal with recursion etc.
      stackedNameForType(schemeType :: Nil, Map())
  }
}
