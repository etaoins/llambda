package io.llambda.compiler.valuetype
import io.llambda

object NameForType {
  private def stackedNameForTypeRef(
      schemeTypeRef : SchemeTypeRef,
      typeStack : SchemeType.Stack,
      recurseVarNames : Map[SchemeType, Char]
  ) = schemeTypeRef match {
    case RecursiveSchemeTypeRef(depth) =>
      recurseVarNames(typeStack(depth))

    case DirectSchemeTypeRef(directType) =>
      stackedNameForType(directType :: typeStack, recurseVarNames)
  }
  
  private[valuetype] def stackedNameForNonRecurseType(typeStack : SchemeType.Stack, recurseVarNames : Map[SchemeType, Char]) : String = {
    typeStack.head match {
      case ConstantBooleanType(false) =>
        "#f"
      
      case ConstantBooleanType(true) =>
        "#t"

      case SchemeTypeAtom(concreteCellType) =>
        concreteCellType.schemeName

      case pairType : PairType =>
        val carName = stackedNameForTypeRef(pairType.carTypeRef, typeStack, recurseVarNames)
        val cdrName = stackedNameForTypeRef(pairType.cdrTypeRef, typeStack, recurseVarNames)

        s"(Pairof ${carName} ${cdrName})"

      case recordType : RecordType =>
        recordType.sourceName

      case specificVectorType : SpecificVectorType =>
        val memberTypeNames = specificVectorType.memberTypeRefs map { memberTypeRef =>
          stackedNameForTypeRef(memberTypeRef, typeStack, recurseVarNames)
        }

        "(Vector" + memberTypeNames.map(" " + _).mkString("") + ")" 
      
      case uniformVectorType : UniformVectorType =>
        val memberTypeRef = uniformVectorType.memberTypeRef
        val memberTypeName = stackedNameForTypeRef(memberTypeRef, typeStack, recurseVarNames)

        s"(Vectorof ${memberTypeName})"

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
          case List(pairType @ SpecificPairType(memberTypeRef, RecursiveSchemeTypeRef(1))) =>
            // Special case (Listof) which constructs a particular recursive type structure
            val memberName = stackedNameForTypeRef(memberTypeRef, pairType :: unionType :: typeStack, recurseVarNames)
            return s"(Listof ${memberName})"

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

    case _ : ClosureType =>
      "<internal-closure-type>"

    case schemeType : SchemeType =>
      // We need to deal with recursion etc.
      stackedNameForType(schemeType :: Nil, Map())
  }
}
