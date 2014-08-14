package io.llambda.compiler.valuetype
import io.llambda

object NameForType {
  def apply(valueType : ValueType) : String = valueType match {
    case anyType if anyType eq AnySchemeType =>
      "<any>"

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

    case ConstantBooleanType(false) =>
      "#f"
    
    case ConstantBooleanType(true) =>
      "#t"

    case SchemeTypeAtom(concreteCellType) =>
      concreteCellType.schemeName

    case pairType : PairType =>
      s"(Pair ${apply(pairType.carType)} ${apply(pairType.cdrType)})"

    case ProperListType(memberType) =>
      s"(Listof ${apply(memberType)})"

    case recordType : RecordType =>
      recordType.sourceName

    case unionType @ UnionType(memberTypes) =>
      unionType.exactCellTypeOpt match {
        case Some(exactCellType) =>
          // This is useful for named unions like <number> and <list-element>
          exactCellType.schemeName

        case _ =>
          memberTypes.map(apply).toList.sorted match {
            case List(singleTypeName) =>
              singleTypeName

            case multipleTypeNames =>
              "(U" + multipleTypeNames.map(" " + _).mkString("")  + ")"
          }
      }
  }
}
