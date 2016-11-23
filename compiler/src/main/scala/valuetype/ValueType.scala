package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.valuetype.{polymorphic => pm}

/** Type of any value known to the compiler
  * 
  * These are more specific than the types defined in R7RS in that they define both the semantic type and the runtime
  * representation. For example, Int32 and ExactIntegerType are both exact integers but have different native
  * representations.
  */
sealed abstract class ValueType {
  val schemeType : SchemeType
  val isGcManaged : Boolean

  override def toString = NameForType(this)
}

/** Indicates types that forbid recursive type references to cross them */
sealed abstract trait NonRecursiveType extends ValueType

/** Indicates types that contain no references to other types, recursive or direct */
sealed abstract trait LeafType extends NonRecursiveType

/** Type represented by a native pointer */
sealed abstract trait PointerType extends ValueType

/** Pointer to a garbage collected value cell */
sealed abstract trait CellValueType extends PointerType {
  val cellType : ct.CellType
}

/** Primitive types shared with C */
sealed abstract class NativeType extends ValueType with LeafType {
  val isGcManaged = false
}

/** Type reperesented by a native integer */
sealed abstract class IntLikeType(val bits : Int, val signed : Boolean) extends NativeType

/** Native boolean value */
case object Predicate extends IntLikeType(1, false) {
  val schemeType = BooleanType
}

/** Native integer type representing a Scheme exact integer */
sealed abstract class IntType(bits : Int, signed : Boolean) extends IntLikeType(bits, signed) {
  val schemeType = ExactIntegerType

  def minIntValue = if (signed) {
    -1L << (bits - 1)
  }
  else {
    0
  }

  def maxIntValue = if (signed) {
    (1L << (bits - 1)) - 1
  }
  else {
    (1L << bits) - 1
  }
}

case object Int8 extends IntType(8, true)
case object Int16 extends IntType(16, true)
case object Int32 extends IntType(32, true)
case object Int64 extends IntType(64, true)

case object UInt8 extends IntType(8, false)
case object UInt16 extends IntType(16, false)
case object UInt32 extends IntType(32, false)
// UInt64 is outside the range we can represent

/** Native floating point type representing a Scheme inexact rational */
sealed abstract class FpType extends NativeType {
  val schemeType = FlonumType
}

case object Float extends FpType 
case object Double extends FpType

/** Native integer representing a Unicode code point */
case object UnicodeChar extends IntLikeType(32, true) {
  val schemeType = CharType
}

/** Pointer to a garbage collected value cell containing a user-defined record type
  *
  * This uniquely identifies a record type even if has the same name and internal structure as another type
  */
class RecordType(
    val sourceName : String,
    val fields : List[RecordField],
    val selfTypeVarOpt : Option[pm.TypeVar] = None,
    val parentRecordOpt : Option[RecordType] = None
) extends CellValueType with NonRecursiveType with DerivedSchemeType with RecordLikeType {
  val cellType = ct.RecordCell
  val isGcManaged = true
  val parentType = SchemeTypeAtom(ct.RecordCell)

  /** Test if this type is equal to or a child of another type */
  def isEqualToOrChildOf(other : RecordType) : Boolean = {
    if (other eq this) {
      true
    }
    else {
      parentRecordOpt match {
        case Some(parentRecordLike) =>
          parentRecordLike.isEqualToOrChildOf(other)

        case None =>
          false
      }
    }
  }

  lazy val typeForField : Map[RecordField, ValueType] = fields.map({ field =>
    val selfTypeVars = selfTypeVarOpt.map(_ -> this).toMap
    val reconciledVars = pm.ReconcileTypeVars.Result(selfTypeVars)

    field -> pm.InstantiateType(reconciledVars, field.typeTemplate)
  }).toMap ++ parentRecordOpt.map(_.typeForField).getOrElse(Map())
}

/** Represents an externally defined record type
  *
  * This allows types to be defined by native libraries without explicit support from the compiler. They're treated
  * similarly to Scheme record types by the type system: every type is considered distinct. However, the compiler will
  * emit no definition for the type or accessors for for its fields.
  *
  * @param  sourceNameOpt   Name of the type at the site of definition. This is used by diagnostic messages.
  * @param  predicateOpt   Optional predicate function. If none is provided then runtime type checks will not be
  *                        supported.
  */
class ExternalRecordType(
    val sourceNameOpt : Option[String],
    val predicateOpt : Option[ExternalRecordTypePredicate]
) extends CellValueType with NonRecursiveType with DerivedSchemeType {
  val cellType = ct.RecordCell
  val isGcManaged = true
  val parentType = SchemeTypeAtom(ct.RecordCell)
}

/** Represents a hash map type */
case class HashMapType(
    keyType : SchemeType,
    valueType : SchemeType
) extends CellValueType with NonRecursiveType with DerivedSchemeType {
  val cellType = ct.HashMapCell
  val isGcManaged = true
  val parentType = SchemeTypeAtom(ct.HashMapCell)
}

object AnyHashMapType extends HashMapType(AnySchemeType, AnySchemeType)

/** Types visible to Scheme programs without using the NFI */
sealed abstract trait SchemeType extends CellValueType {
  val schemeType = this

  /** Unrolls any recursive type references to ourselves */
  def unrolled : SchemeType =
    UnrollType.unrollType(this, this, 0)

  /** Unrolls one of our child types
    *
    * Unrolling makes sure any recursive references to ourselves inside the unrolled type are replaced by a literal
    * copy of ourselves. This ensures that the resulting type is a valid standalone type
    */
  def unrollChildType(childType : SchemeType) : SchemeType =
    UnrollType.unrollType(childType, this, 1)

  /** Unrolls a child type reference
    *
    * This is the same as unrollChildType except it operates on type references
    */
  def unrollChildTypeRef(childTypeRef : SchemeTypeRef) : SchemeType =
    UnrollType.unrollTypeRef(childTypeRef, this, 0) match {
      case DirectSchemeTypeRef(directType) =>
        directType

      case _ =>
        throw new InvalidSchemeTypeRef("Attempted to unroll invalid recursive type") 
    }
  
  /** Shortcut for IntersectTypes(this, otherType) */
  def &(otherType : SchemeType) : SchemeType =
    IntersectTypes(this, otherType)

  /** Shortcut for SubtractTypes(this, otherType) */
  def -(otherType : SchemeType) : SchemeType =
    SubtractTypes(this, otherType)

  /** Creates a union of this type with another */
  def +(otherType : SchemeType) : SchemeType =
    if (this == otherType) {
      this
    }
    else {
      SchemeType.fromTypeUnion(List(this, otherType))
    }

  /** Returns the applicable type for this type if one exists
    *
    * Unions can only have a single applicable type at once. This will return that applicable type if it exists or None 
    * if the union has no applicable type.
    */
  def applicableTypeOpt : Option[ApplicableType] =
    None

  /** Returns this type with a new applicable type
    *
    * If the previous type did not have a applicable type this will return the original type
    */
   def replaceApplicableType(applicableType : ApplicableType) : SchemeType =
     this
}

/** Scheme type representing an specific literal value */
sealed abstract trait LiteralValueType extends SchemeType with LeafType

/** All Scheme types except unions
  *
  * This is to enforce that unions of unions have their member types flattened in to a single union.
  */
sealed abstract trait NonUnionSchemeType extends SchemeType {
  val cellType : ct.CellType
}

/** Utility type for Scheme types derived from other Scheme types */
sealed abstract trait DerivedSchemeType extends NonUnionSchemeType {
  val parentType : NonUnionSchemeType
}

/** Pointer to a garbage collected value cell containing an intrinsic type */
case class SchemeTypeAtom(cellType : ct.ConcreteCellType) extends NonUnionSchemeType with LeafType {
  val isGcManaged = cellType match {
    case preconstruct : ct.PreconstructedCellType =>
      // Only constant instances of this exist
      false

    case _ =>
      true
  }
  
  override def applicableTypeOpt : Option[ApplicableType] =
    if (cellType == ct.ProcedureCell) {
      Some(TopProcedureType)
    }
    else {
      None
    }
   
  override def replaceApplicableType(procType : ApplicableType) : SchemeType =
    if (cellType == ct.ProcedureCell) {
      procType
    }
    else {
      this
    }
}

/** Literal boolean type */
case class LiteralBooleanType(value : Boolean) extends DerivedSchemeType with LiteralValueType {
  val cellType = ct.BooleanCell
  val parentType = BooleanType
  val isGcManaged = BooleanType.isGcManaged
}

/** Literal symbol types */
case class LiteralSymbolType(value : String) extends DerivedSchemeType with LiteralValueType {
  val cellType = ct.SymbolCell
  val parentType = SymbolType
  val isGcManaged = SymbolType.isGcManaged
}

/** Trait for pair types */
sealed trait PairType extends NonUnionSchemeType {
  val carTypeRef : SchemeTypeRef
  val cdrTypeRef : SchemeTypeRef
}

/** Pair with specific types for its car and cdr */
case class SpecificPairType(carTypeRef : SchemeTypeRef, cdrTypeRef : SchemeTypeRef) extends DerivedSchemeType with PairType {
  val cellType = ct.PairCell
  val parentType = SchemeTypeAtom(ct.PairCell)
  val isGcManaged = true
}

/** Pair with no type constraints on its car or cdr
  *
  * This is identical to the pair cell's Scheme type atom. This is important so that there isn't a distinct pair type
  * atom from a pair type with <any> for both of its types
  */
object AnyPairType extends SchemeTypeAtom(ct.PairCell) with PairType {
  val carTypeRef = DirectSchemeTypeRef(AnySchemeType)
  val cdrTypeRef = DirectSchemeTypeRef(AnySchemeType)
}

object PairType {
  /** Constructs a new pair type with the given car and cdr types
    *
    * If both car and cdr are <any> then AnyPairType is returned. Otherwise, an appropriate instance of
    * SpecificPairType is constructed.
    */
  def apply(carTypeRef : SchemeTypeRef, cdrTypeRef : SchemeTypeRef) : PairType = {
    (carTypeRef, cdrTypeRef) match {
      case (DirectSchemeTypeRef(AnySchemeType), DirectSchemeTypeRef(AnySchemeType)) =>
        AnyPairType

      case _ =>
        SpecificPairType(carTypeRef, cdrTypeRef)
    }
  }
}

object UniformProperListType {
  /** Constructs a recursive type representing a unsized proper list with a uniform member type */
  def apply(memberType : SchemeType) : SchemeType = {
    UnionType(Set(
      EmptyListType,
      SpecificPairType(
        carTypeRef=DirectSchemeTypeRef(memberType),
        // Point back to the outer union type
        cdrTypeRef=RecursiveSchemeTypeRef(1)
      )
    ))
  }
}

object SpecificProperListType {
  /** Constructs a recursive type representing a fixed length proper list with specific member types */
  def apply(memberTypeRefs : Iterable[SchemeType]) : NonUnionSchemeType =
    memberTypeRefs.foldRight(EmptyListType : NonUnionSchemeType) { case (memberTypeRef, cdrType) =>
      SpecificPairType(DirectSchemeTypeRef(memberTypeRef), DirectSchemeTypeRef(cdrType))
    }

  def unapply(schemeType : SchemeType) : Option[List[SchemeTypeRef]] = schemeType match {
    case SpecificPairType(typeRef, DirectSchemeTypeRef(EmptyListType)) =>
      Some(List(typeRef))

    case SpecificPairType(typeRef, DirectSchemeTypeRef(tail)) =>
      unapply(tail).map { tailTypeRefs =>
        typeRef :: tailTypeRefs
      }

    case _ => None
  }
}

/** Union of multiple Scheme types */
case class UnionType(memberTypes : Set[NonUnionSchemeType]) extends SchemeType {
  lazy val isGcManaged = memberTypes.exists(_.isGcManaged)
  
  private def cellTypesBySpecificity(rootType : ct.CellType) : List[ct.CellType] = {
    rootType.directSubtypes.toList.flatMap(cellTypesBySpecificity) :+ rootType
  }

  /** Most specific cell type that is a superset all of our member types */
  lazy val cellType : ct.CellType = {
    val possibleCellTypes = memberTypes.flatMap(_.cellType.concreteTypes) : Set[ct.ConcreteCellType]

    // Find the most specific cell type that will cover all of our member types
    (cellTypesBySpecificity(ct.AnyCell).find { candidateCellType =>
      possibleCellTypes.subsetOf(candidateCellType.concreteTypes)
    }).get
  }

  /** Cell type exactly matching our member types or None if no exact match exists */
  private[valuetype] def exactCellTypeOpt : Option[ct.CellType] = {
    (cellTypesBySpecificity(ct.AnyCell).find { candidateCellType =>
      SchemeType.fromCellType(candidateCellType) == this 
    })
  }
  
  override def applicableTypeOpt : Option[ApplicableType] =
    memberTypes.flatMap(_.applicableTypeOpt).headOption
  
  override def replaceApplicableType(procType : ApplicableType) : SchemeType = {
    val (oldProcTypes, nonProcTypes) =  memberTypes.partition(_.isInstanceOf[ProcedureType])

    if (oldProcTypes.isEmpty) {
      this
    }
    else {
      SchemeType.fromTypeUnion(nonProcTypes + procType)
    }
  }

}

sealed abstract trait ApplicableType extends NonUnionSchemeType with NonRecursiveType {
  def signatures : Seq[ProcedureType]
}

case class ProcedureType(
    mandatoryArgTypes : List[SchemeType],
    optionalArgTypes : List[SchemeType],
    restArgMemberTypeOpt : Option[SchemeType],
    returnType : ReturnType.ReturnType[SchemeType]
) extends DerivedSchemeType with ApplicableType {
  val cellType = ct.ProcedureCell
  val isGcManaged = true

  val parentType = SchemeTypeAtom(ct.ProcedureCell)

  def signatures = List(this)
  
  override def applicableTypeOpt : Option[ApplicableType] =
    Some(this)

  override def replaceApplicableType(applicableType : ApplicableType) =
    applicableType

  def toPolymorphic =
    pm.PolymorphicProcedureType(Set(), this)
}

case class CaseProcedureType(
    clauseTypes : List[ProcedureType]
) extends DerivedSchemeType with ApplicableType {
  val cellType = ct.ProcedureCell
  val isGcManaged = true

  val parentType = SchemeTypeAtom(ct.ProcedureCell)

  def signatures = clauseTypes
  
  override def applicableTypeOpt : Option[ApplicableType] =
    Some(this)
}

object TopProcedureType extends ProcedureType(Nil, Nil, Some(AnySchemeType), ReturnType.SingleValue(AnySchemeType))

/** Union of all possible Scheme types */
object AnySchemeType extends UnionType(ct.AnyCell.concreteTypes.map(SchemeTypeAtom(_)))

/** Empty union of types
  *
  * No type satisfies this type. It doesn't make sense for values or args to have the empty type but it's not explicitly
  * forbidden. It occurs mostly as a result of operations on other types, such as the intersection of disjoint types.
  */
object EmptySchemeType extends UnionType(Set())

object SchemeType {
  private val allBooleans = Set[NonUnionSchemeType](LiteralBooleanType(false), LiteralBooleanType(true))

  /** Represents a stack of Scheme types
    *
    * SchemeType stacks are used to track enclosing types when handling recursive types. Recursive type references are
    * made to a particular level in the type stack above the referencing type.
    */
  type Stack = List[SchemeType]

  def fromCellType(cellType : ct.CellType) : SchemeType = {
    cellType match {
      case concrete : ct.ConcreteCellType =>
        SchemeTypeAtom(concrete)

      case _ =>
        // Non-concrete cell types are a way of sharing unions types between C++ and Scheme
        // Break them down to union types on our side
        UnionType(cellType.concreteTypes.map(SchemeTypeAtom(_)))
    }
  }

  private def simplifyApplicableTypes(nonUnionTypes : Set[NonUnionSchemeType]) : Set[NonUnionSchemeType] = 
    if (nonUnionTypes.count(_.isInstanceOf[ApplicableType]) > 1) {
      val (allApplicableTypes, nonApplicableTypes) = nonUnionTypes.partition(_.isInstanceOf[ApplicableType])

      // We can't distinguish procedure types at runtime
      // Try to combine them in to a single procedure type
      val superApplicableType = allApplicableTypes.reduceLeft({
        (left : NonUnionSchemeType, right : NonUnionSchemeType) =>
          if (SatisfiesType(left, right) == Some(true)) {
            // Left type is more general
            left
          }
          else if (SatisfiesType(right, left) == Some(true)) {
            // Right type is more general
            right
          }
          else {
            // Types are disjoint - use the top procedure type and abort
            return nonApplicableTypes + TopProcedureType
          }
      })

      nonApplicableTypes + superApplicableType
    }
    else {
      nonUnionTypes
    }

  def fromTypeUnion(otherTypes : Iterable[SchemeType]) : SchemeType = {
    val nonUnionTypes = (otherTypes.flatMap {
      case nonUnion : NonUnionSchemeType =>
        Set(nonUnion)

      case union : UnionType =>
        // This is a bit tricky
        // We attempt to flatten any union types together in to the returned union. For example,
        // (U (U <exact-integer> <flonum>) (U <symbol> <string>))
        // Will become:
        // (U <exact-integer> <flonum> <symbol> <string>)
        //
        // However, e.g. for proper lists this would naively change:
        // (U (Rec A (U '() (Pairof <any> A))) #f)
        // In to:
        // (Rec A (U '() #f (Pairof <any> A)))
        //
        // This is no longer a proper list type as #f has been flattened in to the proper list's union. Instead we need
        // to unroll any recursive unions to preserve their recursive structure
        union.memberTypes.map(UnrollType.unrollNonUnionType(_, union, depth=1))
    }).toSet

    // Convert (U #f #t) to <boolean>
    // This should just be cosmetic
    val boolSimplifiedTypes = if (allBooleans.subsetOf(nonUnionTypes)) {
      nonUnionTypes -- allBooleans + BooleanType
    }
    else {
      nonUnionTypes
    }

    val applicableSimplifiedTypes = simplifyApplicableTypes(boolSimplifiedTypes)

    if (applicableSimplifiedTypes.size == 1) {
      applicableSimplifiedTypes.head
    }
    else {
      UnionType(applicableSimplifiedTypes.toSet)
    }
  }
}
