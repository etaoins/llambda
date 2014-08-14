package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}

/** Type of any value known to the compiler
  * 
  * These are more specific than the types defined in R7RS in that they define both the semantic type and the runtime
  * representation. For example, Int32 and vt.ExactIntegerType are both exact integers but have different native
  * representations.
  */
sealed abstract class ValueType {
  val schemeType : SchemeType
  val isGcManaged : Boolean

  override def toString = NameForType(this)
}

/** Type represented by a native pointer */
sealed abstract trait PointerType extends ValueType

/** Pointer to a garbage collected value cell */
sealed abstract trait CellValueType extends PointerType {
  val cellType : ct.CellType
}

/** Primitive types shared with C */
sealed abstract class NativeType extends ValueType {
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

/** Identifies a record field
  *
  * This is not a case class because a field with the same source name and type can be distinct if it's declared in
  * another record type. It's even possible for one type to have fields with the same source name if they come from
  * different scopes.
  */
class RecordField(val sourceName : String, val fieldType : ValueType)

/** Pointer to a garabge collected value cell containing a record-like type */
sealed abstract class RecordLikeType extends CellValueType {
  val sourceName : String
  val fields : List[RecordField]
  val cellType : ct.ConcreteCellType with ct.RecordLikeFields
  val isGcManaged = true
}

/** Pointer to a closure type
  *
  * Closure types store the data needed for a procedure from its parent lexical scope. The storage is internally
  * implemented identically to user-defined record types.
  */
class ClosureType(val sourceName : String, val fields : List[RecordField]) extends RecordLikeType {
  val cellType = ct.ProcedureCell
  val schemeType = ProcedureType
}

/** Types visible to Scheme programs without using the NFI */ 
sealed abstract trait SchemeType extends CellValueType {
  val schemeType = this
  
  /** Subtracts another type from this one
    *
    * This is typically used after a type test to build a new possible Scheme type for a tested value
    */
  def -(otherType : SchemeType) : SchemeType

  /** Creates a union of this type with another */
  def +(otherType : SchemeType) : SchemeType = {
    SchemeType.fromTypeUnion(List(this, otherType))
  }
  
  /** Intersects this type with another */
  def &(otherType : SchemeType) : SchemeType
}

/** Scheme type representing an exact value */
sealed abstract trait ConstantValueType extends SchemeType

/** All Scheme types except unions
  *
  * This is to enforce that unions of unions have their member types flattened in to a single union.
  */
sealed abstract trait NonUnionSchemeType extends SchemeType {
  val cellType : ct.CellType

  def -(otherType : SchemeType) : SchemeType = 
    if (SatisfiesType(otherType, this) == Some(true)) {
      // No type remains
      EmptySchemeType
    }
    else {
      this
    }

  def &(otherType : SchemeType) : SchemeType = otherType match {
    case otherProperList : ProperListType =>
      // Proper lists know how to deal with intersections
      otherProperList & this

    case _ if (SatisfiesType(otherType, this) == Some(true)) =>
      this

    case _ if (SatisfiesType(this, otherType) == Some(true)) =>
      otherType

    case _ =>
      // No intersection
      EmptySchemeType
  }
}

/** Utility type for Scheme types derived from other Scheme types */
sealed abstract trait DerivedSchemeType extends NonUnionSchemeType {
  val parentType : NonUnionSchemeType
}

/** Pointer to a garbage collected value cell containing an intrinsic type */
case class SchemeTypeAtom(cellType : ct.ConcreteCellType) extends NonUnionSchemeType {
  val isGcManaged = cellType match {
    case preconstruct : ct.PreconstructedCellType =>
      // Only constant instances of this exist
      false

    case _ =>
      true
  }
  
  // Handle <boolean> specially - it only has two subtypes
  override def -(otherType : SchemeType) : SchemeType =  (cellType, otherType) match {
    case (ct.BooleanCell, ConstantBooleanType(value))=>
      ConstantBooleanType(!value)

    case _ =>
      super.-(otherType)
  }
}

/** Constant boolean type */
case class ConstantBooleanType(value : Boolean) extends DerivedSchemeType with ConstantValueType {
  val cellType = ct.BooleanCell
  val parentType = BooleanType
  val isGcManaged = BooleanType.isGcManaged
}

/** Trait for pair types */
sealed trait PairType extends NonUnionSchemeType {
  val carType : SchemeType
  val cdrType : SchemeType
}

/** Pair with specific types for its car and cdr */
case class SpecificPairType(carType : SchemeType, cdrType : SchemeType) extends DerivedSchemeType with PairType {
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
  val carType = AnySchemeType
  val cdrType = AnySchemeType
}

object PairType {
  /** Constructs a new pair type with the given car and cdr types
    *
    * If both car and cdr are <any> then AnyPairType is returned. Otherwise, an appropriate instance of
    * SpecificPairType is constructed.
    */
  def apply(carType : SchemeType, cdrType : SchemeType) : PairType = {
    (SatisfiesType(carType, AnySchemeType), SatisfiesType(cdrType, AnySchemeType)) match {
      case (Some(true), Some(true)) =>
        AnyPairType

      case _ =>
        SpecificPairType(carType, cdrType)
    }
  }
}

/** Proper list contains members of a certain type */
case class ProperListType(memberType : SchemeType) extends NonUnionSchemeType {
  val cellType = ct.ListElementCell
  val isGcManaged = true

  private def pairType = SpecificPairType(memberType, this)
  
  override def -(otherType : SchemeType) : SchemeType = otherType match {
    case superType if SatisfiesType(otherType, this) == Some(true) =>
      EmptySchemeType

    case EmptyListType => 
      pairType

    case compatiblePair : PairType if SatisfiesType(compatiblePair, pairType) == Some(true) =>
      EmptyListType

    case _ =>
      this
  }

  override def &(otherType: SchemeType) : SchemeType = otherType match {
    case superType if SatisfiesType(otherType, this) == Some(true) =>
      this

    case ProperListType(otherMemberType) =>
      val memberTypeIntersect = memberType & otherMemberType

      if (memberTypeIntersect == EmptySchemeType) {
        EmptySchemeType
      }
      else {
        ProperListType(memberTypeIntersect)
      }

    case compatiblePair : PairType if SatisfiesType(compatiblePair, pairType) == Some(true) =>
      pairType & compatiblePair

    case EmptyListType =>
      EmptyListType

    case _ =>
      EmptySchemeType
  }
}


/** Pointer to a garabge collected value cell containing a user-defined record type
  * 
  * This uniquely identifies a record type even if has the same name and internal structure as another type 
  */
class RecordType(val sourceName : String, val fields : List[RecordField]) extends RecordLikeType with DerivedSchemeType {
  val cellType = ct.RecordCell
  val parentType = SchemeTypeAtom(ct.RecordCell)
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
  private[valuetype] lazy val exactCellTypeOpt : Option[ct.CellType] = {
    (cellTypesBySpecificity(ct.AnyCell).find { candidateCellType =>
      SchemeType.fromCellType(candidateCellType) == this 
    })
  }
  
  def -(otherType : SchemeType) : SchemeType = {
    val remainingMembers = memberTypes.map(_.-(otherType))
    SchemeType.fromTypeUnion(remainingMembers.toList)
  }

  def &(otherType: SchemeType) : SchemeType = otherType match {
    case otherProperList : ProperListType =>
      // Proper lists know how to deal with intersections
      otherProperList & this

    case _ =>
      val intersectedMembers = memberTypes.map(_.&(otherType))
      SchemeType.fromTypeUnion(intersectedMembers.toList)
  }
}

/** Union of all possible Scheme types */
object AnySchemeType extends UnionType(ct.AnyCell.concreteTypes.map(SchemeTypeAtom(_)))

/** Empty union of types
  *
  * No type satisfies this type. It doesn't make sense for values or operands to have the empty type but it's not
  * explicitly forbiddened. It occurs mostly as a result of operations on other types, such as the intersection
  * of disjoint types.
  */
object EmptySchemeType extends UnionType(Set())

object SchemeType {
  private val allBooleans = Set[NonUnionSchemeType](ConstantBooleanType(false), ConstantBooleanType(true))

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

  def fromTypeUnion(otherTypes : Seq[SchemeType]) : SchemeType = {
    val nonUnionTypes = (otherTypes.flatMap {
      case nonUnion : NonUnionSchemeType =>
        Set(nonUnion)

      case union : UnionType =>
        union.memberTypes
    }).toSet

    // Convert (U #f #t) to <boolean>
    // This should just be cosmetic
    val simplifiedTypes = if (allBooleans.subsetOf(nonUnionTypes)) {
      nonUnionTypes -- allBooleans + BooleanType
    }
    else {
      nonUnionTypes
    }

    if (simplifiedTypes.size == 1) {
      simplifiedTypes.head
    }
    else {
      UnionType(simplifiedTypes.toSet)
    }
  }
}
