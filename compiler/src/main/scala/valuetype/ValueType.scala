package io.llambda.compiler.valuetype
import io.llambda

import llambda.compiler.{celltype => ct}
import llambda.compiler.{valuetype => vt}

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
  val schemeType = TopProcedureType
}

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
  def +(otherType : SchemeType) : SchemeType = {
    SchemeType.fromTypeUnion(List(this, otherType))
  }
  
  /** Returns the procedure type for this type if one exists
    *
    * Unions can only have a single procedure type at once. This will return that procedure type if it exists or 
    * None if the union has no procedure type.
    */
  def procedureTypeOpt : Option[ProcedureType] =
    None
}

/** Scheme type representing an exact value */
sealed abstract trait ConstantValueType extends SchemeType

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
case class SchemeTypeAtom(cellType : ct.ConcreteCellType) extends NonUnionSchemeType {
  val isGcManaged = cellType match {
    case preconstruct : ct.PreconstructedCellType =>
      // Only constant instances of this exist
      false

    case _ =>
      true
  }
  
  override def procedureTypeOpt : Option[ProcedureType] =
    if (cellType == ct.ProcedureCell) {
      Some(TopProcedureType)
    }
    else {
      None
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
  def apply(memberTypeRef : SchemeTypeRef) : SchemeType = {
    UnionType(Set(
      EmptyListType,
      SpecificPairType(
        carTypeRef=memberTypeRef,
        // Point back to the outer union type
        cdrTypeRef=RecursiveSchemeTypeRef(1)
      )
    ))
  }
}

object SpecificProperListType {
  /** Constructs a recursive type representing a fixed length proper list with specific member types */
  def apply(memberTypeRefs : Iterable[SchemeTypeRef]) : NonUnionSchemeType = 
    memberTypeRefs.foldRight(EmptyListType : NonUnionSchemeType) { case (memberTypeRef, cdrType) =>
      SpecificPairType(memberTypeRef, DirectSchemeTypeRef(cdrType))
    }

  def unapply(schemeType : SchemeType) : Option[List[SchemeTypeRef]] = schemeType match {
    case SpecificPairType(typeRef, DirectSchemeTypeRef(EmptyListType)) =>
      Some(List(typeRef))

    case SpecificPairType(typeRef, DirectSchemeTypeRef(tail))  => 
      unapply(tail).map { tailTypeRefs =>
        typeRef :: tailTypeRefs
      }

    case _ => None
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
  private[valuetype] def exactCellTypeOpt : Option[ct.CellType] = {
    (cellTypesBySpecificity(ct.AnyCell).find { candidateCellType =>
      SchemeType.fromCellType(candidateCellType) == this 
    })
  }
  
  override def procedureTypeOpt : Option[ProcedureType] =
    memberTypes.flatMap(_.procedureTypeOpt).headOption
}

/** Abstract trait for vector types */
sealed trait VectorType extends DerivedSchemeType {
  val cellType = ct.VectorCell
  val isGcManaged = true

  val parentType = SchemeTypeAtom(ct.VectorCell)
}

/** Vector with a uniform type */
case class UniformVectorType(memberTypeRef : SchemeTypeRef) extends VectorType

/** Vector with known value types */
case class SpecificVectorType(memberTypeRefs : Vector[SchemeTypeRef]) extends VectorType

object VectorOfType {
  def apply(memberTypeRef : SchemeTypeRef) : NonUnionSchemeType =
    if (memberTypeRef == DirectSchemeTypeRef(AnySchemeType)) {
      SchemeTypeAtom(ct.VectorCell)
    }
    else {
      UniformVectorType(memberTypeRef)
    }
}

case class ProcedureType(
    fixedArgTypes : List[SchemeType],
    restArgMemberTypeOpt : Option[SchemeType],
    returnType : vt.ReturnType.ReturnType
) extends DerivedSchemeType {
  val cellType = ct.ProcedureCell
  val isGcManaged = true

  val parentType = SchemeTypeAtom(ct.ProcedureCell)
  
  override def procedureTypeOpt : Option[ProcedureType] =
    Some(this)
}

object TopProcedureType extends ProcedureType(Nil, Some(AnySchemeType), ReturnType.ArbitraryValues)

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

  private def simplifyProcTypes(nonUnionTypes : Set[NonUnionSchemeType]) : Set[NonUnionSchemeType] = 
    if (nonUnionTypes.count(_.isInstanceOf[ProcedureType]) > 1) {
      val (allProcTypes, nonProcTypes) =  nonUnionTypes.partition(_.isInstanceOf[ProcedureType])

      // We can't distinguish procedure types at runtime
      // Try to combine them in to a single procedure type
      val superProcType = allProcTypes.reduceLeft({
        (left : vt.NonUnionSchemeType, right : vt.NonUnionSchemeType) =>
          if (vt.SatisfiesType(left, right) == Some(true)) {
            // Left type is more general
            left
          }
          else if (vt.SatisfiesType(right, left) == Some(true)) {
            // Right type is more general
            right
          }
          else {
            // Types are disjoint - use the top procedure type and abort
            return nonProcTypes + TopProcedureType
          }
      })

      nonProcTypes + superProcType
    }
    else {
      nonUnionTypes
    }

  def fromTypeUnion(otherTypes : Iterable[SchemeType]) : SchemeType = {
    val nonUnionTypes = (otherTypes.flatMap {
      case nonUnion : NonUnionSchemeType =>
        Set(nonUnion)

      case union : UnionType =>
        union.memberTypes
    }).toSet

    // Convert (U #f #t) to <boolean>
    // This should just be cosmetic
    val boolSimplifiedTypes = if (allBooleans.subsetOf(nonUnionTypes)) {
      nonUnionTypes -- allBooleans + BooleanType
    }
    else {
      nonUnionTypes
    }

    val procSimplifiedTypes = simplifyProcTypes(boolSimplifiedTypes)

    if (procSimplifiedTypes.size == 1) {
      procSimplifiedTypes.head
    }
    else {
      UnionType(procSimplifiedTypes.toSet)
    }
  }
}
