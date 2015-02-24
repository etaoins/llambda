package io.llambda.compiler
import io.llambda

import llambda.compiler.{valuetype => vt}
import llambda.compiler.valuetype.{polymorphic => pm}

/** Represents the declared type for a storage location */
sealed abstract class LocTypeDeclaration {
  def toSchemeType : vt.SchemeType
}

/** Indicates a declaration of a polymorphic procedure */
case class MonomorphicDeclaration(schemeType : vt.SchemeType) extends LocTypeDeclaration {
  override def toString = vt.NameForType(schemeType)

  def toSchemeType =
    schemeType
}

/** Indicates the declaration of a monomorphic type */
case class PolymorphicProcedureDeclaration(polyType : pm.PolymorphicProcedureType) extends LocTypeDeclaration {
  override def toString = pm.NameForPolymorphicProcedureType(polyType)

  def toSchemeType =
    polyType.upperBound
}
