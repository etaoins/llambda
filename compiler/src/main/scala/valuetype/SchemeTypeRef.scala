package io.llambda.compiler.valuetype
import io.llambda

class InvalidSchemeTypeRef(message : String) extends Exception(message)

sealed abstract trait SchemeTypeRef {
  def resolve(typeStack : SchemeType.Stack) : SchemeType 
}

case class DirectSchemeTypeRef(referencedType : SchemeType) extends SchemeTypeRef {
  def resolve(typeStack : SchemeType.Stack) : SchemeType =
    // No need to use the type stack
    referencedType
}

case class RecursiveSchemeTypeRef(stackDistance : Int) extends SchemeTypeRef {
  def resolve(typeStack : SchemeType.Stack) : SchemeType = {
    if (stackDistance >= typeStack.length) {
      throw new InvalidSchemeTypeRef("Reference to type outside of type stack: " + typeStack.length + ", " + stackDistance)
    }

    typeStack(stackDistance)
  }
}

