package io.llambda.compiler.valuetype
import io.llambda

object SubtractTypes {
  private[valuetype] def stackedSubtractTypes(
      inputStack: SchemeType.Stack,
      removingType: SchemeType
  ): SchemeType = (inputStack.head, removingType) match {
    case (UnionType(inputMemberTypes), _) =>
      val remainingMembers = inputMemberTypes map { inputMemberType =>
        stackedSubtractTypes(inputMemberType :: inputStack, removingType)
      }

      val unrolledMembers = if (inputMemberTypes == remainingMembers) {
        // Don't need to unroll
        remainingMembers
      }
      else {
        remainingMembers.map(inputStack.head.unrollChildType)
      }

      SchemeType.fromTypeUnion(unrolledMembers)

    case (BooleanType, LiteralBooleanType(value)) =>
      // There are only two boolean values - special case this
      LiteralBooleanType(!value)

    case _ if SatisfiesType.stackedSatisfiesType(removingType :: Nil, inputStack) == Some(true) =>
      // No type remains
      EmptySchemeType

    case _ =>
      inputStack.head
  }

  /** Subtracts one type from another
    *
    * Subtracting a type removes the part of inputType that satisfies the removingType. For example,
    * (U <symbol> <string>) minus (U <symbol> <pair>) is <string>.
    *
    * @param  inputType     Type to subtract from
    * @param  removingType  Type to subtract from inputType
    */
  def apply(inputType: SchemeType, removingType: SchemeType): SchemeType = {
    stackedSubtractTypes(inputType :: Nil, removingType)
  }
}
