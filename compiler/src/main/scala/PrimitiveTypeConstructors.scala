package io.llambda.compiler
import io.llambda

/** Bindings for the primitive type constructors */
object PrimitiveTypeConstructors {
  object Union extends PrimitiveTypeConstructor

  val bindings = {
    Map[String, BoundValue](
      "U" -> Union
    )
  }
}

