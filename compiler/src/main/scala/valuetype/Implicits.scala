package io.llambda.compiler.valuetype


object Implicits {
  import scala.language.implicitConversions

  implicit def schemeType2Ref(schemeType: SchemeType): SchemeTypeRef =
    DirectSchemeTypeRef(schemeType)
}
