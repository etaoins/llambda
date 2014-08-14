package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.platform.TargetPlatform
import llambda.compiler.{valuetype => vt}

object IntrinsicTypes {
  def apply(targetPlatform : TargetPlatform) : Map[String, vt.ValueType] = 
    // Intrinsic native types
    List(
      vt.Predicate,
      vt.Int8,
      vt.Int16,
      vt.Int32,
      vt.Int64,
      vt.UInt8,
      vt.UInt16,
      vt.UInt32,
      vt.Float,
      vt.Double,
      vt.UnicodeChar
    ).map({nativeType =>
      (vt.NameForType(nativeType) -> nativeType)
    }).toMap ++
    // Type aliases
    Map(
      ("<native-int>"     -> targetPlatform.intType),
      ("<native-long>"    -> targetPlatform.longType),
      ("<native-ushort>"  -> targetPlatform.ushortType),
      ("<native-uint>"    -> targetPlatform.uintType),
      ("<native-size_t>"  -> targetPlatform.sizeType),
      ("<native-wchar_t>" -> targetPlatform.wcharType)
    ) ++ vt.IntrinsicSchemeTypes() +
    ("<any>" -> vt.AnySchemeType)
}
