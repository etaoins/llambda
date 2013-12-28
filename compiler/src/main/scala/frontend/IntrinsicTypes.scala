package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.platform.TargetPlatform
import llambda.compiler.{valuetype => vt}

object IntrinsicTypes {
  private val cellTypes = IntrinsicCellTypes().mapValues(vt.IntrinsicCellType.apply)

  def apply(targetPlatform : TargetPlatform) : Map[String, vt.IntrinsicType] = 
    // Intrinsic native types
    List(
      vt.CBool,
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
      (nativeType.schemeName -> nativeType)
    }).toMap ++
    // Type aliases
    Map(
      ("<int>"     -> targetPlatform.intType),
      ("<long>"    -> targetPlatform.longType),
      ("<ushort>"  -> targetPlatform.ushortType),
      ("<uint>"    -> targetPlatform.uintType),
      ("<size_t>"  -> targetPlatform.sizeType),
      ("<wchar_t>" -> targetPlatform.wcharType)
    ) ++ cellTypes
}
