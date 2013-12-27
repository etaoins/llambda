package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler.platform.TargetPlatform
import llambda.compiler.{valuetype => vt}

object IntrinsicTypes {
  private val cellTypes = IntrinsicCellTypes().mapValues(vt.IntrinsicCellType.apply)

  def apply(targetPlatform : TargetPlatform) : Map[String, vt.IntrinsicType] = 
    Map(
      ("<bool>"   -> vt.CBool),
      ("<int8>"   -> vt.Int8),
      ("<int16>"  -> vt.Int16),
      ("<int32>"  -> vt.Int32),
      ("<int64>"  -> vt.Int64),
      ("<uint8>"  -> vt.UInt8),
      ("<uint16>" -> vt.UInt16),
      ("<uint32>" -> vt.UInt32),
      ("<float>"  -> vt.Float),
      ("<double>" -> vt.Double),

      ("<unicode-char>" -> vt.UnicodeChar),

      ("<short>"   -> targetPlatform.shortType),
      ("<int>"     -> targetPlatform.intType),
      ("<long>"    -> targetPlatform.longType),
      ("<ushort>"  -> targetPlatform.ushortType),
      ("<uint>"    -> targetPlatform.uintType),
      ("<size_t>"  -> targetPlatform.sizeType),
      ("<wchar_t>" -> targetPlatform.wcharType)
    ) ++ cellTypes
}
