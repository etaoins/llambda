package llambda.frontend

import llambda.nfi
import llambda.{valuetype => vt}

object IntrinsicTypes {
  def apply() : Map[String, vt.IntrinsicType] = 
    Map(
      ("<bool>"   -> nfi.CBool),
      ("<int8>"   -> nfi.Int8),
      ("<int16>"  -> nfi.Int16),
      ("<int32>"  -> nfi.Int32),
      ("<int64>"  -> nfi.Int64),
      ("<uint8>"  -> nfi.UInt8),
      ("<uint16>" -> nfi.UInt16),
      ("<uint32>" -> nfi.UInt32),
      ("<float>"  -> nfi.Float),
      ("<double>" -> nfi.Double),

      ("<utf8-cstring>" -> nfi.Utf8CString),

      ("<unicode-char>" -> nfi.UnicodeChar),

      // XXX: This assumes Unix-like LP64: 64bit Linux, FreeBSD, Mac OS X, etc 
      // These aliases are here so we can do the right thing when porting to other archs
      ("<short>"  -> nfi.Int16),
      ("<int>"    -> nfi.Int32),
      ("<long>"   -> nfi.Int64),
      ("<ushort>" -> nfi.UInt16),
      ("<uint>"   -> nfi.UInt32)
    ).mapValues(vt.ScalarType.apply) ++
    (IntrinsicBoxedTypes().mapValues(vt.BoxedIntrinsicType.apply))
}
