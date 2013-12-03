package llambda.frontend

import llambda.{valuetype => vt}

object IntrinsicTypes {
  def apply() : Map[String, vt.IntrinsicType] = 
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

      ("<utf8-cstring>" -> vt.Utf8CString),

      ("<unicode-char>" -> vt.UnicodeChar),

      // XXX: This assumes Unix-like LP64: 64bit Linux, FreeBSD, Mac OS X, etc 
      // These aliases are here so we can do the right thing when porting to other archs
      ("<short>"  -> vt.Int16),
      ("<int>"    -> vt.Int32),
      ("<long>"   -> vt.Int64),
      ("<ushort>" -> vt.UInt16),
      ("<uint>"   -> vt.UInt32)
    ) ++
    (IntrinsicCellTypes().mapValues(vt.IntrinsicCellType.apply))
}
