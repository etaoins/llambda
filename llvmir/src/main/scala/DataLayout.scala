package io.llambda.llvmir


sealed class DataLayoutParseErrorException(message: String) extends Exception(message)

sealed abstract class Endian

object Endian {
  case object Big extends Endian
  case object Little extends Endian
}

sealed abstract class Mangling

object Mangling {
  case object ELF extends Mangling
  case object MIPS extends Mangling
  case object MachO extends Mangling
  case object WinCOFF extends Mangling
  case object Win86COFF extends Mangling
}

case class TypeLayout(sizeBits: Int, abiAlignmentBits: Int, preferredAlignmentBits: Int)

case class DataLayout(
  endian: Endian = Endian.Big,
  mangling: Mangling = Mangling.ELF, // This isn't documented by LLVM. Every layout should override this anyway.
  stackAlignmentBits: Int = 0,
  nativeIntegerBits: Set[Int] = Set(8, 16, 32, 64), // This is also isn't documented
  pointerLayout: TypeLayout = TypeLayout(64, 64, 64),
  aggregateLayout: TypeLayout = TypeLayout(0, 0, 64),
  integerLayouts: Map[Int, TypeLayout] = Map(
    1 -> TypeLayout(1, 8, 8),
    8 -> TypeLayout(8, 8, 8),
    16 -> TypeLayout(16, 16, 16),
    32 -> TypeLayout(32, 32, 32),
    64 -> TypeLayout(64, 32, 64),
  ),
  floatLayouts: Map[Int, TypeLayout] = Map(
    16 -> TypeLayout(16, 16, 16),
    32 -> TypeLayout(32, 32, 32),
    64 -> TypeLayout(64, 64, 64),
    128 -> TypeLayout(128, 128, 128),
  ),
  vectorLayouts: Map[Int, TypeLayout] = Map(
    64 -> TypeLayout(64, 64, 64),
    128 -> TypeLayout(128, 128, 128),
  )
)

object TypeLayout {
  def fromSpecifier(specifier: String): TypeLayout = specifier.split(":").map(_.toInt) match {
    case Array(sizeBits) => TypeLayout(sizeBits, sizeBits, sizeBits)
    case Array(sizeBits, abiBits) => TypeLayout(sizeBits, abiBits, abiBits)
    case Array(sizeBits, abiBits, preferredBits) => TypeLayout(sizeBits, abiBits, preferredBits)
    case _ =>
      throw new DataLayoutParseErrorException(s"Unknown alignment specifier: ${specifier}")
  }
}

object DataLayout {
  def fromDataLayoutString(str: String): DataLayout = {
    val specifications = str.split("-")

    val StackAlignmentSpecification = """S(\d+)""".r
    val PointerAlignmentSpecification = """p0?:(.+)""".r
    val AggregagteAlignmentSpecification = """a:(\d+):(\d+)""".r
    val IntegerAlignmentSpecification = """i(.+)""".r
    val FloatAlignmentSpecification = """f(.+)""".r
    val VectorLayoutSpecification = """v(.+)""".r
    val NativeIntegerWidthsSpecification = """n(.+)""".r

    specifications.foldLeft(DataLayout()) {
      case (dataLayout, "E") =>
        dataLayout.copy(endian=Endian.Big)

      case (dataLayout, "e") =>
        dataLayout.copy(endian=Endian.Little)

      case (dataLayout, StackAlignmentSpecification(alignStr)) =>
        dataLayout.copy(stackAlignmentBits=alignStr.toInt)

      case (dataLayout, PointerAlignmentSpecification(specifier)) =>
        dataLayout.copy(pointerLayout=TypeLayout.fromSpecifier(specifier))

      case (dataLayout, AggregagteAlignmentSpecification(abiStr, preferredStr)) =>
        val aggregateLayout = TypeLayout(0, abiStr.toInt, preferredStr.toInt)
        dataLayout.copy(aggregateLayout=aggregateLayout)

      case (dataLayout, IntegerAlignmentSpecification(specifier)) =>
        val integerLayout = TypeLayout.fromSpecifier(specifier)
        val newIntegerLayouts = dataLayout.integerLayouts + (integerLayout.sizeBits -> integerLayout)

        dataLayout.copy(integerLayouts=newIntegerLayouts)

      case (dataLayout, FloatAlignmentSpecification(specifier)) =>
        val floatLayout = TypeLayout.fromSpecifier(specifier)
        val newFloatLayouts = dataLayout.floatLayouts + (floatLayout.sizeBits -> floatLayout)

        dataLayout.copy(floatLayouts=newFloatLayouts)

      case (dataLayout, VectorLayoutSpecification(specifier)) =>
        val vectorLayout = TypeLayout.fromSpecifier(specifier)
        val newVectorLayouts = dataLayout.vectorLayouts + (vectorLayout.sizeBits -> vectorLayout)

        dataLayout.copy(vectorLayouts=newVectorLayouts)

      case (dataLayout, "m:e") =>
        dataLayout.copy(mangling=Mangling.ELF)

      case (dataLayout, "m:m") =>
        dataLayout.copy(mangling=Mangling.MIPS)

      case (dataLayout, "m:o") =>
        dataLayout.copy(mangling=Mangling.MachO)

      case (dataLayout, "m:w") =>
        dataLayout.copy(mangling=Mangling.WinCOFF)

      case (dataLayout, "m:x") =>
        dataLayout.copy(mangling=Mangling.Win86COFF)

      case (dataLayout, "A0") =>
        // We don't support multiple address spaces
        dataLayout

      case (dataLayout, NativeIntegerWidthsSpecification(specifier)) =>
        dataLayout.copy(nativeIntegerBits=specifier.split(":").map(_.toInt).toSet)

      case (dataLayout, _) =>
        // Unknown specifier; ignore
        dataLayout
    }
  }
}
