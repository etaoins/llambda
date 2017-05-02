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

case class DataAlignment(sizeBits: Int, abiBits: Int, preferredBits: Int)

case class DataLayout(
  endian: Endian = Endian.Big,
  mangling: Mangling = Mangling.ELF, // This isn't documented by LLVM. Every layout should override this anyway.
  stackAlignmentBits: Int = 0,
  nativeIntegerBits: Set[Int] = Set(8, 16, 32, 64), // This is also isn't documented
  pointerAlignment: DataAlignment = DataAlignment(64, 64, 64),
  aggregateAlignment: DataAlignment = DataAlignment(0, 0, 64),
  integerAlignments: Map[Int, DataAlignment] = Map(
    1 -> DataAlignment(1, 8, 8),
    8 -> DataAlignment(8, 8, 8),
    16 -> DataAlignment(16, 16, 16),
    32 -> DataAlignment(32, 32, 32),
    64 -> DataAlignment(64, 32, 64),
  ),
  floatAlignments: Map[Int, DataAlignment] = Map(
    16 -> DataAlignment(16, 16, 16),
    32 -> DataAlignment(32, 32, 32),
    64 -> DataAlignment(64, 64, 64),
    128 -> DataAlignment(128, 128, 128),
  ),
  vectorAlignments: Map[Int, DataAlignment] = Map(
    64 -> DataAlignment(64, 64, 64),
    128 -> DataAlignment(128, 128, 128),
  )
)

object DataAlignment {
  def fromSpecifier(specifier: String): DataAlignment = specifier.split(":").map(_.toInt) match {
    case Array(sizeBits) => DataAlignment(sizeBits, sizeBits, sizeBits)
    case Array(sizeBits, abiBits) => DataAlignment(sizeBits, abiBits, abiBits)
    case Array(sizeBits, abiBits, preferredBits) => DataAlignment(sizeBits, abiBits, preferredBits)
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
    val VectorAlignmentSpecification = """v(.+)""".r
    val NativeIntegerWidthsSpecification = """n(.+)""".r

    specifications.foldLeft(DataLayout()) {
      case (dataLayout, "E") =>
        dataLayout.copy(endian=Endian.Big)

      case (dataLayout, "e") =>
        dataLayout.copy(endian=Endian.Little)

      case (dataLayout, StackAlignmentSpecification(alignStr)) =>
        dataLayout.copy(stackAlignmentBits=alignStr.toInt)

      case (dataLayout, PointerAlignmentSpecification(specifier)) =>
        dataLayout.copy(pointerAlignment=DataAlignment.fromSpecifier(specifier))

      case (dataLayout, AggregagteAlignmentSpecification(abiStr, preferredStr)) =>
        val aggregateAlignment = DataAlignment(0, abiStr.toInt, preferredStr.toInt)
        dataLayout.copy(aggregateAlignment=aggregateAlignment)

      case (dataLayout, IntegerAlignmentSpecification(specifier)) =>
        val integerAlignment = DataAlignment.fromSpecifier(specifier)
        val newIntegerAlignments = dataLayout.integerAlignments + (integerAlignment.sizeBits -> integerAlignment)

        dataLayout.copy(integerAlignments=newIntegerAlignments)

      case (dataLayout, FloatAlignmentSpecification(specifier)) =>
        val floatAlignment = DataAlignment.fromSpecifier(specifier)
        val newFloatAlignments = dataLayout.floatAlignments + (floatAlignment.sizeBits -> floatAlignment)

        dataLayout.copy(floatAlignments=newFloatAlignments)

      case (dataLayout, VectorAlignmentSpecification(specifier)) =>
        val vectorAlignment = DataAlignment.fromSpecifier(specifier)
        val newVectorAlignments = dataLayout.vectorAlignments + (vectorAlignment.sizeBits -> vectorAlignment)

        dataLayout.copy(vectorAlignments=newVectorAlignments)

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
