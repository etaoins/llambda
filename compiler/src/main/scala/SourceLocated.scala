package io.llambda.compiler

import scala.annotation.tailrec


case class SourceLocation(
  filenameOpt: Option[String],
  sourceString: String,
  offset: Int
) {
  private lazy val lineColumn: (Int, Int) = {
    @tailrec
    def findLineColumn(cursor: Int = 0, line: Int = 1, column: Int = 1): (Int, Int) =
      if (cursor == offset) {
        // We're done
        (line, column)
      }
      else if (sourceString.charAt(cursor) == '\n') {
        // New line
        findLineColumn(cursor + 1, line + 1, 1)
      }
      else {
        // New column
        findLineColumn(cursor + 1, line, column + 1)
      }

      findLineColumn()
  }

  def line =
    lineColumn._1

  def column =
    lineColumn._2

  private def sourceSnippet: String = {
    val sourceLines = sourceString.split('\n')
    val lineOfInterest = sourceLines(line - 1)

    // Happy ASCII arrow
    val columnArrow = (" " * (column - 1)) + "^"

    lineOfInterest + "\n" + columnArrow
  }

  def locationWithSnippetString =
    filenameOpt.getOrElse("(unknown)") + ":" +
      line + ":\n" +
      sourceSnippet

  def locationOnlyString =
    filenameOpt.getOrElse("(unknown)") + ":" + line

  override def toString =
    locationWithSnippetString
}

abstract trait SourceLocated {
  var locationOpt: Option[SourceLocation] = None

  def hasLocation = locationOpt.isDefined

  def assignLocationTo(other: SourceLocated) {
    if (this.locationOpt.isDefined) {
      other.locationOpt = this.locationOpt
    }
  }

  def assignLocationFrom(other: SourceLocated): SourceLocated.this.type = {
    if (other.locationOpt.isDefined) {
      this.locationOpt = other.locationOpt
    }
    this
  }

  def locationString: String = {
    locationOpt.map(_.toString).getOrElse("")
  }
}

object NoSourceLocation extends SourceLocated

sealed abstract class InlineReason
object InlineReason {
  case object MacroExpansion extends InlineReason
  case object LambdaInline extends InlineReason
}

case class InlinePathEntry(
  contextOpt: Option[debug.SourceContext],
  locationOpt: Option[SourceLocation],
  inlineReason: InlineReason
)

abstract trait ContextLocated extends SourceLocated {
  var contextOpt: Option[debug.SourceContext] = None
  var inlinePath: List[InlinePathEntry] = Nil

  override def assignLocationTo(other: SourceLocated) {
    super.assignLocationTo(other)

    other match {
      case otherSpLocated: ContextLocated =>
        if (this.contextOpt.isDefined) {
          otherSpLocated.contextOpt = this.contextOpt
          otherSpLocated.inlinePath = this.inlinePath
        }

      case _ =>
    }
  }

  def assignLocationAndContextFrom(other: SourceLocated, context: debug.SourceContext): ContextLocated.this.type = {
    assignLocationFrom(other)
    this.contextOpt = Some(context)

    this
  }

  override def assignLocationFrom(other: SourceLocated): ContextLocated.this.type = {
    super.assignLocationFrom(other)

    other match {
      case otherSpLocated: ContextLocated =>
        if (otherSpLocated.contextOpt.isDefined) {
          this.contextOpt = otherSpLocated.contextOpt
          this.inlinePath = otherSpLocated.inlinePath
        }

      case _ =>
    }

    this
  }

  override def locationString: String = {
    val locationPathParts = super.locationString :: inlinePath.collect {
      case InlinePathEntry(_, Some(location), InlineReason.MacroExpansion) =>
        "Expanded from:\n" + location.toString

      case InlinePathEntry(_, Some(location), InlineReason.LambdaInline) =>
        "Inlined from:\n" + location.toString
    }

    locationPathParts.mkString("\n")
  }
}

object NoContextLocation extends ContextLocated
