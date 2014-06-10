package io.llambda.compiler
import io.llambda

import scala.annotation.tailrec

case class SourceLocation(
  filenameOpt : Option[String],
  sourceString : String,
  offset : Int,
  expandedFromOpt : Option[SourceLocated] = None
) {
  private lazy val lineColumn : (Int, Int) = {
    @tailrec
    def findLineColumn(cursor : Int = 0, line : Int = 1, column : Int = 1) : (Int, Int) =
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
}

abstract trait SourceLocated {
  var locationOpt : Option[SourceLocation] = None

  def hasLocation = locationOpt.isDefined

  def assignLocationTo(other : SourceLocated) {
    if (this.locationOpt.isDefined) {
      other.locationOpt = this.locationOpt
    }
  }
  
  def assignLocationFrom(other : SourceLocated) : SourceLocated.this.type = {
    if (other.locationOpt.isDefined) {
      this.locationOpt = other.locationOpt
    }
    this
  }

  private def sourceSnippet(location : SourceLocation) : String = {
    val sourceLines = location.sourceString.split('\n')
    val lineOfInterest = sourceLines(location.line - 1)

    // Happy ASCII arrow
    val columnArrow = (" " * (location.column - 1)) + "^"

    lineOfInterest + "\n" + columnArrow
  }

  def locationString : String = {
    locationOpt match {
      case Some(location) =>
        val selfString = location.filenameOpt.getOrElse("(unknown)") + ":" +
          location.line + ":\n" +
          sourceSnippet(location)

        location.expandedFromOpt match {
          case Some(expandedFrom) =>
            selfString + "\nExpanded from:\n" + expandedFrom.locationString

          case None =>
            selfString
        }

      case None =>
        ""
    }
  }
}

object NoSourceLocation extends SourceLocated
