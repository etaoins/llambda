package llambda

import scala.util.parsing.input.{Positional, NoPosition}

abstract trait SourceLocated extends Positional {
  var filename : Option[String] = None

  def setFilename(newFilename : Option[String]) : SourceLocated.this.type = {
    filename = newFilename
    this
  }

  def assignLocationTo(other : SourceLocated) {
    other.setPos(pos)
    other.setFilename(filename)
  }

  def locationString : String = {
    val filenameString = filename getOrElse "(unknown)"

    val positionString = if (pos eq NoPosition) {
      "(unknown)"
    }
    else {
      s"${pos.toString}:\n${pos.longString}"
    }

    s"${filenameString}:${positionString}"
  }
}

object NoSourceLocation extends SourceLocated
