package io.llambda.compiler
import io.llambda

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.io.Source
import java.io.File

import scala.language.postfixOps

class ParseErrorException(val filename : Option[String], val message : String) extends
  Exception(filename.getOrElse("(unknown)") + ": " + message)

object SchemeParserDefinitions {
  // These are the characters that can appear in an identifier
  val identifierCharacterPattern= """[a-zA-Z0-9\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_\.]"""

  // This is monsterous to exclude a single "." and starting with numbers
  val identifierPattern = """(([a-zA-Z\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_\.])""" +
                           identifierCharacterPattern + "+" +
                              "|" +
                           """([a-zA-Z\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_]))"""

  val hexEscapePattern = """\\x([0-9A-Za-z]+);"""
  val lineContinuationPattern = """\\\s*\n\s*"""
  val stringEscapePattern = "(" +
                              """\\(a|b|t|n|r|"|\\|\|)""" + "|" +
                              hexEscapePattern + "|" + 
                              lineContinuationPattern + 
                            ")"
    
  // Only build these once
  private val HexEscape = hexEscapePattern.r
  private val LineContinuation = lineContinuationPattern.r
  private val EscapedQuote = "\\" + "\""
  private val HexCharPattern = """#\\(?i)x([0-9a-z]+)"""r

  def unescapeString(rawString : String) : String = stringEscapePattern.r.replaceAllIn(rawString, { matchData =>
    matchData.matched match {
      case LineContinuation() => ""
      case other =>
        Regex.quoteReplacement(
          (other match {
            case """\a""" => 0x07
            case """\b""" => 0x08
            case """\t""" => 0x09
            case """\n""" => 0x0a
            case """\r""" => 0x0d
            case EscapedQuote => 0x22
            case """\\""" => 0x5c
            case """\|""" => 0x7c
            case HexEscape(value) => Integer.parseInt(value, 16) 
          }).toChar.toString
        )
    }
  })
  
  def intLiteralInBase(base : Int)(literalStr : String) = {
    // Scheme allows trailing dots on exact integers
    val withoutTrailingDot = """\.$""".r.replaceFirstIn(literalStr, "")
    ast.IntegerLiteral(java.lang.Long.parseLong(withoutTrailingDot, base))
  }

  val parseCharacterLiteral : PartialFunction[String, ast.CharLiteral] = {
    case """#\alarm"""     => ast.CharLiteral(0x07)
    case """#\backspace""" => ast.CharLiteral(0x08)
    case """#\delete"""    => ast.CharLiteral(0x7f)
    case """#\escape"""    => ast.CharLiteral(0x1b)
    case """#\newline"""   => ast.CharLiteral('\n')
    case """#\null"""      => ast.CharLiteral(0x00)
    case """#\return"""    => ast.CharLiteral(0x0d)
    case """#\space"""     => ast.CharLiteral(' ')
    case """#\tab"""       => ast.CharLiteral(0x09)

    case HexCharPattern(hexits) =>
      ast.CharLiteral(Integer.parseInt(hexits, 16).toChar)

    case literalChar if literalChar.length == 3 =>
      ast.CharLiteral(literalChar.charAt(2))
  }
}

class SchemeParser(filename : Option[String]) extends RegexParsers {
  case class Comment()

  import SchemeParserDefinitions._

  def apply(input : String) : ParseResult[List[ast.Datum]] = parseAll(program, input)

  def program = rep(datum | commentedDatum) ^^ { 
    // Remove all commented datums
    _.collect {
      case x : ast.Datum => x
    }
  }

  def datum : Parser[ast.Datum] = positioned(atom | list | quotedDatum | quasiquotedDatum | splicingUnquotedDatum | unquotedDatum) ^^ { innerDatum =>
    // Update our filename for our source location
    // The "positioned" above will set the line/column for us
    innerDatum.setFilename(filename)
  }

  def commentedDatum : Parser[Comment] = """#;""".r ~> (atom | list) ^^^ Comment() 

  def quotedDatum = "'" ~> datum ^^ { innerDatum => 
    val syntheticSymbol = ast.Symbol("quote")
    // This ensures our synthetic symbol as a source location for error messages
    innerDatum.assignLocationTo(syntheticSymbol)

    ast.ProperList(List(syntheticSymbol, innerDatum)) 
  }

  def quasiquotedDatum = "`" ~> datum ^^ { innerDatum =>
    val syntheticSymbol = ast.Symbol("quasiquote")
    innerDatum.assignLocationTo(syntheticSymbol)
    
    ast.ProperList(List(syntheticSymbol, innerDatum)) 
  }

  def unquotedDatum =  "," ~> datum ^^ { innerDatum => 
    val syntheticSymbol = ast.Symbol("unquote")
    innerDatum.assignLocationTo(syntheticSymbol)
    
    ast.ProperList(List(syntheticSymbol, innerDatum)) 
  }

  def splicingUnquotedDatum = ",@" ~> datum ^^ { innerDatum =>
    val syntheticSymbol = ast.Symbol("unquote-splicing")
    innerDatum.assignLocationTo(syntheticSymbol)

    ast.ProperList(List(syntheticSymbol, innerDatum))
  }

  def atom : Parser[ast.Datum] = string | number | boolean | symbol | vector | bytevector | character | unit

  def boolean = trueLiteral | falseLiteral
  def trueLiteral = """#t(rue)?""".r ^^^ ast.BooleanLiteral(true)
  def falseLiteral = """#f(alse)?""".r ^^^ ast.BooleanLiteral(false)

  // Ignore the integer suffixes as we're not required to do anything with the storage hints
  def number = numberValue <~ opt("(?i)[sfdle]0".r)
  def numberValue = real | integer 
  
  def integer = binaryInteger | octalInteger | hexInteger | decimalInteger
  def binaryInteger = "#[bB]".r ~>"""-?[0-1]+""".r     ^^ intLiteralInBase(2)
  def octalInteger = "#[oO]".r ~> """-?[0-7]+""".r     ^^ intLiteralInBase(8)
  def decimalInteger = opt("#[dD]".r) ~>"""-?\d+\.?""".r  ^^ intLiteralInBase(10)
  def hexInteger = "#[xX]".r ~> """-?[0-9a-fA-F]+""".r ^^ intLiteralInBase(16)
  
  def real = decimalReal | positiveInf | negativeInf | notANumber
  def decimalReal = opt("#[dD]".r) ~>  """-?\d+\.\d+""".r  ^^ { x => ast.RationalLiteral(x.toDouble) }
  def positiveInf = """(?i)\+inf\.0""".r   ^^^ ast.PositiveInfinityLiteral()
  def negativeInf = """(?i)-inf\.0""".r    ^^^ ast.NegativeInfinityLiteral()
  def notANumber = """(?i)[+\-]nan\.0""".r ^^^ ast.NaNLiteral()

  def string = ("\"" + """([^"\\]|""" + stringEscapePattern + """)*""" + "\"").r  ^^ { rawString => 
    ast.StringLiteral(unescapeString(rawString.drop(1).dropRight(1)))
  }

  def symbol = directSymbol | enclosedSymbol

  def directSymbol = identifierPattern.r ^^ { ast.Symbol(_) }
  def enclosedSymbol = ("""\|([^\|\\]|""" + stringEscapePattern + """)*\|""").r ^^ { rawIdentifier =>
    ast.Symbol(unescapeString(rawIdentifier.drop(1).dropRight(1)))
  }

  def list = properList | improperList

  def properList = "(" ~> rep(datum | commentedDatum) <~ ")" ^^ { values =>
    // Filter out comments
    ast.ProperList(values.collect {
      case x : ast.Datum => x
    })
  }

  def improperList = "(" ~> rep1(datum) ~ "." ~ datum <~ ")" ^^ { case head ~ _  ~ terminator =>
    ast.AnyList(head, terminator)
  }

  def vector = "#(" ~> rep(datum) <~ ")" ^^ { data =>
    ast.VectorLiteral(data.toVector) 
  }
  
  def bytevector = "#u8(" ~> rep("""\d+""".r) <~ ")" ^^ { byteStrs =>
    ast.Bytevector(byteStrs.map(Integer.parseInt(_).toShort).toVector) 
  }

  def character = ("""#\\(""" + identifierPattern + """|.?)""").r ^? parseCharacterLiteral 

  def unit = "#!unit" ^^^ ast.UnitValue()

  // Space, ; comments and #| |# comments are whitespace. Datum comments are handled by commentedDatum
  override protected val whiteSpace = """(\s|;.*(\n|$)|#\|(.|\n)*?\|#)+""".r
}

object SchemeParser {
  def parseFileAsData(input : File) : List[ast.Datum] = {
    val filename = input.getAbsolutePath 
    val inputString = Source.fromFile(input, "UTF-8").mkString

    parseStringAsData(inputString, Some(filename))
  }

  def parseStringAsData(input : String, filename : Option[String] = None) : List[ast.Datum] = {
    val parser = new SchemeParser(filename)

    parser(input) match {
      case parser.Success(data, _) => data
      case err =>
        throw new ParseErrorException(filename, err.toString)
    }
  }
}
