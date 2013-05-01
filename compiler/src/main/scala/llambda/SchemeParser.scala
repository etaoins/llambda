package llambda

import scala.util.matching.Regex
import scala.util.parsing.combinator._

import scala.language.postfixOps

object SchemeParserDefinitions {
  // This is monsterous to exclude a single "." and starting with numbers
  val identifierPattern = """(([a-zA-Z\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_\.])""" +
                           """([a-zA-Z0-9\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_\.])+""" +
                              "|" +
                           """([a-zA-Z\!\$\%\&\*\+\-\/\:\<\=\>\?\@\^\_]))"""

  val hexEscapePattern = """\\x([0-9A-Za-z]+);"""
  val lineContinuationPattern = """\\\s*\n\s*"""
  val stringEscapePattern = "(" +
                              """\\(a|b|t|n|r|"|\|)""" + "|" +
                              hexEscapePattern + "|" + 
                              lineContinuationPattern + 
                            ")"

  def unescapeString(rawString : String) : String = stringEscapePattern.r.replaceAllIn(rawString, { matchData =>
    val HexEscape = hexEscapePattern.r
    val LineContinuation = lineContinuationPattern.r
    val EscapedQuote = "\\" + "\""

    matchData.matched match {
      case LineContinuation() => ""
      case other =>
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
    }
  })
  
  def intLiteralInBase(base : Int)(literalStr : String) =
    ast.IntegerLiteral(Integer.parseInt(literalStr, base))
}

object SchemeParser extends RegexParsers {
  case class Comment()

  import SchemeParserDefinitions._

  def apply(input : String) : ParseResult[List[ast.Datum]] = parseAll(program, input)

  def program = rep(datum | commentedDatum) ^^ { 
    // Remove all commented datums
    _.collect {
      case x : ast.Datum => x
    }
  }

  def datum : Parser[ast.Datum] = atom | list | quotedDatum | quasiquotedDatum | splicingUnquotedDatum | unquotedDatum

  def commentedDatum : Parser[Comment] = """#;""".r ~> (atom | list) ^^^ Comment() 

  def quotedDatum = "'" ~> datum ^^ { innerDatum => 
    ast.ProperList(List(ast.Symbol("quote"), innerDatum)) 
  }

  def quasiquotedDatum = "`" ~> datum ^^ { innerDatum =>
    ast.ProperList(List(ast.Symbol("quasiquote"), innerDatum)) 
  }

  def unquotedDatum =  "," ~> datum ^^ { innerDatum => 
    ast.ProperList(List(ast.Symbol("unquote"), innerDatum)) 
  }

  def splicingUnquotedDatum = ",@" ~> datum ^^ { innerDatum =>
    ast.ProperList(List(ast.Symbol("unquote-splicing"), innerDatum))
  }

  def atom : Parser[ast.Datum] = string | number | boolean | symbol | vector | bytevector | character 

  def boolean = trueLiteral | falseLiteral
  def trueLiteral = """#t(rue)?""".r ^^^ ast.TrueLiteral
  def falseLiteral = """#f(alse)?""".r ^^^ ast.FalseLiteral

  // Ignore the integer suffixes as we're not required to do anything with the storage hints
  def number = numberValue <~ opt("(?i)[sfdle]0".r)
  def numberValue = real | integer 
  
  def integer = binaryInteger | octalInteger | hexInteger | decimalInteger
  def binaryInteger = "#[bB]".r ~>"""-?[0-1]+""".r     ^^ intLiteralInBase(2)
  def octalInteger = "#[oO]".r ~> """-?[0-7]+""".r     ^^ intLiteralInBase(8)
  def decimalInteger = opt("#[dD]".r) ~>"""-?\d+""".r  ^^ intLiteralInBase(10)
  def hexInteger = "#[xX]".r ~> """-?[0-9a-fA-F]+""".r ^^ intLiteralInBase(16)
  
  def real = decimalReal | positiveInf | negativeInf | notANumber
  def decimalReal = opt("#[dD]".r) ~>  """-?\d+\.\d+""".r  ^^ { x => ast.RealLiteral(x.toDouble) }
  def positiveInf = """(?i)\+inf\.0""".r   ^^^ ast.PositiveInfinityLiteral
  def negativeInf = """(?i)-inf\.0""".r    ^^^ ast.NegativeInfinityLiteral
  def notANumber = """(?i)[+\-]nan\.0""".r ^^^ ast.NaNLiteral

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
    ast.ImproperList(head, terminator)
  }

  def vector = "#(" ~> rep(datum) <~ ")" ^^ { data =>
    ast.VectorLiteral(data.toVector) 
  }
  
  def bytevector = "#u8(" ~> rep("""\d+""".r) <~ ")" ^^ { byteStrs =>
    ast.ByteVector(byteStrs.map(Integer.parseInt(_)).toVector) 
  }

  def character = symbolicCharacter | hexScalarCharacter | literalSpace | literalCharacter

  def symbolicCharacter = symbolicAlarm | symbolicBackspace | symbolicDelete |
                          symbolicEscape | symbolicNewline | symbolicNull |
                          symbolicReturn | symbolicSpace | symbolicTab

  def symbolicAlarm =     """(?i)#\\alarm""".r     ^^^ ast.CharLiteral(0x07)
  def symbolicBackspace = """(?i)#\\backspace""".r ^^^ ast.CharLiteral(0x08)
  def symbolicDelete =    """(?i)#\\delete""".r    ^^^ ast.CharLiteral(0x7f)
  def symbolicEscape =    """(?i)#\\escape""".r    ^^^ ast.CharLiteral(0x1b)
  def symbolicNewline =   """(?i)#\\newline""".r   ^^^ ast.CharLiteral('\n')
  def symbolicNull =      """(?i)#\\null""".r      ^^^ ast.CharLiteral(0x00)
  def symbolicReturn =    """(?i)#\\return""".r    ^^^ ast.CharLiteral(0x0d)
  def symbolicSpace =     """(?i)#\\space""".r     ^^^ ast.CharLiteral(' ')
  def symbolicTab =       """(?i)#\\tab""".r       ^^^ ast.CharLiteral(0x09)

  def hexScalarCharacter = """(?i)#\\x[0-9a-z]+""".r ^^ { literalStr =>
    ast.CharLiteral(Integer.parseInt(literalStr.drop(3), 16).toChar)
  }

  // We need this explicitly so the parser doesn't treat it as whitespace
  def literalSpace = """#\ """ ^^^ ast.CharLiteral(' ')
  def literalCharacter = """#\""" ~> """.""".r ^^ { literalStr => ast.CharLiteral(literalStr.charAt(0)) }
  
  // Space, ; comments and #| |# comments are whitespace. Datum comments are handled by commentedDatum
  override protected val whiteSpace = """(\s|;.*(\n|$)|#\|(.|\n)*\|#)+""".r
}
