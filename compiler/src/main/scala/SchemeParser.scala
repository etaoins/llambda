package io.llambda.compiler
import io.llambda

import scala.language.implicitConversions
import scala.annotation.switch

import scala.io.Source
import java.io.File
import scala.util.{Try,Success,Failure}
  
import org.parboiled2._

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
}

class SchemeParser(sourceString : String, filenameOpt : Option[String]) extends Parser with StringBuilding {
  import CharPredicate._

  val input : ParserInput = sourceString

  // Used to datum location tracking
  private var currentLocationOpt : Option[SourceLocation] = None

  private def updateLocation : Rule0 = rule {
    run {
      currentLocationOpt = Some(
        SourceLocation(
          filenameOpt=filenameOpt,
          sourceString=sourceString,
          offset=cursor
        )
      )
      ()
    }
  }

  private def located(datum : ast.Datum) : ast.Datum = {
    datum.locationOpt = Some(
      SourceLocation(
        filenameOpt=filenameOpt,
        sourceString=sourceString,
        offset=cursor
      )
    )

    datum
  }

  private val NonPeriodExtendedIdentifierChar = CharPredicate("!$%&*+-/:<=>?@^_~")
  private val ExtendedIdentifierChar = NonPeriodExtendedIdentifierChar ++ CharPredicate('.')
  private val IdentifierChar = Alpha ++ Digit ++ ExtendedIdentifierChar

  private val BinaryDigit = CharPredicate("01")
  private val OctalDigit = CharPredicate("01234567")

  private val SignCharacter = CharPredicate("+-")

  private val IntralineWhitespaceChar = CharPredicate(" \t\r")
  private val NewlineChar = CharPredicate("\n")
  private val WhitespaceChar = IntralineWhitespaceChar ++ NewlineChar
  
  // Whitespace handling
  def Whitespace : Rule0 = rule {
    zeroOrMore(WhitespaceChar | LineComment | DatumComment | BlockComment)
  }

  def LineComment = rule {
    ';' ~ zeroOrMore(noneOf("\n")) 
  }

  def DatumComment = rule {
    str("#;") ~ zeroOrMore(WhitespaceChar) ~ Datum ~> ({ _ =>
      ()
    })
  }

  def BlockComment = rule {
    str("#|") ~ UnclosedBlockComment
  }

  def UnclosedBlockComment : Rule0 = rule {
    zeroOrMore(noneOf("|")) ~ (str("|#") | (ANY ~ UnclosedBlockComment))
  }

  implicit def wspStr(s: String): Rule0 = rule {
    str(s) ~ Whitespace
  }

  def Data = rule {
    Whitespace ~ zeroOrMore(Datum) ~ EOI 
  }

  // Split datums in to ones starting with '#', '(' and everything else
  // This is to speed up the parser
  def Datum : Rule1[ast.Datum] = rule {
    updateLocation ~ (run {
      (cursorChar : @switch) match {
        case '#' => 
          OctoDatum

        case '(' =>
          ListDatum

        case '|' =>
          EnclosedSymbol

        case '"' =>
          StringDatum

        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          // This isn't the only way to Number - they have quite a complex definition in R7RS
          // This is just a parser shortcut
          UnradixedDecimalNumber

        case '\'' =>
          QuotedDatum
        
        case '`' =>
          QuasiquotedDatum
        
        case ',' =>
          UnquotedSplicingDatum | UnquotedDatum

        case _ =>
          OtherDatum
      }
    } ~> { unlocatedDatum =>
      unlocatedDatum.locationOpt = currentLocationOpt
      unlocatedDatum
    })
  }

  def OctoDatum = rule {
    UnitDatum | BooleanDatum | VectorDatum | RadixedNumber | BytevectorDatum | CharDatum
  }

  // Quotations
  def QuotedDatum = rule {
    "'" ~ Datum ~> { datum =>
      ast.ProperList(List(ast.Symbol("quote"), datum))
    }
  }

  def QuasiquotedDatum = rule {
    "`" ~ Datum ~> { datum =>
      ast.ProperList(List(ast.Symbol("quasiquote"), datum))
    }
  }
  
  def UnquotedSplicingDatum = rule {
    ",@" ~ Datum ~> { datum =>
      ast.ProperList(List(ast.Symbol("unquote-splicing"), datum))
    }
  }
  
  def UnquotedDatum = rule {
    "," ~ Datum ~> { datum =>
      ast.ProperList(List(ast.Symbol("unquote"), datum))
    }
  }

  // UnradixedDecimalNumber must come first:
  // "An identifier is any sequence of (...)  provided that it does not have a prefix which is a valid number"
  def OtherDatum = rule {
    UnradixedDecimalNumber | UnenclosedSymbol
  }

  // Lists
  def ListDatum = rule {
    ProperList | ImproperList
  }

  def ProperList = rule {
    "(" ~ zeroOrMore(Datum) ~ ")" ~> (ast.ProperList(_))
  }
  
  def ImproperList = rule {
    "(" ~ zeroOrMore(Datum) ~ "." ~ Datum ~ ")" ~> ({ (head, terminator) =>
      ast.AnyList(head, terminator)
    })
  }

  // Decimal numbers
  def UnradixedDecimalNumber = rule {
    (RealDatum | IntegerDatum) ~ Whitespace
  }

  def RealDatum = rule {
    RationalDatum | PositiveInfinity | NegativeInfinity | NaNDatum
  }

  def RationalDatum = rule {
    capture(optional(SignCharacter) ~ oneOrMore(Digit) ~ optional('.') ~ oneOrMore(Digit)) ~> ({ number =>
      ast.RationalLiteral(number.toDouble)
    })
  }

  def PositiveInfinity = rule {
    ignoreCase("+inf.0") ~ push(ast.RationalLiteral(Double.PositiveInfinity))
  }

  def NegativeInfinity = rule {
    ignoreCase("-inf.0") ~ push(ast.RationalLiteral(Double.NegativeInfinity))
  }

  def NaNDatum = rule {
    SignCharacter ~ ignoreCase("nan.0") ~ push(ast.RationalLiteral(Double.NaN))
  }

  def IntegerDatum = rule {
    UnradixedExactInteger ~> (ast.IntegerLiteral(_)) 
  }

  def UnradixedExactInteger = rule {
    capture(optional(SignCharacter) ~ oneOrMore(Digit)) ~ optional('.') ~> ({ number =>
      java.lang.Long.parseLong(number)
    })
  }

  // Non-decimal numbers
  def RadixedNumber = rule {
    (RadixedNonDecimal | RadixedDecimalNumber) ~ Whitespace
  }

  def RadixedNonDecimal = rule {
    RadixedExactInteger ~> (ast.IntegerLiteral(_))
  }


  def RadixedExactInteger = rule {
    BinaryInteger | OctalInteger | HexInteger
  }

  def BinaryInteger = rule {
    ignoreCase("#b") ~ capture(optional(SignCharacter) ~ oneOrMore(BinaryDigit)) ~> ({ number =>
      java.lang.Long.parseLong(number, 2)
    })
  }
  
  def OctalInteger = rule {
    ignoreCase("#o") ~ capture(optional(SignCharacter) ~ oneOrMore(OctalDigit)) ~> ({ number =>
      java.lang.Long.parseLong(number, 8)
    })
  }

  def RadixedDecimalNumber = rule {
    ignoreCase("#d") ~ UnradixedDecimalNumber
  }

  def HexInteger = rule {
    ignoreCase("#x") ~ capture(optional(SignCharacter) ~ oneOrMore(HexDigit)) ~> ({ number =>
      java.lang.Long.parseLong(number, 16)
    })
  }

  // Vectors
  def VectorDatum = rule {
    "#(" ~ zeroOrMore(Datum) ~ ")" ~> ({ elements =>
      ast.VectorLiteral(elements.toVector)
    })
  }

  // Unit
  def UnitDatum = rule {
    "#!unit" ~ push(ast.UnitValue())
  }

  // Booleans
  def BooleanDatum = rule {
    BooleanTrue | BooleanFalse
  }

  def BooleanTrue =  rule {
    ("#true" | "#t")  ~ push(ast.BooleanLiteral(true))
  }
  
  def BooleanFalse = 
    rule { ("#false" | "#f") ~ push(ast.BooleanLiteral(false)) }

  // All backslash escaped chars
  def EscapedChar = rule {
    '\\' ~ appendSB('\\') |
    'a' ~ appendSB(0x07.toChar) |
    'b' ~ appendSB(0x08.toChar) |
    't' ~ appendSB(0x09.toChar) |
    'n' ~ appendSB(0x0a.toChar) |
    'r' ~ appendSB(0x0d.toChar) |
    '"' ~ appendSB(0x22.toChar) |
    '|' ~ appendSB(0x7c.toChar) |
    'x' ~ capture(zeroOrMore(HexDigit)) ~ ';' ~> { hexNumber =>
      appendSB(Integer.parseInt(hexNumber, 16).toChar)
      ()
    } |
    zeroOrMore(IntralineWhitespaceChar) ~ '\n' ~ zeroOrMore(IntralineWhitespaceChar) // Line continuation
  }

  // Unquoted symbols
  def UnenclosedSymbol = rule {
    (MultiCharSymbol | SingleCharSymbol) ~ Whitespace
  }

  def SingleCharSymbol = rule {
    capture(NonPeriodExtendedIdentifierChar ++ Alpha) ~> (ast.Symbol(_))
  }

  def MultiCharSymbol = rule {
    capture(IdentifierChar ~ oneOrMore(IdentifierChar)) ~> (ast.Symbol(_))
  }

  // Enclosed symbols
  def EnclosedSymbol = rule {
    '|' ~ clearSB() ~ zeroOrMore(EnclosedSymbolChar) ~ '|' ~ Whitespace ~ push(ast.Symbol(sb.toString))
  }

  def EnclosedSymbolChar = rule {
    ('\\' ~ EscapedChar) | (noneOf("|") ~ appendSB())
  }
  
  // Strings
  def StringDatum = rule {
    '"' ~ clearSB() ~ zeroOrMore(StringChar) ~ '"' ~ Whitespace ~ push(ast.StringLiteral(sb.toString))
  }

  def StringChar = rule {
    ('\\' ~ EscapedChar) | (noneOf("\"") ~ appendSB())
  }
  
  // Bytevectors
  def BytevectorDatum = rule {
    "#u8(" ~ zeroOrMore(Byte) ~ ")" ~> ({ elements =>
      ast.Bytevector(elements.map(_.toShort).toVector)
    })
  }

  def Byte = rule {
    (RadixedExactInteger | UnradixedExactInteger | (ignoreCase("#d") ~ UnradixedExactInteger)) ~ Whitespace
  }

  // Characters
  def CharDatum = rule {
    str("#\\") ~ CharBody
  }

  def CharBody = rule {
    """alarm"""     ~ push(ast.CharLiteral(0x07)) |
    """backspace""" ~ push(ast.CharLiteral(0x08)) |
    """delete"""    ~ push(ast.CharLiteral(0x7f)) |
    """escape"""    ~ push(ast.CharLiteral(0x1b)) |
    """newline"""   ~ push(ast.CharLiteral('\n')) |
    """null"""      ~ push(ast.CharLiteral(0x00)) |
    """return"""    ~ push(ast.CharLiteral(0x0d)) |
    """space"""     ~ push(ast.CharLiteral(' ')) |
    """tab"""       ~ push(ast.CharLiteral(0x09)) |
    ignoreCase("x") ~ capture(oneOrMore(HexDigit)) ~ Whitespace ~> ({ hexCode =>
      ast.CharLiteral(Integer.parseInt(hexCode, 16).toChar)
    }) |
    capture(Digit) ~ Whitespace ~> ({ literalCharString =>
      ast.CharLiteral(literalCharString.charAt(0))
    }) |
    capture(ANY) ~ !UnenclosedSymbol ~ Whitespace ~> ({ literalCharString =>
      ast.CharLiteral(literalCharString.charAt(0))
    })
  }
}

object SchemeParser {
  def parseFileAsData(input : File) : List[ast.Datum] = {
    val filename = input.getAbsolutePath 
    val inputString = Source.fromFile(input, "UTF-8").mkString

    parseStringAsData(inputString, Some(filename))
  }

  def parseStringAsData(input : String, filenameOpt : Option[String] = None) : List[ast.Datum] = {
    val parser = new SchemeParser(input, filenameOpt)

    parser.Data.run() match {
      case Success(data) =>
        data.toList

      case Failure(parseError : ParseError) =>
        throw new ParseErrorException(filenameOpt, parser.formatError(parseError))

      case Failure(throwable)=>
        throw throwable
    }
  }
}
