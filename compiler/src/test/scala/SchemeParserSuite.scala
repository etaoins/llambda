package io.llambda.compiler
import io.llambda

import org.scalatest.{FunSuite, Inside}
import llambda.compiler.SchemeStringImplicits._

class SchemeParserSuite extends FunSuite with Inside {
  // This checks:
  // 1) parsed source === expected
  // 2) parse((parsed source).toString) === expected
  def assertReflexiveParse(source : String, expected : List[ast.Datum]) {
    val parsed = SchemeParser.parseStringAsData(source)
    assert(parsed === expected)

    // Convert the parsed data back to a string to make sure .toString works
    val parsedAsSource = parsed.map(_.toString).mkString("\n")

    val reparsed = SchemeParser.parseStringAsData(parsedAsSource)
    assert(reparsed === expected)
  }

  def assertParsesAsSymbol(string : String, identifier : String) {
    assertReflexiveParse(string, List(ast.Symbol(identifier)))
  }
  
  def assertParsesAsSymbol(string : String) {
    assertParsesAsSymbol(string, string)
  }

  def testSymbolShorthand(shorthand : String, symbolName : String) {
    assertReflexiveParse(shorthand + "foo",
      ast.ProperList(List(ast.Symbol(symbolName), ast.Symbol("foo"))) :: Nil
    )

    assertReflexiveParse(shorthand + "(1 . 2)",
      ast.ProperList(List(ast.Symbol(symbolName), ast.Pair(ast.IntegerLiteral(1), ast.IntegerLiteral(2)))) :: Nil
    )
    
    assertReflexiveParse(shorthand + "(real? 1.0)",
      ast.ProperList(List(ast.Symbol(symbolName), ast.ProperList(List(ast.Symbol("real?"), ast.RationalLiteral(1.0))))) :: Nil
    )
    
    assertReflexiveParse(shorthand + shorthand + "#true",
      ast.ProperList(List(ast.Symbol(symbolName),
        ast.ProperList(List(ast.Symbol(symbolName),
          ast.BooleanLiteral(true)
        ))
      )) :: Nil
    )
  }
  
  test("empty list") {
    assertReflexiveParse("()", List(ast.EmptyList()))
  }

  test("booleans") {
    assertReflexiveParse("#t", List(ast.BooleanLiteral(true)))
    assertReflexiveParse("#true", List(ast.BooleanLiteral(true)))
    
    assertReflexiveParse("#f", List(ast.BooleanLiteral(false)))
    assertReflexiveParse("#false", List(ast.BooleanLiteral(false)))
  }

  test("symbols") {
    assertParsesAsSymbol("HELLO")
    assertParsesAsSymbol("HELLO123")
    assertParsesAsSymbol("predicate?")
    assertParsesAsSymbol("!$%&*+-./:<=>?@^_")
    assertParsesAsSymbol("from->to")
    assertParsesAsSymbol("...")
  }
  
  test("symbols are case sensitive") {
    assert(scm"HELLO" != scm"hello")
  }

  test("quoted symbols") {
    assertParsesAsSymbol("|Hello, world!|", "Hello, world!")
    assertParsesAsSymbol("""|\"|""", "\"")
    assertParsesAsSymbol("""|\||""", "|")
    assertParsesAsSymbol("""|two\x20;words|""", "two words")
    assertParsesAsSymbol("||", "")
    assertParsesAsSymbol("""|\t\t|""", "\t\t")
  }

  test("integers") {
    assertReflexiveParse("0", List(ast.IntegerLiteral(0)))
    assertReflexiveParse("000", List(ast.IntegerLiteral(0)))
    assertReflexiveParse("10000", List(ast.IntegerLiteral(10000)))
    assertReflexiveParse("-10000", List(ast.IntegerLiteral(-10000)))
    
    assertReflexiveParse("3.", List(ast.IntegerLiteral(3)))
    
    assertReflexiveParse("#b111", List(ast.IntegerLiteral(7)))
    assertReflexiveParse("#B-1000", List(ast.IntegerLiteral(-8)))

    assertReflexiveParse("#O1234", List(ast.IntegerLiteral(668)))
    assertReflexiveParse("#o-010", List(ast.IntegerLiteral(-8)))
    
    assertReflexiveParse("#Xdead", List(ast.IntegerLiteral(57005)))
    assertReflexiveParse("#x-b00b5", List(ast.IntegerLiteral(-721077)))
    
    // This is too large to be parsed as a 32bit integer or a double
    assertReflexiveParse("9007199254740993", List(ast.IntegerLiteral(9007199254740993L)))
  }

  test("reals") {
    assertReflexiveParse("0.0", List(ast.RationalLiteral(0.0)))
    assertReflexiveParse("33.337", List(ast.RationalLiteral(33.337)))
    assertReflexiveParse("-0100.0", List(ast.RationalLiteral(-100.0)))

    assertReflexiveParse("+inf.0", List(ast.PositiveInfinityLiteral()))
    assertReflexiveParse("-inf.0", List(ast.NegativeInfinityLiteral()))
    
    assertReflexiveParse("+INF.0", List(ast.PositiveInfinityLiteral()))
    assertReflexiveParse("-INF.0", List(ast.NegativeInfinityLiteral()))

    assertReflexiveParse("+NaN.0", List(ast.NaNLiteral()))
    assertReflexiveParse("-NaN.0", List(ast.NaNLiteral()))
  }

  test("strings") {
     def assertStringParsesAs(schemeString : String, result : String) = {
       assertReflexiveParse("\"" + schemeString + "\"", List(ast.StringLiteral(result)))
     }

    assertStringParsesAs("", "")
    assertStringParsesAs("""Hello, world!""", "Hello, world!")
    assertStringParsesAs("""Hello\"World""", "Hello\"World")
    assertStringParsesAs("""Hello\\World""", "Hello\\World")
    assertStringParsesAs("""Hello\|World""", "Hello|World")
    assertStringParsesAs("""Tab\t""", "Tab\t")
    assertStringParsesAs("""\nnewline""", "\nnewline")
    assertStringParsesAs("""carriage: \r""", "carriage: \r")
    assertStringParsesAs("""Space\x20;Bar""", "Space Bar")
    assertStringParsesAs("""l\x03BB;""", "l\u03bb")
    assertStringParsesAs("""The word \"recursion\" has many meanings.""", """The word "recursion" has many meanings.""")

    assertStringParsesAs("""Bare
newline""", "Bare\nnewline")

    assertStringParsesAs("""Here's text \
                            containing just one line""", """Here's text containing just one line""")

    intercept[ParseErrorException] {
      scm"""open "string"""
    }
  }

  test("proper lists") {
    assert(scm"(#true integer? |Hello| -1 2.0)" === List(
      ast.ProperList(List(
        ast.BooleanLiteral(true), 
        ast.Symbol("integer?"), 
        ast.Symbol("Hello"),
        ast.IntegerLiteral(-1),
        ast.RationalLiteral(2.0)
      ))
    ))
    
    intercept[ParseErrorException] {
      scm"""open (list"""
    }
  }

  test("improper lists") {
    assert(scm"(#false ONE 2.0 . +inf.0)" == List(
      ast.Pair(ast.BooleanLiteral(false),
        ast.Pair(ast.Symbol("ONE"),
          ast.Pair(ast.RationalLiteral(2.0), ast.PositiveInfinityLiteral()
    )))))
  }
  
  test("no expressions") {
    assert(scm"" === Nil)
  }

  test("multiple expression") {
    assert(scm"(define x 4) (add x 1)" === List(
      ast.ProperList(List(ast.Symbol("define"), ast.Symbol("x"), ast.IntegerLiteral(4))),
      ast.ProperList(List(ast.Symbol("add"), ast.Symbol("x"), ast.IntegerLiteral(1)))
    ))
  }

  test("quoted datums") {
    testSymbolShorthand("'", "quote")
  }
  
  test("quasiquoted datums") {
    testSymbolShorthand("`", "quasiquote")
  }
  
  test("unquoted datums") {
    testSymbolShorthand(",", "unquote")
  }
  
  test("splicing unquoted datums") {
    testSymbolShorthand(",@", "unquote-splicing")
  }

  test("vectors") {
    assertReflexiveParse("#(0 (2 2 2 2) Anna)", List(
      ast.VectorLiteral(Vector(
        ast.IntegerLiteral(0), 
        ast.ProperList(List(
          ast.IntegerLiteral(2),
          ast.IntegerLiteral(2),
          ast.IntegerLiteral(2),
          ast.IntegerLiteral(2)
        )),
        ast.Symbol("Anna")
      ))
    ))
    
    assertReflexiveParse("#()", List(ast.VectorLiteral(Vector())))
  }

  test("bytevectors") {
    assertReflexiveParse("#u8(+0 10. 5 #xff #d0)", List(
      ast.Bytevector(Vector(0, 10, 5, 255, 0))
    ))

    assertReflexiveParse("#u8()", List(ast.Bytevector(Vector())))
  }

  test("characters") { 
    assertReflexiveParse(raw"#\alarm", List(ast.CharLiteral(0x07)))
    assertReflexiveParse(raw"#\backspace", List(ast.CharLiteral(0x08)))
    assertReflexiveParse(raw"#\delete", List(ast.CharLiteral(0x7f)))
    assertReflexiveParse(raw"#\escape", List(ast.CharLiteral(0x1b)))
    assertReflexiveParse(raw"#\newline", List(ast.CharLiteral(0x0a)))
    assertReflexiveParse(raw"#\null", List(ast.CharLiteral(0x00)))
    assertReflexiveParse(raw"#\return", List(ast.CharLiteral(0x0d)))
    assertReflexiveParse(raw"#\space", List(ast.CharLiteral(0x20)))
    assertReflexiveParse(raw"#\tab", List(ast.CharLiteral(0x09)))

    assertReflexiveParse(raw"#\a", List(ast.CharLiteral('a')))
    assertReflexiveParse(raw"#\A", List(ast.CharLiteral('A')))
    assertReflexiveParse(raw"#\(", List(ast.CharLiteral('(')))
    assertReflexiveParse(raw"#\ ", List(ast.CharLiteral(' ')))
    assertReflexiveParse(raw"#\x03BB", List(ast.CharLiteral(0x3bb)))

    // Symbolic names are case sensitive by default
    // Additionally, an alphabetic character literal cannot be followed by an
    // identifier character without whitespace
    intercept[ParseErrorException] {
      SchemeParser.parseStringAsData(raw"#\SPACE")
    }

    // However, non-alphabetic character literls can be immediately followed
    // by another token. This is probably a bad idea to use in practice.
    assertReflexiveParse(raw"#\1moretime", List(ast.CharLiteral('1'), ast.Symbol("moretime")))
  }

  test("unit") {
    assertReflexiveParse("#!unit", List(ast.UnitValue()))
  }

  test("comments") {
    assertReflexiveParse("test ; COMMENT", List(ast.Symbol("test")))
    assertReflexiveParse("(Hello #;(you jerk))", List(ast.ProperList(ast.Symbol("Hello") :: Nil)))

    val multilineTest = """
      #| This is a block comment
         This can be as many lines as it wants
         It can also contain # and | |#
      (display "LOL")
      #| Make sure we treat this as a separate comment |#
      """;

    val data = SchemeParser.parseStringAsData(multilineTest)
    assert(data === List(ast.ProperList(List(ast.Symbol("display"), ast.StringLiteral("LOL")))))
  }
}
