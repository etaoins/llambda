package io.llambda.compiler
import io.llambda

import org.scalatest.{FunSuite, Inside}
import llambda.compiler.SchemeStringImplicits._

class SchemeParserSuite extends FunSuite with Inside {
  // This checks:
  // 1) parsed source === expected
  // 2) parse((parsed source).toString) === expected
  def assertReflexiveParse(source: String, expected: List[ast.Datum]) {
    val parsed = SchemeParser.parseStringAsData(source)
    assert(parsed === expected)

    // Convert the parsed data back to a string to make sure .toString works
    val parsedAsSource = parsed.map(_.toString).mkString("\n")

    val reparsed = SchemeParser.parseStringAsData(parsedAsSource)
    assert(reparsed === expected)
  }

  def assertParsesAsSymbol(string: String, identifier: String) {
    assertReflexiveParse(string, List(ast.Symbol(identifier)))
  }

  def assertParsesAsSymbol(string: String) {
    assertParsesAsSymbol(string, string)
  }

  def testSymbolShorthand(shorthand: String, symbolName: String) {
    assertReflexiveParse(shorthand + "foo",
      ast.ProperList(List(ast.Symbol(symbolName), ast.Symbol("foo"))) :: Nil
    )

    // Make sure whitespace is allowed
    assertReflexiveParse(shorthand + " (1 . 2)",
      ast.ProperList(List(ast.Symbol(symbolName), ast.Pair(ast.Integer(1), ast.Integer(2)))) :: Nil
    )

    assertReflexiveParse(shorthand + "(rational? 1.0)",
      ast.ProperList(List(ast.Symbol(symbolName), ast.ProperList(List(ast.Symbol("rational?"), ast.Flonum(1.0))))) :: Nil
    )

    assertReflexiveParse(shorthand + shorthand + "#true",
      ast.ProperList(List(ast.Symbol(symbolName),
        ast.ProperList(List(ast.Symbol(symbolName),
          ast.Boolean(true)
        ))
      )) :: Nil
    )
  }

  test("empty list") {
    assertReflexiveParse("()", List(ast.EmptyList()))
  }

  test("booleans") {
    assertReflexiveParse("#t", List(ast.Boolean(true)))
    assertReflexiveParse("#true", List(ast.Boolean(true)))

    assertReflexiveParse("#f", List(ast.Boolean(false)))
    assertReflexiveParse("#false", List(ast.Boolean(false)))
  }

  test("symbols") {
    assertParsesAsSymbol("HELLO")
    assertParsesAsSymbol("HELLO123")
    assertParsesAsSymbol("predicate?")
    assertParsesAsSymbol("!$%&*+-./:<=>?@^_")
    assertParsesAsSymbol("from->to")
    assertParsesAsSymbol("...")

    intercept[ParseErrorException] {
      scm"""."""
    }
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
    assertParsesAsSymbol("|0|", "0")
    assertParsesAsSymbol("""|\t\t|""", "\t\t")

    intercept[ParseErrorException] {
      scm"""|foo"""
    }
  }

  test("integers") {
    assertReflexiveParse("0", List(ast.Integer(0)))
    assertReflexiveParse("000", List(ast.Integer(0)))
    assertReflexiveParse("10000", List(ast.Integer(10000)))
    assertReflexiveParse("-10000", List(ast.Integer(-10000)))

    assertReflexiveParse("3.", List(ast.Integer(3)))

    assertReflexiveParse("#b111", List(ast.Integer(7)))
    assertReflexiveParse("#b-1000", List(ast.Integer(-8)))

    assertReflexiveParse("#o1234", List(ast.Integer(668)))
    assertReflexiveParse("#o-010", List(ast.Integer(-8)))

    assertReflexiveParse("#d1234", List(ast.Integer(1234)))
    assertReflexiveParse("#d-010", List(ast.Integer(-10)))

    assertReflexiveParse("#xdead", List(ast.Integer(57005)))
    assertReflexiveParse("#x-b00b5", List(ast.Integer(-721077)))

    // This is too large to be parsed as a 32bit integer or a double
    assertReflexiveParse("9007199254740993", List(ast.Integer(9007199254740993L)))
  }

  test("reals") {
    assertReflexiveParse("0.0", List(ast.Flonum(0.0)))
    assertReflexiveParse("1.0", List(ast.Flonum(1.0)))
    assertReflexiveParse("33.337", List(ast.Flonum(33.337)))
    assertReflexiveParse("-0100.0", List(ast.Flonum(-100.0)))
    assertReflexiveParse(".25", List(ast.Flonum(0.25)))
    assertReflexiveParse("-.125", List(ast.Flonum(-0.125)))
    assertReflexiveParse("9007199254740992.0", List(ast.Flonum(9007199254740992.0)))

    assertReflexiveParse("5e-1", List(ast.Flonum(0.5)))
    assertReflexiveParse("5e+1", List(ast.Flonum(50.0)))
    assertReflexiveParse("2.5e5", List(ast.Flonum(250000.0)))
    assertReflexiveParse(".5e0", List(ast.Flonum(0.5)))
    assertReflexiveParse("-5e6", List(ast.Flonum(-5000000.0)))

    assertReflexiveParse("10eat", List(ast.Integer(10), ast.Symbol("eat")))
    assertReflexiveParse("10+eat", List(ast.Integer(10), ast.Symbol("+eat")))

    assertReflexiveParse("+inf.0", List(ast.PositiveInfinity()))
    assertReflexiveParse("-inf.0", List(ast.NegativeInfinity()))

    assertReflexiveParse("+nan.0", List(ast.NaN()))
    assertReflexiveParse("-nan.0", List(ast.NaN()))

    // An identifier is any sequence (...)  provided that it does not have a prefix which is a valid number
    assertReflexiveParse("+inf.00", List(ast.PositiveInfinity(), ast.Integer(0)))
    assertReflexiveParse("-inf.00", List(ast.NegativeInfinity(), ast.Integer(0)))
    assertReflexiveParse("+nan.00", List(ast.NaN(), ast.Integer(0)))
    assertReflexiveParse("-nan.00", List(ast.NaN(), ast.Integer(0)))
  }

  test("strings") {
     def assertStringParsesAs(schemeString: String, result: String) = {
       assertReflexiveParse("\"" + schemeString + "\"", List(ast.String(result)))
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
    assertStringParsesAs("""\x0;null!""", "\u0000null!")
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
        ast.Boolean(true),
        ast.Symbol("integer?"),
        ast.Symbol("Hello"),
        ast.Integer(-1),
        ast.Flonum(2.0)
      ))
    ))

    intercept[ParseErrorException] {
      scm"""open (list"""
    }
  }

  test("improper lists") {
    assert(scm"(. #t)" === List(ast.Boolean(true)))

    assert(scm"(#false ONE 2.0 . +inf.0)" === List(
      ast.Pair(ast.Boolean(false),
        ast.Pair(ast.Symbol("ONE"),
          ast.Pair(ast.Flonum(2.0), ast.PositiveInfinity()
    )))))

    // Make sure symbol prefixed with . isn't considered an improper list terminator
    assert(scm"(one two .three)" === List(ast.ProperList(
      List(
        ast.Symbol("one"),
        ast.Symbol("two"),
        ast.Symbol(".three")
    ))))

    intercept[ParseErrorException] {
      scm"""(one . two three)"""
    }

    intercept[ParseErrorException] {
      scm"""(one two three .)"""
    }
  }

  test("square proper lists") {
    assert(scm"[#true integer? |Hello| -1 2.0]" === List(
      ast.ProperList(List(
        ast.Boolean(true),
        ast.Symbol("integer?"),
        ast.Symbol("Hello"),
        ast.Integer(-1),
        ast.Flonum(2.0)
      ))
    ))

    intercept[ParseErrorException] {
      scm"""open (list"""
    }
  }

  test("square improper lists") {
    assert(scm"[. #t]" === List(ast.Boolean(true)))

    assert(scm"[#false ONE 2.0 . +inf.0]" === List(
      ast.Pair(ast.Boolean(false),
        ast.Pair(ast.Symbol("ONE"),
          ast.Pair(ast.Flonum(2.0), ast.PositiveInfinity()
    )))))
  }

  test("no expressions") {
    assert(scm"" === Nil)
  }

  test("multiple expression") {
    assert(scm"(define x 4) (add x 1)" === List(
      ast.ProperList(List(ast.Symbol("define"), ast.Symbol("x"), ast.Integer(4))),
      ast.ProperList(List(ast.Symbol("add"), ast.Symbol("x"), ast.Integer(1)))
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
      ast.Vector(Vector(
        ast.Integer(0),
        ast.ProperList(List(
          ast.Integer(2),
          ast.Integer(2),
          ast.Integer(2),
          ast.Integer(2)
        )),
        ast.Symbol("Anna")
      ))
    ))

    assertReflexiveParse("#()", List(ast.Vector(Vector())))

    intercept[ParseErrorException] {
      SchemeParser.parseStringAsData("#(bar")
    }
  }

  test("bytevectors") {
    assertReflexiveParse("#u8(+0 10. 5 #xff #d0)", List(
      ast.Bytevector(Vector(0, 10, 5, 255, 0).map(_.toByte))
    ))

    assertReflexiveParse("#u8()", List(ast.Bytevector(Vector())))

    intercept[ParseErrorException] {
      SchemeParser.parseStringAsData("#u8(1 2")
    }
  }

  test("characters") {
    assertReflexiveParse(raw"#\alarm", List(ast.Char(0x07)))
    assertReflexiveParse(raw"#\backspace", List(ast.Char(0x08)))
    assertReflexiveParse(raw"#\delete", List(ast.Char(0x7f)))
    assertReflexiveParse(raw"#\escape", List(ast.Char(0x1b)))
    assertReflexiveParse(raw"#\newline", List(ast.Char(0x0a)))
    assertReflexiveParse(raw"#\null", List(ast.Char(0x00)))
    assertReflexiveParse(raw"#\return", List(ast.Char(0x0d)))
    assertReflexiveParse(raw"#\space", List(ast.Char(0x20)))
    assertReflexiveParse(raw"#\tab", List(ast.Char(0x09)))

    assertReflexiveParse(raw"#\a", List(ast.Char('a')))
    assertReflexiveParse(raw"#\A", List(ast.Char('A')))
    assertReflexiveParse(raw"#\(", List(ast.Char('(')))
    assertReflexiveParse(raw"#\ ", List(ast.Char(' ')))
    assertReflexiveParse(raw"#\x03BB", List(ast.Char(0x3bb)))
    assertReflexiveParse(raw"#\☃", List(ast.Char(0x2603)))

    // Symbolic names are case sensitive by default
    // Additionally, an alphabetic character literal cannot be followed by an
    // identifier character without whitespace
    intercept[ParseErrorException] {
      SchemeParser.parseStringAsData(raw"#\SPACE")
    }

    // However, non-alphabetic character literals can be immediately followed
    // by another token. This is probably a bad idea to use in practice.
    assertReflexiveParse(raw"#\1moretime", List(ast.Char('1'), ast.Symbol("moretime")))

    // This is not a valid Unicode code point
    intercept[ParseErrorException] {
      SchemeParser.parseStringAsData(raw"#\x110000")
    }
  }

  test("unit") {
    assertReflexiveParse("#!unit", List(ast.Unit()))
  }

  test("comments") {
    assertReflexiveParse("test ; COMMENT", List(ast.Symbol("test")))
    assertReflexiveParse("(Hello #;(you jerk))", List(ast.ProperList(List(
      ast.Symbol("Hello")
    ))))

    assertReflexiveParse("(Hello #;  you jerk)", List(ast.ProperList(List(
      ast.Symbol("Hello"),
      ast.Symbol("jerk")
    ))))

    val multilineTest = """
      #| This is a block comment
         This can be as many lines as it wants
         It can also contain # and |
         It can even contain a #| nested comment |# |#
      (display "LOL")
      #| Make sure we treat this as a separate comment |#
      """;

    val data = SchemeParser.parseStringAsData(multilineTest)
    assert(data === List(ast.ProperList(List(ast.Symbol("display"), ast.String("LOL")))))
  }

  test("datum labels") {
    assertReflexiveParse("#123=(a b c) (d e #123#)", List(
      ast.ProperList(List("a", "b", "c").map(ast.Symbol(_))),
      ast.ProperList(List(
        ast.Symbol("d"),
        ast.Symbol("e"),
        ast.ProperList(List("a", "b", "c").map(ast.Symbol(_)))
      ))
    ))

    intercept[ParseErrorException] {
      // Reference to an undefined label
      SchemeParser.parseStringAsData("#123=(a b c) (d e #456#)")
    }
  }
}
