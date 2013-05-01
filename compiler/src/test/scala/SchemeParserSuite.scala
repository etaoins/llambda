package test.scala

import org.scalatest.{FunSuite, Inside}
import llambda._
import llambda.SchemeStringImplicits._

class SchemeParserSuite extends FunSuite with Inside {
  def assertParsesAsSymbol(string : String, identifier : String) {
    inside(SchemeParser(string)) { case SchemeParser.Success(data, _) =>
      assert(data === List(ast.Symbol(identifier)))
    }
  }
  
  def assertParsesAsSymbol(string : String) {
    assertParsesAsSymbol(string, string)
  }

  def testSymbolShorthand(shorthand : String, symbolName : String) {
    inside(SchemeParser(shorthand + "foo")) {
      case SchemeParser.Success(datum :: Nil, _) =>
        assert(datum === 
          ast.ProperList(List(ast.Symbol(symbolName), ast.Symbol("foo")))
        )
    }

    inside(SchemeParser(shorthand + "(1 . 2)")) { 
      case SchemeParser.Success(datum :: Nil, _) =>
        assert(datum === 
          ast.ProperList(List(ast.Symbol(symbolName), ast.Pair(ast.IntegerLiteral(1), ast.IntegerLiteral(2))))
        )
    }
    
    inside(SchemeParser(shorthand + "(real? 1.0)")) { 
      case SchemeParser.Success(datum :: Nil, _) =>
        assert(datum === 
          ast.ProperList(List(ast.Symbol(symbolName), ast.ProperList(List(ast.Symbol("real?"), ast.RealLiteral(1.0)))))
        )
    }
    
    inside(SchemeParser(shorthand + shorthand + "#true")) {
      case SchemeParser.Success(datum :: Nil, _) =>
        assert(datum ===
          ast.ProperList(List(ast.Symbol(symbolName),
            ast.ProperList(List(ast.Symbol(symbolName),
              ast.TrueLiteral
            ))
          ))
        )
    }
  }
  
  test("empty list") {
    assert(scm"()" === List(ast.EmptyList))
  }

  test("booleans") {
    assert(scm"#t" === List(ast.TrueLiteral))
    assert(scm"#true" === List(ast.TrueLiteral))
    
    assert(scm"#f" === List(ast.FalseLiteral))
    assert(scm"#false" === List(ast.FalseLiteral))
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
    assert(scm"0" === List(ast.IntegerLiteral(0)))
    assert(scm"000" === List(ast.IntegerLiteral(0)))
    assert(scm"10000" === List(ast.IntegerLiteral(10000)))
    assert(scm"-10000" === List(ast.IntegerLiteral(-10000)))
    
    assert(scm"#b111" === List(ast.IntegerLiteral(7)))
    assert(scm"#b-1000" === List(ast.IntegerLiteral(-8)))

    assert(scm"#o1234" === List(ast.IntegerLiteral(668)))
    assert(scm"#o-010" === List(ast.IntegerLiteral(-8)))
    
    assert(scm"#xdead" === List(ast.IntegerLiteral(57005)))
    assert(scm"#x-b00b5" === List(ast.IntegerLiteral(-721077)))
  }

  test("reals") {
    assert(scm"0.0" === List(ast.RealLiteral(0.0)))
    assert(scm"33.337" === List(ast.RealLiteral(33.337)))
    assert(scm"-0100.0" === List(ast.RealLiteral(-100.0)))

    assert(scm"+inf.0" === List(ast.PositiveInfinityLiteral))
    assert(scm"-inf.0" === List(ast.NegativeInfinityLiteral))
    
    assert(scm"+INF.0" === List(ast.PositiveInfinityLiteral))
    assert(scm"-INF.0" === List(ast.NegativeInfinityLiteral))

    assert(scm"+NaN.0" === List(ast.NaNLiteral))
    assert(scm"-NaN.0" === List(ast.NaNLiteral))
  }

  test("strings") {
     def assertStringParsesAs(schemeString : String, result : String) =
      inside(SchemeParser("\"" + schemeString + "\"")) { case SchemeParser.Success(data, _) =>
        assert(data === List(ast.StringLiteral(result)))
      }

    assertStringParsesAs("", "")
    assertStringParsesAs("""Hello, world!""", "Hello, world!")
    assertStringParsesAs("""Hello\"World""", "Hello\"World")
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
  }

  test("proper lists") {
    assert(scm"(#true integer? |Hello| -1 2.0)" === List(
      ast.ProperList(List(
        ast.TrueLiteral, 
        ast.Symbol("integer?"), 
        ast.Symbol("Hello"),
        ast.IntegerLiteral(-1),
        ast.RealLiteral(2.0)
      ))
    ))
  }

  test("improper lists") {
    assert(scm"(#false ONE 2.0 . +inf.0)" == List(
      ast.Pair(ast.FalseLiteral,
        ast.Pair(ast.Symbol("ONE"),
          ast.Pair(ast.RealLiteral(2.0), ast.PositiveInfinityLiteral
    )))))
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
    assert(scm"#(0 (2 2 2 2) Anna)" === List(
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
    
    assert(scm"#()" === List(ast.VectorLiteral(Vector())))
  }

  test("bytevectors") {
    assert(scm"#u8(0 10 5)" === List(
      ast.ByteVector(Vector(0, 10, 5))
    ))

    assert(scm"#u8()" === List(ast.ByteVector(Vector())))
  }

  test("characters") { 
    assert(scm"#\alarm" === List(ast.CharLiteral(0x07)))
    assert(scm"#\backspace" === List(ast.CharLiteral(0x08)))
    assert(scm"#\delete" === List(ast.CharLiteral(0x7f)))
    assert(scm"#\escape" === List(ast.CharLiteral(0x1b)))
    assert(scm"#\newline" === List(ast.CharLiteral(0x0a)))
    assert(scm"#\null" === List(ast.CharLiteral(0x00)))
    assert(scm"#\return" === List(ast.CharLiteral(0x0d)))
    assert(scm"#\space" === List(ast.CharLiteral(0x20)))
    assert(scm"#\tab" === List(ast.CharLiteral(0x09)))

    assert(scm"#\a" === List(ast.CharLiteral('a')))
    assert(scm"#\A" === List(ast.CharLiteral('A')))
    assert(scm"#\(" === List(ast.CharLiteral('(')))
    assert(scm"#\ " === List(ast.CharLiteral(' ')))
    assert(scm"#\x03BB" === List(ast.CharLiteral(0x3bb)))
  }

  test("comments") {
    assert(scm"test ; COMMENT" === List(ast.Symbol("test")))
    assert(scm"(Hello #;(you jerk))" === List(ast.ProperList(ast.Symbol("Hello") :: Nil)))

    val multilineTest = """
      #| This is a block comment
         This can be as many lines as it wants |#
      (display "LOL")
      """;

    inside(SchemeParser(multilineTest)) { case SchemeParser.Success(data, _) =>
      assert(data === List(ast.ProperList(List(ast.Symbol("display"), ast.StringLiteral("LOL")))))
    }
  }
}
