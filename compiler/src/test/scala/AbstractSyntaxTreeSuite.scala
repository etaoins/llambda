package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite

class AbstractSyntaxTreeSuite  extends FunSuite {
  test("fold case of symbol") {
    assert(ast.Symbol("UPPER").toCaseFolded === ast.Symbol("upper"))
    assert(ast.Symbol("Mixed").toCaseFolded === ast.Symbol("mixed"))
    assert(ast.Symbol("LOWER").toCaseFolded === ast.Symbol("lower"))
  }

  test("fold case preserves source location") {
    val testLoc = SourceLocation(Some("test-file"), "HELLO", 0)

    val testSymbol = ast.Symbol("UPPER")
    testSymbol.locationOpt = Some(testLoc)

    val foldedSymbol = testSymbol.toCaseFolded

    assert(foldedSymbol.locationOpt === Some(testLoc))
  }

  test("fold case of symbols inside pair") {
    val testPair = ast.Pair(
      ast.Symbol("CAR"),
      ast.Symbol("Cdr")
    )

    val expectedPair = ast.Pair(
      ast.Symbol("car"),
      ast.Symbol("cdr")
    )

    assert(testPair.toCaseFolded === expectedPair)
  }

  test("fold case of symbols inside vector") {
    val testVector = ast.VectorLiteral(Vector(
      ast.Symbol("ONE"),
      ast.Symbol("Two"),
      ast.Symbol("three")
    ))

    val expectedVector = ast.VectorLiteral(Vector(
      ast.Symbol("one"),
      ast.Symbol("two"),
      ast.Symbol("three")
    ))

    assert(testVector.toCaseFolded === expectedVector)
  }

  test("symbols to string") {
    assert(ast.Symbol("Hello").toString === "Hello");
    assert(ast.Symbol("HelloWorldThisRequiresHeapAllocation").toString === "HelloWorldThisRequiresHeapAllocation");
    assert(ast.Symbol("λ").toString === "|λ|");
    assert(ast.Symbol("Hello, world").toString === "|Hello, world|");
    assert(ast.Symbol("Back\\slash").toString === "|Back\\\\slash|");
    assert(ast.Symbol("P|pe").toString === "|P\\|pe|");
    assert(ast.Symbol("Quo\"te").toString === "|Quo\"te|");
    assert(ast.Symbol("").toString === "||");
    assert(ast.Symbol("Open[square").toString === "|Open[square|");
    assert(ast.Symbol("Close]square").toString === "|Close]square|");
    assert(ast.Symbol("Open(round").toString === "|Open(round|");
    assert(ast.Symbol("Close)round").toString === "|Close)round|");
    assert(ast.Symbol("Mid#hash").toString === "|Mid#hash|");
    assert(ast.Symbol("Mid'quote").toString === "|Mid'quote|");
    assert(ast.Symbol("Mid,comma").toString === "|Mid,comma|");
    assert(ast.Symbol("Mid`backtick").toString === "|Mid`backtick|");

    // This is allowed as @ is only special after a ,
    assert(ast.Symbol("Mid@at").toString === "Mid@at");

    // These are "peculiar identifies"
    assert(ast.Symbol("+").toString === "+");
    assert(ast.Symbol("-").toString === "-");

    // (explicit sign) (sign subseqqent) (subsequent)*
    assert(ast.Symbol("++").toString === "++");
    assert(ast.Symbol("+@").toString === "+@");
    assert(ast.Symbol("-+foo").toString === "-+foo");
    assert(ast.Symbol("+@foo").toString === "+@foo");

    // (explicit sign) . (dot subsequent) (subsequent)*
    assert(ast.Symbol("+.+").toString === "+.+");
    assert(ast.Symbol("-.@").toString === "-.@");
    assert(ast.Symbol("+..").toString === "+..");
    assert(ast.Symbol("-.+foo").toString === "-.+foo");
    assert(ast.Symbol("+.@foo").toString === "+.@foo");
    assert(ast.Symbol("-..foo").toString === "-..foo");

    // . (dot subsequent) (subsequent)*
    assert(ast.Symbol(".+").toString === ".+");
    assert(ast.Symbol(".@").toString === ".@");
    assert(ast.Symbol("..").toString === "..");
    assert(ast.Symbol(".+foo").toString === ".+foo");
    assert(ast.Symbol(".@foo").toString === ".@foo");
    assert(ast.Symbol("..foo").toString === "..foo");

    // These are also numbers
    assert(ast.Symbol("0").toString === "|0|");
    assert(ast.Symbol("+0").toString === "|+0|");
    assert(ast.Symbol("-0").toString === "|-0|");
    assert(ast.Symbol("0.5").toString === "|0.5|");
    assert(ast.Symbol("+0.5").toString === "|+0.5|");
    assert(ast.Symbol("-0.5").toString === "|-0.5|");
    assert(ast.Symbol("2/3").toString === "|2/3|");
    assert(ast.Symbol("+2/3").toString === "|+2/3|");
    assert(ast.Symbol("-2/3").toString === "|-2/3|");
    assert(ast.Symbol("+inf.0").toString === "|+inf.0|");
    assert(ast.Symbol("-inf.0").toString === "|-inf.0|");
    assert(ast.Symbol("+nan.0").toString === "|+nan.0|");
    assert(ast.Symbol("-nan.0").toString === "|-nan.0|");
    assert(ast.Symbol("+INF.0").toString === "|+INF.0|");
    assert(ast.Symbol("-INF.0").toString === "|-INF.0|");
    assert(ast.Symbol("+NaN.0").toString === "|+NaN.0|");
    assert(ast.Symbol("-NaN.0").toString === "|-NaN.0|");
    assert(ast.Symbol("+inf.00").toString === "|+inf.00|");
    assert(ast.Symbol("-inf.00").toString === "|-inf.00|");
    assert(ast.Symbol("+nan.00").toString === "|+nan.00|");
    assert(ast.Symbol("-nan.00").toString === "|-nan.00|");
  }
}
