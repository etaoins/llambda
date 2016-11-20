package io.llambda.compiler
import io.llambda

import org.scalatest.FunSuite

class AbstractSyntaxTreeSuite  extends FunSuite {
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
