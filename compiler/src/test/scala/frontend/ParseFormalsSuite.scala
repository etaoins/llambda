package io.llambda.compiler.frontend
import io.llambda

import org.scalatest.{FunSuite, Inside}

import llambda.compiler._
import llambda.compiler.{valuetype => vt}

class ParseFormalsSuite extends FunSuite with Inside with testutil.ExprHelpers {
  // sst.AnyList won't accept single data or empty lists as lists
  private def destructureList(datum: sst.ScopedDatum): (List[sst.ScopedDatum], sst.ScopedDatum) = datum match {
    case sst.Pair(car, tail) =>
      destructureList(tail) match {
        case (tailMembers, terminator) =>
          (car :: tailMembers, terminator)
      }

    case terminator =>
      (Nil, terminator)
  }


  private def formalsFor(scheme: String, allowOptionals: Boolean = false): ParsedFormals = {
    val data = SchemeParser.parseStringAsData(scheme)

    destructureList(sst.ScopedDatum(typedLambdaScope, data.head)) match {
      case (argList, argTerminator) => ParseFormals(argList, argTerminator, allowOptionals)
    }
  }

  test("empty formals") {
    val actual = formalsFor("()")
    val expected = ParsedFormals(Nil, Nil, None)

    assert(actual === expected)
  }

  test("untyped fixed args") {
    inside(formalsFor("(fixed1 fixed2)")) {
      case ParsedFormals(
        List(
          (sst.Symbol(_, "fixed1"), None),
          (sst.Symbol(_, "fixed2"), None)
        ),
        Nil,
        None
      ) =>
    }
  }

  test("untyped rest arg only") {
    inside(formalsFor("rest")) {
      case ParsedFormals(
        Nil,
        Nil,
        Some((sst.Symbol(_, "rest"), None))
      ) =>
    }
  }

  test("typed fixed args") {
    inside(formalsFor("([fixed1 : <integer>] fixed2)")) {
      case ParsedFormals(
        List(
          (sst.Symbol(_, "fixed1"), Some(vt.IntegerType)),
          (sst.Symbol(_, "fixed2"), None)
        ),
        Nil,
        None
      ) =>
    }
  }

  test("typed fixed and rest args") {
    inside(formalsFor("(fixed1 fixed2 rest : <flonum> *)")) {
      case ParsedFormals(
        List(
          (sst.Symbol(_, "fixed1"), None),
          (sst.Symbol(_, "fixed2"), None)
        ),
        Nil,
        Some((sst.Symbol(_, "rest"), Some(vt.FlonumType)))
      ) =>
    }
  }

  test("untyped fixed arg with default") {
    inside(formalsFor("([fixed1 1] [fixed2 2])", allowOptionals=true)) {
      case ParsedFormals(
        Nil,
        List(
          ParsedOptional(
            sst.Symbol(_, "fixed1"),
            None,
            sst.NonSymbolLeaf(ast.Integer(1))
          ),
          ParsedOptional(
            sst.Symbol(_, "fixed2"),
            None,
            sst.NonSymbolLeaf(ast.Integer(2))
          )
        ),
        None
      ) =>
    }
  }

  test("typed fixed arg with default") {
    inside(formalsFor("([fixed1 : <integer> 1] [fixed2 : <symbol> val])", allowOptionals=true)) {
      case ParsedFormals(
        Nil,
        List(
          ParsedOptional(
            sst.Symbol(_, "fixed1"),
            Some(vt.IntegerType),
            sst.NonSymbolLeaf(ast.Integer(1))
          ),
          ParsedOptional(
            sst.Symbol(_, "fixed2"),
            Some(vt.SymbolType),
            sst.Symbol(_, "val")
          )
        ),
        None
      ) =>
    }
  }

  test("argument without default following argument with default fails") {
    intercept[BadSpecialFormException] {
      formalsFor("([fixed1 : <integer> 1] [fixed2 : <symbol>])", allowOptionals=true)
    }
  }

  test("untyped fixed arg with default fails when optionals are not allowed") {
    intercept[BadSpecialFormException] {
      formalsFor("([fixed1 1] [fixed2 2])", allowOptionals=false)
    }
  }
}
