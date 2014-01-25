package io.llambda.compiler
import io.llambda

/** Bindings for the primitive expressions */
object PrimitiveExpressions {
  object Lambda extends PrimitiveExpression
  object Quote extends PrimitiveExpression
  object If extends PrimitiveExpression
  object Set extends PrimitiveExpression
  object SyntaxError extends PrimitiveExpression
  object Include extends PrimitiveExpression
  object Quasiquote extends PrimitiveExpression
  object Unquote extends PrimitiveExpression
  object UnquoteSplicing extends PrimitiveExpression
  object Define extends PrimitiveExpression
  object DefineSyntax extends PrimitiveExpression
  object DefineRecordType extends PrimitiveExpression
  object DefineType extends PrimitiveExpression
  object NativeFunction extends PrimitiveExpression
  object DefineReportProcedure extends PrimitiveExpression
  object AnnotateType extends PrimitiveExpression
  object CondExpand extends PrimitiveExpression
  object Parameterize extends PrimitiveExpression

  val bindings = {
    Map[String, BoundValue](
      "lambda" -> Lambda,
      "quote" -> Quote,
      "if" -> If,
      "set!" -> Set,
      "syntax-error" -> SyntaxError,
      "include" -> Include,
      "quasiquote" -> Quasiquote,
      "unquote" -> Unquote,
      "unquote-splicing" -> UnquoteSplicing,
      "define" -> Define,
      "define-syntax" -> DefineSyntax,
      "define-record-type" -> DefineRecordType,
      "define-type" -> DefineType,
      "native-function" -> NativeFunction,
      "define-report-procedure" -> DefineReportProcedure,
      "ann" -> AnnotateType,
      "cond-expand" -> CondExpand,
      "parameterize" -> Parameterize
    )
  }
}

