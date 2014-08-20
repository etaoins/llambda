package io.llambda.compiler
import io.llambda

/** Bindings for the primitive expressions and type constructors */
object Primitives {
  object Lambda extends PrimitiveExpr
  object Quote extends PrimitiveExpr
  object If extends PrimitiveExpr
  object Set extends PrimitiveExpr
  object SyntaxError extends PrimitiveExpr
  object Include extends PrimitiveExpr
  object Quasiquote extends PrimitiveExpr
  object Unquote extends PrimitiveExpr
  object UnquoteSplicing extends PrimitiveExpr
  object Define extends PrimitiveExpr
  object DefineSyntax extends PrimitiveExpr
  object DefineRecordType extends PrimitiveExpr
  object DefineType extends PrimitiveExpr
  object NativeFunction extends PrimitiveExpr
  object WorldFunction extends PrimitiveExpr
  object DefineReportProcedure extends PrimitiveExpr
  object Cast extends PrimitiveExpr
  object AnnotateExprType extends PrimitiveExpr
  object AnnotateStorageLocType extends PrimitiveExpr
  object CondExpand extends PrimitiveExpr
  object Parameterize extends PrimitiveExpr
  object TypedDefine extends PrimitiveExpr
  object TypedDefineRecordType extends PrimitiveExpr
  object TypedLambda extends PrimitiveExpr
  object MakePredicate extends PrimitiveExpr

  object UnionType extends PrimitiveTypeConstructor
  object PairofType extends PrimitiveTypeConstructor
  object ListofType extends PrimitiveTypeConstructor
  object RecType extends PrimitiveTypeConstructor

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
      "world-function" -> WorldFunction,
      "define-report-procedure" -> DefineReportProcedure,
      "cast" -> Cast,
      "ann" -> AnnotateExprType,
      ":" -> AnnotateStorageLocType,
      "cond-expand" -> CondExpand,
      "parameterize" -> Parameterize,
      "define:" -> TypedDefine,
      "define-record-type:" -> TypedDefineRecordType,
      "lambda:" -> TypedLambda,
      "make-predicate" -> MakePredicate,

      "U" -> UnionType,
      "Pairof" -> PairofType,
      "Listof" -> ListofType,
      "Rec" -> RecType
    )
  }
}

