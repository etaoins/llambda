package io.llambda.compiler
import io.llambda

case class Library(name : List[String], exports : Map[String, BoundValue], exprs : List[et.Expr])
