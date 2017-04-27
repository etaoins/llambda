package io.llambda.compiler


case class Library(name: List[String], exports: Map[String, BoundValue], exprs: List[et.Expr])
