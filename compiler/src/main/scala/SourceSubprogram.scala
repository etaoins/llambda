package io.llambda.compiler
import io.llambda

/** Represents a subprogram in the Scheme source
  *
  * Note that this may not correspond to a function in the generated code. For example, all top-level subprograms are
  * placed in a single generated function. Also, inlining may place expressions from some source procedures inside
  * another.
  *
  * This is used to generate debugging information.
  */
 sealed trait SourceSubprogram {
   val filenameOpt : Option[String]
   val startLine : Int
   val displayNameOpt : Option[String]
 }

/** Represents a procedure in the Scheme source */
 case class SourceProcedure(filenameOpt : Option[String], startLine : Int, sourceNameOpt : Option[String]) {
   val displayNameOpt = sourceNameOpt
 }
