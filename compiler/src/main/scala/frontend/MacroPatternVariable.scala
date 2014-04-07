package io.llambda.compiler.frontend
import io.llambda

import llambda.compiler._

 case class MacroPatternVariable(ellipsisDepth : Int, variable : SyntaxVariable) extends SourceLocated
