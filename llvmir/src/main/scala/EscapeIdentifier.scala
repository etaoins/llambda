package io.llambda.llvmir

object EscapeIdentifier {
  private val llvmIdentifierPattern = 
    """[a-zA-Z_][a-zA-Z0-9_]*""".r.pattern

  /** Converts a free-form string to a valid LLVM IR identifier */
  def apply(string : String) : String = 
    if (llvmIdentifierPattern.matcher(string).matches) {
      string
    }
    else {
      '"' +
        string.replaceAllLiterally("\\", "\\" + "\\")
              .replaceAllLiterally("\"", "\\" + "\"") +
      '"'
    }
}

