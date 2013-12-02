package llambda.codegen.llvmir

object EscapeIdentifier {
  /** Converts a free-form string to a valid LLVM IR identifier */
  def apply(string : String) : String = 
    if (string.matches("""[a-zA-Z_][a-zA-Z0-9_]*""")) {
      string
    }
    else {
      '"' +
        string.replaceAllLiterally("\\", "\\" + "\\")
              .replaceAllLiterally("\"", "\\" + "\"") +
      '"'
    }
}

