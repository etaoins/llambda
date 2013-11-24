package llambda.codegen.llvmir

object EscapeIdentifier {
  /** Converts a free-form string to a valid LLVM IR identifier 
    *
    * Technically this only needs to be done for identifiers with special
    * characters but doing this with all identifiers makes it easy to identify
    * user-supplied identifiers
    */
  def apply(string : String) : String = 
    '"' +
      string.replaceAllLiterally("\\", "\\" + "\\")
            .replaceAllLiterally("\"", "\\" + "\"") +
    '"'
}

