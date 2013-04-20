package llambda

object SchemeStringImplicits {
  import StringContext._

  class StringParseError(message : String) extends Exception(message)

  implicit class SchemeStringHelper(val sc : StringContext) extends AnyVal {
    def scm(args : Any*) : List[ast.Datum] = SchemeParser(sc.raw(args : _*)) match {
      case SchemeParser.Success(data, _) => data
      case SchemeParser.Error(msg, _) => throw new StringParseError(msg)
      case SchemeParser.Failure(msg, _) => throw new StringParseError(msg)
    }
  }
}
