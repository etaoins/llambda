package llambda

import scala.tools.jline.console.ConsoleReader

object Repl {
  def apply() {
    val reader = new ConsoleReader;

    while(true) {
      val command = reader.readLine("llambda> ")

      if (command == ":quit") {
        return;
      }

      SchemeParser(command) match {
        case SchemeParser.Success(expressions, _) => 
          for(expression <- expressions) {
            println("res: " + expression)
          }

        case err =>
          println("error: " + err)
      }
    }
  }
}
