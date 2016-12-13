package io.llambda.compiler
import io.llambda

/** Contains the result of running a Llambda program
  *
  * @param  stdout     Complete standard output of the program
  * @param  stderr     Complete standard error output of the program
  * @param  exitValue  Exit value of the process. 0 indicates success.
  * @param  runMethod  Method used to run the program
  */
case class RunResult(stdout: String, stderr: String, exitValue: Int, runMethod: RunResult.RunMethod)

object RunResult {
  sealed abstract class RunMethod

  /** Indicates that the program was interpreted */
  case object Interpreted extends RunMethod

  /** Indicates that the program was compiled to a native executable and run */
  case object ExternalProgram extends RunMethod
}
