package io.llambda.compiler
import io.llambda

/** Contains the result of running a Llambda program
  *
  * @param  stdout     Complete standard output of the program
  * @param  stderr     Complete standard error output of the program
  * @param  exitValue  Exit value of the process. 0 indicates success.
  */
case class RunResult(stdout : String, stderr : String, exitValue : Int)
