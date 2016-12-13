package io.llambda.compiler.functional
import io.llambda

import java.io.File
import java.io.InputStream
import scala.io.Source
import org.scalatest.{FunSuite, Inside}

import llambda.compiler._
import llambda.compiler.SchemeStringImplicits._

abstract class SchemeFunctionalTestRunner(
    testName: String,
    onlyOptimised: Boolean = false
) extends FunSuite with Inside {
  // Implicit import decl every test gets
  private val testImportDecl = datum"(import (llambda nfi) (scheme base) (llambda test-util))"

  private val AbnormalExitCodes = List(
    // SIGILL
    128 + 4,
    // SIGABRT
    128 + 6,
    // SIGBUS
    128 + 10,
    // SIGSEGV
    128 + 11
  )

  private val targetPlatform = platform.DetectTargetPlatform()

  private case class ExecutionResult(
      success: Boolean,
      output: List[ast.Datum],
      errorString: String,
      exitValue: Int,
      runMethod: RunResult.RunMethod
  )

  val resourceBaseDir = "functional/"
  val resourceBaseUrl = getClass.getClassLoader.getResource(resourceBaseDir)
  val resourcePath = s"${resourceBaseDir}${testName}.scm"

  val includePath = frontend.IncludePath(List(resourceBaseUrl))

  val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)

  if (stream == null) {
    throw new Exception(s"Unable to load Scheme test source from ${resourcePath}")
  }

  // Load the tests
  val allTestSource = Source.fromInputStream(stream, "UTF-8").mkString

  val parsed = SchemeParser.parseStringAsData(allTestSource, Some(s":/${resourcePath}"))
  runAllTests(parsed)

  private def runAllTests(allTests: List[ast.Datum]) {
    if (!onlyOptimised) {
      runTestConfiguration(allTests, 0)
    }

    runTestConfiguration(allTests, 2)
  }

  /** Expands top-level (cond-expand) expressions in the test source */
  private def expandTopLevel(data: List[ast.Datum])(implicit libraryLoader: frontend.LibraryLoader, frontendConfig: frontend.FrontendConfig): List[ast.Datum] = {
    data flatMap {
      case ast.ProperList(ast.Symbol("cond-expand") :: firstClause :: restClauses) =>
        frontend.CondExpander.expandData(firstClause :: restClauses)

      case other =>
        List(other)
    }
  }

  private def runTestConfiguration(allTests: List[ast.Datum], optimiseLevel: Int) {
    // Deal with (cond-expand) for this configuration
    val expandLibraryLoader = new frontend.LibraryLoader(targetPlatform)
    val expandFrontendConfig = frontend.FrontendConfig(
      includePath=includePath,
      featureIdentifiers=targetPlatform.platformFeatures
    )

    val expandedTests = expandTopLevel(allTests)(expandLibraryLoader, expandFrontendConfig)

    for(singleTest <- expandedTests) {
      singleTest match {
        case ast.ProperList(ast.Symbol("define-test") :: ast.StringLiteral(name) :: condition :: Nil) =>
          // Start a nested test
          test(s"$name (-O ${optimiseLevel})") {
            runSingleCondition(condition, optimiseLevel)
          }

        case other =>
          fail("Unable to parse test: " + singleTest.toString)
      }
    }
  }

  private def runSingleCondition(condition: ast.Datum, optimiseLevel: Int) {
    condition match {
      case ast.ProperList(ast.Symbol("expect") :: expectedValue :: program) if !program.isEmpty =>
        val result = executeProgram(wrapForPrinting(program), optimiseLevel)

        if (!result.success) {
          if (result.errorString.isEmpty) {
            fail("Execution unexpectedly failed with no output")
          }
          else {
            // Use the error string the program provided
            fail(result.errorString)
          }
        }

        assert(result.output === List(expectedValue))

      case ast.ProperList(ast.Symbol("expect-output") :: ast.ProperList(expectedOutput) :: program) if !program.isEmpty =>
        val result = executeProgram(program, optimiseLevel)

        if (!result.success) {
          if (result.errorString.isEmpty) {
            fail("Execution unexpectedly failed with no output")
          }
          else {
            // Use the error string the program provided
            fail(result.errorString)
          }
        }

        assert(result.output === expectedOutput)

      case ast.ProperList(ast.Symbol(testType @ ("expect-success" | "expect-static-success")) :: program) if !program.isEmpty =>
        // Make sure the program outputs this at the end
        val canaryValue = ast.Symbol("test-completed")
        val programWithCanary = program :+ ast.ProperList(List(ast.Symbol("quote"), canaryValue))

        val result = executeProgram(wrapForPrinting(programWithCanary), optimiseLevel)

        if (!result.success) {
          if (result.errorString.isEmpty) {
            fail("Execution unexpectedly failed with no output")
          }
          else {
            // Use the error string the program provided
            fail(result.errorString)
          }
        }

        // Did we expect the optimiser to evaluate this?
        if ((testType == "expect-static-success") &&
            (optimiseLevel == 2) &&
            (result.runMethod != RunResult.Interpreted)) {
          fail("Test could not be statically evaluated at -O 2")
        }

        assert(result.output === List(canaryValue), "Execution did not reach end of test")

      case ast.ProperList(ast.Symbol("expect-exit-value") :: ast.IntegerLiteral(exitValue) :: program) if !program.isEmpty =>
        val result = executeProgram(program, optimiseLevel)
        assert(result.exitValue == exitValue)

      case ast.ProperList(ast.Symbol("expect-error") :: ast.Symbol(errorPredicate) :: program) if !program.isEmpty =>
        try {
          val wrappedProgram = wrapForAssertRaises(errorPredicate, program)
          val result = executeProgram(wrappedProgram, optimiseLevel)

          if (!result.success) {
            if (result.errorString.isEmpty) {
              fail("Execution unexpectedly failed with no output")
            }
            else {
              fail(result.errorString)
            }
          }
        }
        catch {
          case expectedError: SemanticException
            if expectedError.errorCategory == ErrorCategory.fromPredicate(errorPredicate) =>
        }

      case ast.ProperList(ast.Symbol("expect-compile-error") :: ast.Symbol(errorPredicate) :: program) if !program.isEmpty =>
        try {
          executeProgram(program, optimiseLevel)
        }
        catch {
          case _: SemanticException if errorPredicate == "error-object?" =>
            return

          case expectedError: SemanticException
              if expectedError.errorCategory == ErrorCategory.fromPredicate(errorPredicate) =>
            return
        }

        fail("Compilation unexpectedly succeeded")

      case other =>
        fail("Unable to parse condition: " + condition.toString)
    }
  }

  private def wrapForPrinting(program: List[ast.Datum]): List[ast.Datum] = {
    // Our special version of (write) that generates less code due to not using parameters
    val lastValueWriter = datum"""(native-function system-library "llcore_write_stdout" (-> <any> <unit>))"""

    // Modify the last expression to print using llcore_write_stdout
    val wrappedDatum = ast.ProperList(List(
      lastValueWriter,
      program.last
    ))

    program.dropRight(1) :+ wrappedDatum
  }

  private def wrapForAssertRaises(errorPredicate: String, program: List[ast.Datum]): List[ast.Datum] = {
    // Make sure we don't wrap any (import)s the test may have
    val (testImports, testExprs) = program.span {
      case ast.ProperList(ast.Symbol("import") :: _) => true
      case _ => false

    }

    (datum"(import (llambda error))" :: testImports) :+
      ast.ProperList(ast.Symbol("assert-raises") :: ast.Symbol(errorPredicate) :: testExprs)
  }

  private def utf8InputStreamToString(stream: InputStream): String =
    Source.fromInputStream(stream, "UTF-8").mkString

  private def executeProgram(program: List[ast.Datum], optimiseLevel: Int): ExecutionResult = {
    val finalProgram = testImportDecl :: program

    // Compile the program
    val compileConfig = CompileConfig(
      includePath=includePath,
      optimiseLevel=optimiseLevel,
      targetPlatform=targetPlatform
    )

    // Build our environment
    val testFilesBaseUrl = getClass.getClassLoader.getResource("test-files/")
    val testFilesBaseDir = new File(testFilesBaseUrl.toURI).getAbsolutePath

    val extraEnv = List[(String, String)](
      "LLAMBDA_TEST" -> "1",
      "LLAMBDA_TEST_FILES_BASE" -> testFilesBaseDir
    )

    val result = Compiler.runData(finalProgram, compileConfig, extraEnv)

    val errorString = result.stderr
    val exitValue = result.exitValue

    if (AbnormalExitCodes.contains(exitValue)) {
      fail("Execution abnormally terminated with signal " + (exitValue - 128))
    }
    else if (exitValue == 0) {
      val output = SchemeParser.parseStringAsData(result.stdout)

      ExecutionResult(success=true, output=output, errorString=errorString, exitValue, result.runMethod)
    }
    else {
      ExecutionResult(success=false, output=Nil, errorString=errorString, exitValue, result.runMethod)
    }
  }
}
