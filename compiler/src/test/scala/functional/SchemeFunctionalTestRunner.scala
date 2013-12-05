package llambda.functional

import java.io.File
import scala.sys.process._
import llambda._
import llambda.{celltype => ct}
import org.scalatest.{FunSuite, Inside}
import java.io.{InputStream}
import SchemeStringImplicits._

abstract class SchemeFunctionalTestRunner(testName : String) extends FunSuite with Inside {
  private case class ExecutionResult(success : Boolean, output : ast.Datum, errorString : String)

  val resourceBaseDir = "functional/"
  val resourceBaseUrl = getClass.getClassLoader.getResource(resourceBaseDir)
  val resourcePath = s"${resourceBaseDir}${testName}.scm"

  val includePath = frontend.IncludePath(
    fileParentDir=Some(resourceBaseUrl),
    packageRootDir=Some(resourceBaseUrl)
  )
  
  val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)

  if (stream == null) {
    throw new Exception(s"Unable to load Scheme test source from ${resourcePath}")
  }

  // Load the tests
  val allTestSource = io.Source.fromInputStream(stream, "UTF-8").mkString

  val parsed = SchemeParser.parseStringAsData(allTestSource, Some(s":/${resourcePath}"))
  runAllTests(parsed)

  private def runAllTests(allTests : List[ast.Datum]) {
    for(singleTest <- allTests) {
      singleTest match {
        case ast.ProperList(ast.Symbol("define-test") :: ast.StringLiteral(name) :: condition :: Nil) =>
          // Start a nested test
          test(name) {
            runFloatCondition(condition)
          }

        case other =>
          fail("Unable to parse test: " + singleTest.toString)
      }
    }
  }

  private def runFloatCondition(condition : ast.Datum) {
    condition match {
      case ast.ProperList(ast.Symbol("expect") :: expectation :: program) if !program.isEmpty =>
        val result = executeProgram(program)

        if (!result.success) {
          if (result.errorString.isEmpty) {
            fail("Execution unexpectedly failed with no output")
          }
          else {
            // Use the error string the program provided
            fail(result.errorString)
          }
        }

        assert(result.output === expectation)
      
      case ast.ProperList(ast.Symbol("expect-failure") :: program) if !program.isEmpty =>
        try {
          val result = executeProgram(program)

          // If we compiled make sure we fail at runtime
          assert(result.success === false, "Execution unexpectedly succeeded")
        }
        catch {
          case e : SemanticException =>
            // Semantic exceptions are allowed
        }

      case other =>
          fail("Unable to parse condition: " + condition.toString)
    }
  }

  private def utf8InputStreamToString(stream : InputStream) : String =
    io.Source.fromInputStream(stream, "UTF-8").mkString

  private def executeProgram(program : List[ast.Datum]) : ExecutionResult = {
    // Import (llambda nfi) and (scheme base)
    val importDecl = datum"(import (llambda nfi) (scheme base))"

    // Modify the last expression to print using lliby_write
    val valueDatum = program.last

    val printValueDatum = ast.ProperList(List(
      ast.ProperList(List(
        ast.Symbol("native-function"),
        ast.StringLiteral("lliby_write"),
        ast.ProperList(List(ast.Symbol("<datum-cell>"))))),
      valueDatum))

    // Rebuild the program with the import and value printing
    val printingProgram = (importDecl :: program.dropRight(1)) :+ printValueDatum

    // Compile the program
    val outputFile = File.createTempFile("llambdafunc", null, null)
    outputFile.deleteOnExit()

    try {
      val compileConfig = CompileConfig(
        includePath=includePath,
        // Optimize to catch more miscompilations
        optimizeLevel=2,
        targetPlatform=platform.DetectJvmPlatform())

      Compiler.compileData(printingProgram, outputFile, compileConfig)

      // Create our output logger
      var stdout : Option[InputStream] = None
      var stderr : Option[InputStream] = None

      val outputIO = new ProcessIO(
        stdin  => Unit, // Don't care
        stdoutStream => stdout = Some(stdoutStream),
        stderrStream => stderr = Some(stderrStream)
      )

      // Call the program
      val testProcess = Process(outputFile.getAbsolutePath).run(outputIO)

      // Request the exit value now which will wait for the process to finish
      val exitValue = testProcess.exitValue()

      // Clean up the temporary executable
      outputFile.delete()
        
      val errorString = utf8InputStreamToString(stderr.get)
      
      if (exitValue == 0) {
        val outputString = utf8InputStreamToString(stdout.get)
        val output :: Nil = SchemeParser.parseStringAsData(outputString)

        ExecutionResult(success=true, output=output, errorString=errorString)
      }
      else {
        ExecutionResult(success=false, output=ast.UnspecificValue(), errorString=errorString)
      }
    }
    finally {
      outputFile.delete()
    }
  }
}
