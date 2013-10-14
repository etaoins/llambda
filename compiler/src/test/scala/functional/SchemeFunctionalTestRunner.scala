package llambda

import java.io.File
import scala.sys.process._

import llambda.codegen.{boxedtype => bt}
import org.scalatest.{FunSuite, Inside}

abstract class SchemeFunctionalTestRunner(testName : String) extends FunSuite with Inside {
  private case class ExecutionResult(success : Boolean, output : ast.Datum)

  val resourcePath = s"functional/${testName}.scm"
  val stream = getClass.getClassLoader.getResourceAsStream(resourcePath)

  // Load the tests
  val allTestSource = io.Source.fromInputStream(stream).mkString

  SchemeParser(allTestSource) match {
    case SchemeParser.Success(parsed, _) =>
      runAllTests(parsed)
    case other =>
      fail(other.toString)
  }

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

        assert(result.success === true)
        assert(result.output === expectation)
      
      case ast.ProperList(ast.Symbol("expect-failure") :: program) if !program.isEmpty =>
        try {
          val result = executeProgram(program)

          // If we compiled make sure we fail at runtime
          assert(result.success === false)
        }
        catch {
          case e : SemanticException =>
            // Semantic exceptions are allowed
        }

      case other =>
          fail("Unable to parse condition: " + condition.toString)
    }
  }

  private def executeProgram(program : List[ast.Datum]) : ExecutionResult = {
    // Import llambda.nfi for native-function
    val importDecl = ast.ProperList(List(
      ast.Symbol("import"), 
      ast.ProperList(List(
        ast.Symbol("llambda"),
        ast.Symbol("nfi")))))

    // Modify the last expression to print using lliby_write
    val valueDatum = program.last

    val printValueDatum = ast.ProperList(List(
      ast.ProperList(List(
        ast.Symbol("native-function"),
        ast.StringLiteral("lliby_write"),
        ast.ProperList(List(ast.Symbol("boxed-datum"))),
        ast.Symbol("void"))),
      valueDatum))

    // Rebuild the program with the import and value printing
    val printingProgram = (importDecl :: program.dropRight(1)) :+ printValueDatum

    // Compile the program
    val outputFile = File.createTempFile("llambdafunc", null, null)

    // Optimize to catch more miscompilations
    Compiler.compileData(printingProgram, outputFile, optimizeLevel=2)

    // Create our output logger
    var outputString = new String
    var errorString = new String

    val outputLogger = ProcessLogger(
      line => outputString += line,
      line => errorString += line
    )

    // Call the program
    val testProcess = Process(outputFile.getAbsolutePath).run(outputLogger)
    // Request the exit value now which will wait for the process to finish
    val exitValue = testProcess.exitValue()
  
    if (exitValue == 0) {
      val output = SchemeParser(outputString) match {
        case SchemeParser.Success(data :: Nil, _) => data
        case other => fail(other.toString)
      }

      if (!errorString.isEmpty) {
        fail(errorString)
      }

      ExecutionResult(success=true, output=output)
    }
    else {
      ExecutionResult(success=false, output=ast.UnspecificValue)
    }
  }
}
