package llambda.frontend

import org.scalatest.FunSuite
import llambda._

import SchemeStringImplicits._

class ResolveIncludeListSuite extends FunSuite { 
  val resourceBaseUrl = getClass.getClassLoader.getResource("")
  val includeBaseUrl = getClass.getClassLoader.getResource("includes/")
  
  implicit val includePath = frontend.IncludePath(
    fileParentDir=Some(resourceBaseUrl),
    packageRootDir=Some(resourceBaseUrl)
  )

  test("zero includes fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(Nil)
    }
  }

  test("including a symbol fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(List(ast.Symbol("test")))
    }
  }

  test("including an exact integer fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(List(ast.IntegerLiteral(1)))
    }
  }
    
  test("including a proper list fails") {
    intercept[BadSpecialFormException] {
      ResolveIncludeList(List(
        ast.StringLiteral("includes/include1.scm"),
        ast.ProperList(List(
          ast.StringLiteral("World")
        ))
      ))
    }
  }
  
  test("including a missing file fails") {
    intercept[IncludeNotFoundException] {
      ResolveIncludeList(List(ast.StringLiteral("doesntexist.scm")))
    }
  }


  test("including a single file") {
    assert(ResolveIncludeList(List(
      ast.StringLiteral("includes/include1.scm")
    )) === List(
      IncludeLoadResult(
        innerIncludePath=frontend.IncludePath(
          packageRootDir=Some(resourceBaseUrl),
          fileParentDir=Some(includeBaseUrl)
        ),
        data=List(
          ast.StringLiteral("include1-line1"),
          ast.StringLiteral("include1-line2")
        )
      )
    ))
  }
  
  test("including multiple files") {
    assert(ResolveIncludeList(List(
      ast.StringLiteral("includes/include1.scm"),
      ast.StringLiteral("includes/include2.scm")
    )) === List(
      IncludeLoadResult(
        innerIncludePath=frontend.IncludePath(
          packageRootDir=Some(resourceBaseUrl),
          fileParentDir=Some(includeBaseUrl)
        ),
        data=List(
          ast.StringLiteral("include1-line1"),
          ast.StringLiteral("include1-line2")
        )
      ),
      IncludeLoadResult(
        innerIncludePath=frontend.IncludePath(
          packageRootDir=Some(resourceBaseUrl),
          fileParentDir=Some(includeBaseUrl)
        ),
        data=List(
          ast.StringLiteral("include2-line1"),
          ast.StringLiteral("include2-line2")
        )
      )
    ))
  }
}
