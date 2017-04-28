package io.llambda.compiler.functional


// Only run at -O2 because these tests are time consuming and tail recursion is unreliable at -O0
class SampleProgramSuite extends SchemeFunctionalTestRunner("SampleProgramSuite", onlyOptimised=true)
