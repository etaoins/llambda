package llambda

import collection.mutable.ListBuffer
import llambda.planner.{step => ps}

package object planner {
  type StepBuffer = ListBuffer[ps.Step]
}


