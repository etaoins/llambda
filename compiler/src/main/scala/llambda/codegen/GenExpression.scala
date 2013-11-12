package llambda.codegen

import llambda.{StorageLocation, InternalCompilerErrorException}
import llambda.{et, nfi}
import llambda.codegen.llvmir._
import llambda.codegen.{boxedtype => bt}

object GenExpression {
  def apply(initialState : GenerationState)(expr : et.Expression) : (GenerationState, LiveValue) = {
    expr match {
      case et.Bind(bindings) =>
        val finalState = bindings.foldLeft(initialState) { case (state, (storageLoc, bindExpression)) =>
          // Evaluate the expression
          val (newState, bindValue) = GenExpression(state)(bindExpression)

          if (newState.mutableVariables.contains(storageLoc)) {
            // Cast to %datum*
            val (castState, datumValue) = bindValue.toRequiredNativeType(newState)(nfi.BoxedValue(bt.BoxedDatum))

            // Allocate a new Cons for the mutable variable
            val (allocState, allocation) = GenConsAllocation(castState)(1)

            val mutableCons = allocation.genTypedPointer(allocState)(0, bt.BoxedMutableVar) 
            bt.BoxedMutableVar.genStoreToCurrentValue(allocState.currentBlock)(datumValue, mutableCons)

            val newLiveMutables = allocState.liveMutables + (storageLoc -> mutableCons)

            allocState.copy(
              liveMutables=newLiveMutables
            )
          }
          else {
            // Add the new variable to our liveImmutables
            val newLiveImmutables = newState.liveImmutables + (storageLoc -> bindValue)

            // Return the modified state
            newState.copy(
              liveImmutables=newLiveImmutables
            )
          }
        }

        (finalState, LiveUnspecific)
        
      case et.VarRef(storageLoc : StorageLocation) if !initialState.mutableVariables.contains(storageLoc) =>
        // Immutable variable - this is trivial
        (initialState, initialState.liveImmutables(storageLoc))
      
      case et.VarRef(storageLoc : StorageLocation) =>
        // Mutable variable. This is unfortunate
        val block = initialState.currentBlock

        val boxedMutable = initialState.liveMutables(storageLoc)
        val currentDatum = bt.BoxedMutableVar.genLoadFromCurrentValue(block)(boxedMutable)

        val possibleTypes = bt.BoxedDatum.concreteTypes
        val liveValue = new BoxedLiveValue(possibleTypes, currentDatum)

        (initialState, liveValue)

      case et.VarRef(_) =>
        throw new InternalCompilerErrorException("Non-StorageLocation VarRef leaked to codegen")

      case et.MutateVar(storageLoc, expression) =>
        // Find our MutableVar's IrValue
        val boxedMutable = initialState.liveMutables(storageLoc)
            
        // Evaluate the new value for the variable
        val (genState, value) = GenExpression(initialState)(expression)
        
        // Cast to %datum*
        val (castState, datumValue) = value.toRequiredNativeType(genState)(nfi.BoxedValue(bt.BoxedDatum))

        // Store to the mutable variable
        bt.BoxedMutableVar.genStoreToCurrentValue(castState.currentBlock)(datumValue, boxedMutable)

        (castState, LiveUnspecific)

      case et.Literal(datum) =>
        (initialState, GenLiteral(initialState.module)(datum))

      case et.Cond(test, trueExpr, falseExpr) =>
        // Generate the test
        val (testState, value) = GenExpression(initialState)(test)

        // Turn it in to a predicate
        val truthyPred = value.genTruthyPredicate(testState)
        
        // Make two blocks
        val trueBlock = testState.currentBlock.startChildBlock("condTrue")
        val falseBlock = testState.currentBlock.startChildBlock("condFalse")

        // Branch!
        testState.currentBlock.condBranch(truthyPred, trueBlock, falseBlock)

        // Continue generation down both branches after splitting our state
        val trueStartState = testState.copy(currentBlock=trueBlock)
        val falseStartState = testState.copy(currentBlock=falseBlock)

        val (trueState, trueValue) = GenExpression(trueStartState)(trueExpr)
        val (falseState, falseValue) = GenExpression(falseStartState)(falseExpr)

        // Phi the values together
        trueValue.genPhiWith(trueState, falseState)(falseValue)

      case nativeFunc : et.NativeFunction =>
        val funcValue = GenNativeFunction(initialState.module)(nativeFunc) 
        (initialState, funcValue)

      case et.Apply(procedure, operands) =>
        // Generate the procedure value
        val (procState, procValue) = apply(initialState)(procedure)

        // Now generate each operand
        val (operandState, operandValues) = operands.foldLeft((procState, List[LiveValue]())) { case ((state, operandValues), operand) =>
          val (operandState, operandValue) = GenExpression(state)(operand)

          (operandState, operandValues :+ operandValue)
        }

        GenApplication(operandState)(procValue, operandValues)

      case et.Begin(expressions) =>
        val defaultResult = (initialState, LiveUnspecific : LiveValue)

        expressions.foldLeft(defaultResult) { case ((currentState, _), expression) =>
          GenExpression(currentState)(expression)
        }
    }
  }
}
