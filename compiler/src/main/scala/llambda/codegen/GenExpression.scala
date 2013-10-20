package llambda.codegen

import llambda.{StorageLocation, InternalCompilerErrorException}
import llambda.{et, nfi}
import llambda.codegen.llvmir._
import llambda.codegen.{boxedtype => bt}

case class ExpressionResult(
  state : GenerationState,
  value : LiveValue)

object GenExpression {
  def apply(initialState : GenerationState)(expr : et.Expression) : ExpressionResult = {
    expr match {
      case et.Bind(bindings) =>
        val finalState = bindings.foldLeft(initialState) { case (state, (storageLoc, bindValue)) =>
          // Evaluate the expression
          val bindResult = GenExpression(state)(bindValue)

          if (state.mutableVariables.contains(storageLoc)) {
            // Cast to %datum*
            val (castState, datumValue) = bindResult.value.toRequiredNativeType(bindResult.state)(nfi.BoxedValue(bt.BoxedDatum))

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
            val newLiveImmutables = bindResult.state.liveImmutables + (storageLoc -> bindResult.value)

            // Return the modified state
            bindResult.state.copy(
              liveImmutables=newLiveImmutables
            )
          }
        }

        ExpressionResult(state=finalState, value=LiveUnspecific)
        
      case et.VarRef(storageLoc : StorageLocation) if !initialState.mutableVariables.contains(storageLoc) =>
        // Immutable variable - this is trivial
        ExpressionResult(
          state=initialState,
          value=initialState.liveImmutables(storageLoc)
        )
      
      case et.VarRef(storageLoc : StorageLocation) =>
        // Mutable variable. This is unfortunate
        val block = initialState.currentBlock

        val boxedMutable = initialState.liveMutables(storageLoc)
        val currentDatum = bt.BoxedMutableVar.genLoadFromCurrentValue(block)(boxedMutable)

        val possibleTypes = (bt.BoxedDatum.subtypes).collect({
          case concrete : bt.ConcreteBoxedType => concrete
        }).toSet

        val liveValue = new BoxedLiveValue(possibleTypes, currentDatum)

        ExpressionResult(
          state=initialState,
          value=liveValue
        )

      case et.VarRef(_) =>
        throw new InternalCompilerErrorException("Non-StorageLocation VarRef leaked to codegen")

      case et.MutateVar(storageLoc, value) =>
        // Find our MutableVar's IrValue
        val boxedMutable = initialState.liveMutables(storageLoc)
            
        // Evaluate the new value for the variable
        val valueResult = GenExpression(initialState)(value)
        
        // Cast to %datum*
        val (castState, datumValue) = valueResult.value.toRequiredNativeType(valueResult.state)(nfi.BoxedValue(bt.BoxedDatum))

        // Store to the mutable variable
        bt.BoxedMutableVar.genStoreToCurrentValue(valueResult.state.currentBlock)(datumValue, boxedMutable)

        ExpressionResult(
          state=castState,
          value=LiveUnspecific
        )

      case et.Literal(datum) =>
        ExpressionResult(state=initialState, value=GenLiteral(initialState.module)(datum))

      case et.Cond(test, trueExpr, falseExpr) =>
        // Generate the test
        val testResult = GenExpression(initialState)(test)

        // Turn it in to a predicate
        val truthyPred = testResult.value.genTruthyPredicate(testResult.state)
        
        // Make two blocks
        val trueBlock = testResult.state.currentBlock.startChildBlock("condTrue")
        val falseBlock = testResult.state.currentBlock.startChildBlock("condFalse")

        // Branch!
        testResult.state.currentBlock.condBranch(truthyPred, trueBlock, falseBlock)

        // Continue generation down both branches after splitting our state
        val trueStartState = testResult.state.copy(currentBlock=trueBlock)
        val falseStartState = testResult.state.copy(currentBlock=falseBlock)

        val trueResult = GenExpression(trueStartState)(trueExpr)
        val falseResult = GenExpression(falseStartState)(falseExpr)

        // Phi the values together
        val (phiState, phiValue) = trueResult.value.genPhiWith(trueResult.state, falseResult.state)(falseResult.value)

        ExpressionResult(state=phiState, value=phiValue)

      case nativeFunc : et.NativeFunction =>
        val funcValue = GenNativeFunction(initialState.module)(nativeFunc) 
        ExpressionResult(state=initialState, value=funcValue)

      case et.Apply(procedure, operands) =>
        // Generate the procedure value
        val procResult = apply(initialState)(procedure)
        val procValue = procResult.value

        // Now generate each operand
        val (operandState, operandValues) = operands.foldLeft((procResult.state, List[LiveValue]())) { case ((state, operandValues), operand) =>
          val operandResult = GenExpression(state)(operand)

          (operandResult.state, operandValues :+ operandResult.value)
        }

        GenApplication(operandState)(procValue, operandValues)

      case et.Begin(expressions) =>
        val defaultResult = ExpressionResult(
          state=initialState,
          value=LiveUnspecific
        )

        expressions.foldLeft(defaultResult) { case (currentResult, expression) =>
          GenExpression(currentResult.state)(expression)
        }
    }
  }
}
