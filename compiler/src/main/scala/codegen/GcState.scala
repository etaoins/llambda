package io.llambda.compiler.codegen


case class GcState(
  rootedIdentities: Map[GcPointerIdentity, Int] = Map(),
  nextUnallocatedSlot: Int = 0
)

object GcState {
  def fromBranches(continuingState: GcState, otherStates: List[GcState]) = {
    val nextUnallocatedSlot = (continuingState :: otherStates).map(_.nextUnallocatedSlot).max

    // This is tricky and depends on a few assumptions:
    // 1) At the end of every branch all newly allocated slots are idle. This is because we pre-root everything live in
    //    the parent state so any new values must be values created in the branch. These values can't live past the end
    //    of the branch and should be all explicitly disposed
    // 2) Any slots allocated in one branch must be null in the other because they would've been nulled in the entry
    //    block
    GcState(
      rootedIdentities=continuingState.rootedIdentities,
      nextUnallocatedSlot=nextUnallocatedSlot
    )
  }
}
