import PreservedSignatureNoSpecializationNoRoot.Funs

open Aeneas Aeneas.Std

namespace PairedClosureMutationWitness

theorem captured_move_fails_at_42 :
    preserved_signature.captured_move 42#u32 = Result.fail .assertionFailure := by
  rfl

/-- info: 'PairedClosureMutationWitness.captured_move_fails_at_42' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms captured_move_fails_at_42

end PairedClosureMutationWitness
