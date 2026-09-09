import FinalAliasParameters

open Aeneas Aeneas.Std alias_parameters

namespace MutationWitness

theorem tag_discards_parameter (n : Usize) : read_tag n = .ok 0#usize := by
  rfl

/-- info: 'MutationWitness.tag_discards_parameter' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms tag_discards_parameter

end MutationWitness
