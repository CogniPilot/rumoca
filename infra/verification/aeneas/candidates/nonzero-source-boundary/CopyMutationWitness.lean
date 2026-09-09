import NonzeroCopyBoundary.Funs

open Aeneas Aeneas.Std nonzero_source_boundary

namespace NonzeroCopyMutation

theorem discards_every_input
    (value : Option (_root_.core.num.nonzero.NonZero Std.U32
      _root_.core.num.niche_types.NonZeroU32Inner)) :
    clone_optional value = Result.ok none := by
  cases value <;> rfl

/-- info: 'NonzeroCopyMutation.discards_every_input' does not depend on any axioms -/
#guard_msgs in
#print axioms discards_every_input

end NonzeroCopyMutation
