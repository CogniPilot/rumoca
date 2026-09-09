import BoxSliceBoundary.Funs

open Aeneas Aeneas.Std box_slice_boundary

namespace BoxSliceModelMutation

theorem clone_is_ignored {T : Type}
    (inst : core.clone.Clone T) (values : Slice T) :
    box_values inst values = Result.ok values := by
  rfl

/-- info: 'BoxSliceModelMutation.clone_is_ignored' depends on axioms: [propext] -/
#guard_msgs in
#print axioms clone_is_ignored

end BoxSliceModelMutation
