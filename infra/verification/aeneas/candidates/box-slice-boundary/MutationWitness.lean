import BoxSliceBoundary.Funs

open Aeneas Aeneas.Std box_slice_boundary

namespace BoxSliceMutation

theorem successful_clone_is_discarded {T : Type}
    (inst : core.clone.Clone T) (values copied : Slice T)
    (h : Slice.clone inst.clone values = Result.ok copied) :
    box_values inst values = Result.ok (Slice.new T) := by
  simp only [box_values, alloc.boxed.FromBoxSliceSharedSlice.from, h]
  rfl

/-- info: 'BoxSliceMutation.successful_clone_is_discarded' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms successful_clone_is_discarded

end BoxSliceMutation
