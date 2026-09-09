import VecBoxBoundary.Funs

open Aeneas Aeneas.Std vec_box_boundary

namespace VecBoxMutation

theorem moved_sequence_is_empty {T : Type} (values : alloc.vec.Vec T) :
    move_values values = Result.ok (alloc.vec.Vec.new T).slice := by
  rfl

/-- info: 'VecBoxMutation.moved_sequence_is_empty' depends on axioms: [propext] -/
#guard_msgs in
#print axioms moved_sequence_is_empty

end VecBoxMutation
