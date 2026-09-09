import VecBoxBoundary.Funs

open Aeneas Aeneas.Std vec_box_boundary

namespace VecBoxLaws

theorem move_preserves_sequence {T : Type} (values : alloc.vec.Vec T) :
    move_values values = Result.ok values.slice := by
  rfl

theorem from_preserves_sequence {T : Type} (values : alloc.vec.Vec T) :
    from_values values = Result.ok values.slice := by
  rfl

theorem both_routes_agree {T : Type} (values : alloc.vec.Vec T) :
    move_values values = from_values values := by
  rfl

/-- info: 'VecBoxLaws.move_preserves_sequence' depends on axioms: [propext] -/
#guard_msgs in
#print axioms move_preserves_sequence
/-- info: 'VecBoxLaws.from_preserves_sequence' depends on axioms: [propext] -/
#guard_msgs in
#print axioms from_preserves_sequence
/-- info: 'VecBoxLaws.both_routes_agree' depends on axioms: [propext] -/
#guard_msgs in
#print axioms both_routes_agree

end VecBoxLaws
