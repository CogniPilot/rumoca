import StaticOwnerControls

open Aeneas Aeneas.Std

namespace static_owner_controls

theorem global_fields : global = Result.ok { first := 11#u32, second := 29#u32 } := by
  unfold global VALUES
  rfl

theorem projection_selects_second (value : Pair) : project value = Result.ok value.second := by
  rfl

theorem call_preserves_second : through_call = Result.ok 29#u32 := by
  unfold through_call global project VALUES
  rfl

theorem copied_fields :
    copies = Result.ok ({ first := 11#u32, second := 29#u32 },
                        { first := 11#u32, second := 29#u32 }) := by
  unfold copies global VALUES
  rfl

theorem scope_preserves_second : local_scope = Result.ok 29#u32 := by
  unfold local_scope global VALUES
  rfl

theorem nested_preserves_value (value : U32) : nested value = Result.ok value := by
  rfl

theorem ordinary_preserves_value (value : U32) : ordinary value = Result.ok value := by
  rfl

#print axioms global_fields
#print axioms projection_selects_second
#print axioms call_preserves_second
#print axioms copied_fields
#print axioms scope_preserves_second
#print axioms nested_preserves_value
#print axioms ordinary_preserves_value

end static_owner_controls
