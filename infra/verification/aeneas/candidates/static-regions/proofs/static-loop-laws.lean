import StaticOwnerAccumulate

open Aeneas Aeneas.Std

namespace static_owner_loop

theorem selected_first : selected 0#usize = Result.ok 11#u32 := by
  unfold selected TABLE
  rfl

theorem selected_second : selected 1#usize = Result.ok 29#u32 := by
  unfold selected TABLE
  rfl

theorem accumulation_step_preserves_selected_value
    (count : U32) (index : Usize) (value total iteration : U32)
    (selected_value : selected index = Result.ok value) :
    accumulate_static_loop.body count index total iteration =
      accumulate_ordinary_loop.body count value total iteration := by
  simp only [accumulate_static_loop.body, accumulate_ordinary_loop.body,
    selected_value, ordinary_selected]

theorem accumulation_preserves_selected_value
    (count : U32) (index : Usize) (value : U32)
    (selected_value : selected index = Result.ok value) :
    accumulate_static count index = accumulate_ordinary count value := by
  unfold accumulate_static accumulate_ordinary
    accumulate_static_loop accumulate_ordinary_loop
  congr 1
  funext state
  exact accumulation_step_preserves_selected_value
    count index value state.1 state.2 selected_value

/--
info: 'static_owner_loop.accumulation_step_preserves_selected_value' depends on axioms: [propext,
 Classical.choice,
 Quot.sound]
-/
#guard_msgs in
#print axioms accumulation_step_preserves_selected_value

/-- info: 'static_owner_loop.accumulation_preserves_selected_value' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms accumulation_preserves_selected_value

/-- info: 'static_owner_loop.selected_first' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms selected_first

/-- info: 'static_owner_loop.selected_second' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms selected_second

end static_owner_loop
