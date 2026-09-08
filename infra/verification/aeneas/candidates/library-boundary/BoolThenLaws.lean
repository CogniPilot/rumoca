import BoolThenSource

open Aeneas.Std
open dae_library_boundary

namespace BoolThenLaws

/-- Disabled dispatch returns None regardless of the callback's result. -/
theorem disabled {T F : Type} (callback : core.ops.function.FnOnce F Unit T)
    (state : F) : core.bool.Bool.then callback false state = .ok none := by
  rfl

/-- Enabled dispatch preserves the callback's value, failure, or divergence. -/
theorem enabled {T F : Type} (callback : core.ops.function.FnOnce F Unit T)
    (state : F) :
    core.bool.Bool.then callback true state =
      match callback.call_once state () with
      | .ok value => .ok (some value)
      | .fail error => .fail error
      | .div => .div := by
  cases result : callback.call_once state () <;>
    simp [core.bool.Bool.then, result]

/-- This also binds the Rust caller's guard, not just the generic library body. -/
theorem disabled_reference (values : Slice U32) :
    conditional_reference false values = .ok none := by
  rfl

/--
info: 'BoolThenLaws.disabled' does not depend on any axioms
-/
#guard_msgs in
#print axioms disabled

/--
info: 'BoolThenLaws.enabled' depends on axioms: [propext, Quot.sound]
-/
#guard_msgs in
#print axioms enabled

/--
info: 'BoolThenLaws.disabled_reference' depends on axioms: [propext, Classical.choice, Quot.sound]
-/
#guard_msgs in
#print axioms disabled_reference

end BoolThenLaws
