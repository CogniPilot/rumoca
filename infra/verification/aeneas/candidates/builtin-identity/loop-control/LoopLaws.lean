import LoopConstructorSource

open Aeneas Aeneas.Std
open loop_constructor_identity

namespace LoopLaws

theorem continuation_is_not_a_source_call :
    once_loop.body true = .ok (.cont false) := by
  rfl

theorem exit_is_not_a_source_call :
    once_loop.body false = .ok (.done ()) := by
  rfl

theorem loop_exits (again : Bool) : once_loop again = .ok () := by
  cases again with
  | false =>
    unfold once_loop
    rw [loop]
    rfl
  | true =>
    unfold once_loop
    rw [loop]
    change loop once_loop.body false = .ok ()
    rw [loop]
    rfl

theorem source_payload_is_preserved (again : Bool) (value : U32) :
    once again value = .ok value := by
  unfold once
  rw [loop_exits]
  rfl

/-- info: 'LoopLaws.continuation_is_not_a_source_call' does not depend on any axioms -/
#guard_msgs in
#print axioms continuation_is_not_a_source_call

/-- info: 'LoopLaws.exit_is_not_a_source_call' does not depend on any axioms -/
#guard_msgs in
#print axioms exit_is_not_a_source_call

/-- info: 'LoopLaws.loop_exits' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms loop_exits

/-- info: 'LoopLaws.source_payload_is_preserved' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms source_payload_is_preserved

end LoopLaws
