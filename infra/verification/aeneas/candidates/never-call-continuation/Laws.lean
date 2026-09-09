import NeverCall.Funs
import NeverScopes.Funs

open Aeneas Aeneas.Std

namespace NeverCallLaws

def outcome {α : Type} (result : Result Never) : Result α :=
  match result with
  | .ok value => nomatch value
  | .fail error => .fail error
  | .div => .div

theorem relay_outcome {S : Type} (model : never_call.NeverSource S) (source : S) :
    never_call.relay model source = outcome (model.run source) := by
  cases h : model.run source with
  | ok value => cases value
  | fail error => simp [never_call.relay, outcome, h]
  | div => simp [never_call.relay, outcome, h]

theorem branch_taken_outcome {S : Type}
    (model : never_call.NeverSource S) (source : S) :
    never_call.branch model source true = outcome (model.run source) := by
  cases h : model.run source with
  | ok value => cases value
  | fail error => simp [never_call.branch, outcome, h]
  | div => simp [never_call.branch, outcome, h]

theorem branch_ordinary {S : Type}
    (model : never_call.NeverSource S) (source : S) :
    never_call.branch model source false = .ok 7#u32 := by
  rfl

theorem relay_never_outcome {S : Type}
    (model : never_scopes.NeverSource S) (source : S) :
    never_scopes.relay_never model source = model.run source := by
  cases h : model.run source with
  | ok value => cases value
  | fail error => simp [never_scopes.relay_never, h]
  | div => simp [never_scopes.relay_never, h]

theorem borrowed_taken_outcome {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (value : U32) :
    never_scopes.borrowed model source value true =
      outcome (model.run source) := by
  cases h : model.run source with
  | ok result => cases result
  | fail error => simp [never_scopes.borrowed, outcome, h]
  | div => simp [never_scopes.borrowed, outcome, h]

theorem borrowed_ordinary {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (value : U32) :
    never_scopes.borrowed model source value false =
      .ok (value, fun replacement => replacement) := by
  rfl

theorem loop_body_taken_outcome {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (count : U32)
    (positive : count > 0#u32) :
    never_scopes.loop_branch_loop.body model source count true =
      outcome (model.run source) := by
  cases h : model.run source with
  | ok result => cases result
  | fail error =>
      simp [never_scopes.loop_branch_loop.body, outcome, h, positive]
  | div =>
      simp [never_scopes.loop_branch_loop.body, outcome, h, positive]

theorem loop_body_zero {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (take : Bool) :
    never_scopes.loop_branch_loop.body model source 0#u32 take =
      .ok (.done 0#u32) := by
  simp [never_scopes.loop_branch_loop.body]

theorem loop_taken_outcome {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (count : U32)
    (positive : count > 0#u32) :
    never_scopes.loop_branch model source count true =
      outcome (model.run source) := by
  unfold never_scopes.loop_branch never_scopes.loop_branch_loop
  rw [loop]
  cases h : model.run source with
  | ok result => cases result
  | fail error => simp [loop_body_taken_outcome model source count positive, outcome, h]
  | div => simp [loop_body_taken_outcome model source count positive, outcome, h]

theorem loop_zero {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (take : Bool) :
    never_scopes.loop_branch model source 0#u32 take = .ok 0#u32 := by
  unfold never_scopes.loop_branch never_scopes.loop_branch_loop
  rw [loop]
  simp [loop_body_zero]

theorem loop_body_ordinary_positive {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (count : U32)
    (positive : count > 0#u32) :
    never_scopes.loop_branch_loop.body model source count false =
      (do
        let next ← count - 1#u32
        Result.ok (.cont (next, false))) := by
  simp [never_scopes.loop_branch_loop.body, positive]

theorem loop_ordinary_step {S : Type}
    (model : never_scopes.NeverSource S) (source : S) (count : U32)
    (positive : count > 0#u32) :
    never_scopes.loop_branch model source count false =
      (do
        let next ← count - 1#u32
        never_scopes.loop_branch model source next false) := by
  unfold never_scopes.loop_branch never_scopes.loop_branch_loop
  rw [loop]
  cases step : count - 1#u32 <;>
    simp [never_scopes.loop_branch_loop.body, positive, step]

theorem loop_one_ordinary {S : Type}
    (model : never_scopes.NeverSource S) (source : S) :
    never_scopes.loop_branch model source 1#u32 false = .ok 0#u32 := by
  calc
    never_scopes.loop_branch model source 1#u32 false =
        (do
          let next ← 1#u32 - 1#u32
          never_scopes.loop_branch model source next false) :=
      loop_ordinary_step model source 1#u32 (by scalar_tac)
    _ = never_scopes.loop_branch model source 0#u32 false := by rfl
    _ = .ok 0#u32 := loop_zero model source false

end NeverCallLaws

/-- info: 'NeverCallLaws.relay_outcome' depends on axioms: [propext] -/
#guard_msgs in
#print axioms NeverCallLaws.relay_outcome
/-- info: 'NeverCallLaws.branch_taken_outcome' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.branch_taken_outcome
/-- info: 'NeverCallLaws.branch_ordinary' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.branch_ordinary
/-- info: 'NeverCallLaws.relay_never_outcome' depends on axioms: [propext] -/
#guard_msgs in
#print axioms NeverCallLaws.relay_never_outcome
/-- info: 'NeverCallLaws.borrowed_taken_outcome' depends on axioms: [propext] -/
#guard_msgs in
#print axioms NeverCallLaws.borrowed_taken_outcome
/-- info: 'NeverCallLaws.borrowed_ordinary' does not depend on any axioms -/
#guard_msgs in
#print axioms NeverCallLaws.borrowed_ordinary
/-- info: 'NeverCallLaws.loop_body_taken_outcome' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.loop_body_taken_outcome
/-- info: 'NeverCallLaws.loop_body_zero' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.loop_body_zero
/-- info: 'NeverCallLaws.loop_taken_outcome' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.loop_taken_outcome
/-- info: 'NeverCallLaws.loop_zero' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.loop_zero

/-- info: 'NeverCallLaws.loop_body_ordinary_positive' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.loop_body_ordinary_positive
/-- info: 'NeverCallLaws.loop_ordinary_step' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.loop_ordinary_step
/-- info: 'NeverCallLaws.loop_one_ordinary' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms NeverCallLaws.loop_one_ordinary
