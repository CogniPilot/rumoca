import BuiltinCaptureSource

open Aeneas Aeneas.Std
open builtin_constructor_identity

namespace CaptureCounterexample

-- This diagnostic imports the predecessor output, never the fixed candidate.
theorem identity_was_captured (value : U32) :
    capture.identity value = .fail .panic := by
  rfl

theorem rust_identity_contract_is_false (value : U32) :
    capture.identity value ≠ .ok value := by
  rw [identity_was_captured]
  intro h
  cases h

/-- info: 'CaptureCounterexample.identity_was_captured' does not depend on any axioms -/
#guard_msgs in
#print axioms identity_was_captured

/-- info: 'CaptureCounterexample.rust_identity_contract_is_false' does not depend on any axioms -/
#guard_msgs in
#print axioms rust_identity_contract_is_false

end CaptureCounterexample
