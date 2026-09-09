import BuiltinCaptureSource

open Aeneas Aeneas.Std
open builtin_constructor_identity

namespace CaptureContract

theorem identity_preserved (value : U32) :
    capture.identity value = .ok value := by
  rfl

theorem neighbor_failure_preserved (value : U32) :
    capture.ok value = .fail .panic := by
  rfl

/-- info: 'CaptureContract.identity_preserved' does not depend on any axioms -/
#guard_msgs in
#print axioms identity_preserved

/-- info: 'CaptureContract.neighbor_failure_preserved' does not depend on any axioms -/
#guard_msgs in
#print axioms neighbor_failure_preserved

end CaptureContract
