import BuiltinConstructorSource

open Aeneas Aeneas.Std
open builtin_constructor_identity

namespace ConstructorLaws

theorem named_ok_preserves_value (value : U32) :
    shadow.ok value = .ok value := by
  rfl

theorem named_fail_preserves_failure : shadow.fail = .fail .panic := by
  rfl

theorem named_panic_preserves_failure : shadow.panic = .fail .panic := by
  rfl

/-- info: 'ConstructorLaws.named_ok_preserves_value' does not depend on any axioms -/
#guard_msgs in
#print axioms named_ok_preserves_value

/-- info: 'ConstructorLaws.named_fail_preserves_failure' does not depend on any axioms -/
#guard_msgs in
#print axioms named_fail_preserves_failure

/-- info: 'ConstructorLaws.named_panic_preserves_failure' does not depend on any axioms -/
#guard_msgs in
#print axioms named_panic_preserves_failure

end ConstructorLaws
