import LocalBasename

open Aeneas Aeneas.Std

namespace LocalBasenameLaws

theorem unnamed_temporary_preserves_payload (raw : U32) :
    local_basename.read raw = .ok raw := by
  rfl

/-- info: 'LocalBasenameLaws.unnamed_temporary_preserves_payload' does not depend on any axioms -/
#guard_msgs in
#print axioms unnamed_temporary_preserves_payload

end LocalBasenameLaws
