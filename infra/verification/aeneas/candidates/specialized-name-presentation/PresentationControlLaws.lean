import PresentationControls

open Aeneas Aeneas.Std

namespace PresentationControlLaws

theorem constructor_and_method_preserve_payload (value : U32) :
    presentation_controls.read value = .ok value := by
  rfl

/-- info: 'PresentationControlLaws.constructor_and_method_preserve_payload' does not depend on any axioms -/
#guard_msgs in
#print axioms constructor_and_method_preserve_payload

end PresentationControlLaws
