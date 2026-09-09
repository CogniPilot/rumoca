import PreservedSignatureNoSpecializationNoRoot.Funs
import SimplerClosures4.Funs

open Aeneas Aeneas.Std

namespace PairedClosureLaws

theorem unrelated_outer_call (value : U32) :
    preserved_signature.call value = Result.ok value := by
  rfl

theorem captured_reference_call (value : U32) :
    preserved_signature.captured_call value = Result.ok value := by
  rfl

theorem captured_move_call (value : U32) :
    preserved_signature.captured_move value = Result.ok value := by
  rfl

theorem named_call (value : U32) :
    simpler_closures.named_call value = Result.ok value := by
  rfl

theorem higher_ranked_call (value : U32) :
    simpler_closures.higher_ranked_call value = Result.ok value := by
  rfl

/-- info: 'PairedClosureLaws.unrelated_outer_call' does not depend on any axioms -/
#guard_msgs in
#print axioms unrelated_outer_call
/-- info: 'PairedClosureLaws.captured_reference_call' does not depend on any axioms -/
#guard_msgs in
#print axioms captured_reference_call
/-- info: 'PairedClosureLaws.captured_move_call' does not depend on any axioms -/
#guard_msgs in
#print axioms captured_move_call
/-- info: 'PairedClosureLaws.named_call' does not depend on any axioms -/
#guard_msgs in
#print axioms named_call
/-- info: 'PairedClosureLaws.higher_ranked_call' does not depend on any axioms -/
#guard_msgs in
#print axioms higher_ranked_call

end PairedClosureLaws
