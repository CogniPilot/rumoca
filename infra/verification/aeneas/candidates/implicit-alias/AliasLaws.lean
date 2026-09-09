import FinalAliasParameters

open Aeneas Aeneas.Std alias_parameters

namespace AliasLaws

theorem tag_preserves_parameter (n : Usize) : read_tag n = .ok n := by
  rfl

theorem wrapped_preserves_parameter (n : Usize) : read_wrapped n = .ok n := by
  rfl

theorem pair_preserves_parameter (n : Usize) (value : U32) :
    read_pair n value = .ok n := by
  rfl

theorem nominal_preserves_parameter (n : Usize) (tag : NominalTag n) :
    read_nominal tag = .ok n := by
  rfl

theorem brand_preserves_payload (value : U32) : branded_value () value = .ok value := by
  rfl

theorem identity_preserves_payload (T : Type) (value : T) : identity value = .ok value := by
  rfl

theorem wrapper_preserves_payload (T : Type) (value : WrappedValue T) :
    unwrap_value value = .ok value := by
  rfl

theorem array_preserves_length (n : Usize) (values : Std.Array U32 n) :
    array_length values = .ok n := by
  rfl

/-- info: 'AliasLaws.tag_preserves_parameter' does not depend on any axioms -/
#guard_msgs in
#print axioms tag_preserves_parameter
/-- info: 'AliasLaws.wrapped_preserves_parameter' does not depend on any axioms -/
#guard_msgs in
#print axioms wrapped_preserves_parameter
/-- info: 'AliasLaws.pair_preserves_parameter' does not depend on any axioms -/
#guard_msgs in
#print axioms pair_preserves_parameter
/-- info: 'AliasLaws.nominal_preserves_parameter' does not depend on any axioms -/
#guard_msgs in
#print axioms nominal_preserves_parameter
/-- info: 'AliasLaws.brand_preserves_payload' does not depend on any axioms -/
#guard_msgs in
#print axioms brand_preserves_payload
/-- info: 'AliasLaws.identity_preserves_payload' does not depend on any axioms -/
#guard_msgs in
#print axioms identity_preserves_payload
/-- info: 'AliasLaws.wrapper_preserves_payload' does not depend on any axioms -/
#guard_msgs in
#print axioms wrapper_preserves_payload
/-- info: 'AliasLaws.array_preserves_length' does not depend on any axioms -/
#guard_msgs in
#print axioms array_preserves_length

end AliasLaws
