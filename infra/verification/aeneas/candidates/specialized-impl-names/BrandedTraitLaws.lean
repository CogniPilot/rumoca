import BrandedTrait

open Aeneas Aeneas.Std

namespace BrandedTraitLaws

theorem branded_rhs_preserves_comparison (value : U32) (id : branded_trait.Id) :
    branded_trait.branded_rhs value id = .ok (decide (value = id.raw)) := by
  rfl

theorem branded_self_preserves_comparison (id : branded_trait.Id) (value : U32) :
    branded_trait.branded_self id value = .ok (decide (id.raw = value)) := by
  rfl

/-- info: 'BrandedTraitLaws.branded_rhs_preserves_comparison' does not depend on any axioms -/
#guard_msgs in
#print axioms branded_rhs_preserves_comparison

/-- info: 'BrandedTraitLaws.branded_self_preserves_comparison' does not depend on any axioms -/
#guard_msgs in
#print axioms branded_self_preserves_comparison

end BrandedTraitLaws
