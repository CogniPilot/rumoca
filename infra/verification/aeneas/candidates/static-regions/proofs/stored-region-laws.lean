import ClosedStatic
import «Brand-mixedMutIdentity»

open Aeneas Aeneas.Std

theorem closed_field_preserves_value (value : static_region_generics.ClosedOuter) :
    static_region_generics.closed_static value = Result.ok value.inner.value := by
  rfl

theorem mixed_mut_preserves_forward_and_update
    (value : static_region_brands.MixedMut) (replacement : U32) :
    (do
      let (returned, back) ← static_region_brands.mixed_mut_identity value
      let restored := back { returned with value := replacement }
      Result.ok (returned.value, restored.value)) = Result.ok (value.value, replacement) := by
  rfl

/-- info: 'closed_field_preserves_value' does not depend on any axioms -/
#guard_msgs in
#print axioms closed_field_preserves_value
/-- info: 'mixed_mut_preserves_forward_and_update' does not depend on any axioms -/
#guard_msgs in
#print axioms mixed_mut_preserves_forward_and_update
