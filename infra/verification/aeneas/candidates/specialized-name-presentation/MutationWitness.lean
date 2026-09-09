import DefaultMethods

open Aeneas Aeneas.Std

namespace DefaultMethodMutationWitness

theorem missing_negation_claims_equal_values_differ :
    default_methods.default_on_self { raw := 7#u32, brand := () } 7#u32 = .ok true := by
  rfl

/--
info: 'DefaultMethodMutationWitness.missing_negation_claims_equal_values_differ' depends on axioms: [propext,
 Classical.choice,
 Quot.sound]
-/
#guard_msgs in
#print axioms missing_negation_claims_equal_values_differ

end DefaultMethodMutationWitness
