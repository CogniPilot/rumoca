import BrandedTrait

open Aeneas Aeneas.Std

namespace BrandedTraitMutationWitness

theorem inverted_source_rejects_equal_values :
    branded_trait.branded_self { raw := 7#u32, brand := () } 7#u32 = .ok false := by
  rfl

/--
info: 'BrandedTraitMutationWitness.inverted_source_rejects_equal_values' depends on axioms: [propext,
 Classical.choice,
 Quot.sound]
-/
#guard_msgs in
#print axioms inverted_source_rejects_equal_values

end BrandedTraitMutationWitness
