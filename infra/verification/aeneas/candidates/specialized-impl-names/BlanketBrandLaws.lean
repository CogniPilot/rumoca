import BlanketBrand

open Aeneas Aeneas.Std

namespace BlanketBrandLaws

theorem specialized_blanket_keeps_receiver
    (id : blanket_brand.branded_trait.Id) (value : U32) :
    blanket_brand.keep_with_brand id value = .ok value := by
  rfl

/-- info: 'BlanketBrandLaws.specialized_blanket_keeps_receiver' does not depend on any axioms -/
#guard_msgs in
#print axioms specialized_blanket_keeps_receiver

end BlanketBrandLaws
