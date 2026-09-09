import IteratorOwnedSource

open Aeneas Aeneas.Std
open iterator_specialization

namespace MutationWitness

def singleton : Slice U32 := Slice.from [7#u32] (by scalar_tac)

theorem wrong_count_is_observable :
    mapped_pair singleton 4#u32 = .ok (some (7#u32, 6#u32), none) := by
  rfl

theorem original_contract_is_false :
    mapped_pair singleton 4#u32 ≠ .ok (some (7#u32, 5#u32), none) := by
  rw [wrong_count_is_observable]
  intro h
  simp only [Result.ok.injEq, Prod.mk.injEq, Option.some.injEq] at h
  have counts := congrArg UScalar.val h.1.2
  norm_num at counts

/-- info: 'MutationWitness.wrong_count_is_observable' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms wrong_count_is_observable

/-- info: 'MutationWitness.original_contract_is_false' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms original_contract_is_false

end MutationWitness
