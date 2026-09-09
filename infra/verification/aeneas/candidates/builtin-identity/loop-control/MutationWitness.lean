import LoopConstructorSource

open Aeneas Aeneas.Std
open loop_constructor_identity

namespace MutationWitness

theorem wrong_payload (value : U32) : once true value = .ok 0#u32 := by
  unfold once once_loop
  rw [loop]
  change (do loop once_loop.body false; done value) = .ok 0#u32
  rw [loop]
  rfl

/-- info: 'MutationWitness.wrong_payload' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms wrong_payload

end MutationWitness
