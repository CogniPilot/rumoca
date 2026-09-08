import RumocaRootValueFixture
import RumocaRootValueContract

/-! # Root value witnesses

Model-domain witnesses for `RootValuesRelation`, built on the single base root in
`RumocaRootValueFixture`. Each says only that the relation and the projection
behave as claimed on values of the generated `SolveModel` type. None of them
claims such a root is reachable from a Modelica source, from `lower_solve_model`,
or from any production caller; the Rust fixtures carry that, and an empty root
here is a domain fixture rather than a compilation claim.

The fixture's field values decide what these theorems mean, so it is part of the
review surface, not scaffolding under it.
-/

open Aeneas Aeneas.Std
open rumoca_phase_solve
open rumoca_phase_solve.scalar_constant_derivative_refinement
open RumocaRootValueContract
open RumocaRootValueFixture

namespace RumocaRootValueWitness

/-- The projection accepts the base root. Without this, every refusal witness
below would be consistent with a projection that never succeeds at all. -/
theorem base_root_projection_succeeds :
    IsOk (project_solve_values_with_owner_premises baseModel owners0 meta0) = true := by rfl

/-- An empty catalog carries no start, and reports absence rather than refusing. -/
theorem empty_catalog_reports_absent_start :
    startFacts baseModel = some (false, 0#usize, 0#u64) := by rfl

theorem absent_start_reports_absent :
    startFacts (modelWithStart none) = some (false, 0#usize, 0#u64) := by rfl

/-- A present but empty start agrees with an absent one on width and bits. -/
theorem present_empty_start_reports_present :
    startFacts (modelWithStart (some (Slice.from [] (by scalar_tac))))
      = some (true, 0#usize, 0#u64) := by rfl

/-- The discriminating pair: the two differ only in the presence flag, so that
flag is not derivable from width and bits and cannot be dropped as redundant. -/
theorem absent_and_present_empty_start_differ :
    startFacts (modelWithStart none)
      ≠ startFacts (modelWithStart (some (Slice.from [] (by scalar_tac)))) := by decide

theorem positive_zero_start_bits :
    startFacts (bitsModel 0#u64) = some (true, 1#usize, 0#u64) := by rfl

theorem negative_zero_start_bits :
    startFacts (bitsModel 0x8000000000000000#u64)
      = some (true, 1#usize, 0x8000000000000000#u64) := by rfl

/-- Negative and positive zero reach different facts. A projection carrying a
numeric value rather than storage bits would identify these two roots. -/
theorem signed_zero_starts_differ :
    startFacts (bitsModel 0#u64) ≠ startFacts (bitsModel 0x8000000000000000#u64) := by decide

theorem base_visible_block_is_refused :
    project_scalar_block 2#usize base_rumoca_ir_solve_ScalarProgramBlock
      = .ok .UnsupportedShape := by rfl

/-- Reaching `Exact` matters: without it every block witness would be a refusal,
and a projection that refused unconditionally would satisfy them all. -/
theorem exact_block_is_not_refused :
    project_scalar_block 2#usize exactBlock2 ≠ .ok .UnsupportedShape := by
  rw [RumocaProjectionContract.scalar_block_projection_eq]
  rw [if_pos (show exactBlock2.output_indices.val = [0#usize] from rfl)]
  intro refused
  exact (RumocaProjectionContract.projection_refuses_iff 2#usize _).mp refused ⟨prog2, rfl, rfl⟩

/-- Two roots differing only in `visible_value_rows` cannot share one fact
record, so the relation reads that field rather than supplying it. -/
theorem visible_rows_field_is_observed (facts : SolveFacts)
    (onExact : RootValuesRelation (modelWithVisible exactBlock2) owners0 meta0 facts) :
    ¬ RootValuesRelation baseModel owners0 meta0 facts := by
  intro onBase
  have fromBase : project_scalar_block 2#usize base_rumoca_ir_solve_ScalarProgramBlock
      = .ok facts.visible_rows := onBase.2.2.2.2.2.1
  have fromExact : project_scalar_block 2#usize exactBlock2
      = .ok facts.visible_rows := onExact.2.2.2.2.2.1
  rw [base_visible_block_is_refused] at fromBase
  exact exact_block_is_not_refused (fromExact.trans fromBase.symm)

/-- info: 'RumocaRootValueWitness.base_root_projection_succeeds' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms base_root_projection_succeeds
/-- info: 'RumocaRootValueWitness.empty_catalog_reports_absent_start' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms empty_catalog_reports_absent_start
/-- info: 'RumocaRootValueWitness.absent_start_reports_absent' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms absent_start_reports_absent
/-- info: 'RumocaRootValueWitness.present_empty_start_reports_present' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms present_empty_start_reports_present
/-- info: 'RumocaRootValueWitness.absent_and_present_empty_start_differ' depends on axioms: [propext,
 Classical.choice,
 Quot.sound] -/
#guard_msgs in
#print axioms absent_and_present_empty_start_differ
/-- info: 'RumocaRootValueWitness.positive_zero_start_bits' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms positive_zero_start_bits
/-- info: 'RumocaRootValueWitness.negative_zero_start_bits' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms negative_zero_start_bits
/-- info: 'RumocaRootValueWitness.signed_zero_starts_differ' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms signed_zero_starts_differ
/-- info: 'RumocaRootValueWitness.base_visible_block_is_refused' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms base_visible_block_is_refused
/-- info: 'RumocaRootValueWitness.exact_block_is_not_refused' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms exact_block_is_not_refused
/-- info: 'RumocaRootValueWitness.visible_rows_field_is_observed' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms visible_rows_field_is_observed

end RumocaRootValueWitness
