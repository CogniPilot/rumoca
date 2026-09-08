import RumocaDaeFactContract
import RumocaSolveFactContract

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve variable_catalog_refinement
open rumoca_ir_dae.model.view rumoca_ir_solve.model

namespace RumocaOccurrenceContract

set_option autoImplicit false

/-- Equal-length, pointwise field preservation preserves the entire field list.
This is a list lemma, not a second implementation of a compiler projection. -/
theorem mapped_fields_eq {α β γ : Type} (source : List α) (target : List β)
    (sourceField : α → γ) (targetField : β → γ)
    (sameLength : target.length = source.length)
    (matched : ∀ (index : Nat) a b, source[index]? = some a → target[index]? = some b →
      targetField b = sourceField a) :
    target.map targetField = source.map sourceField := by
  apply List.ext_getElem?
  intro index
  simp only [List.getElem?_map]
  cases hs : source[index]? <;> cases ht : target[index]? <;> simp_all; grind

/-- Total production projection, with exact occurrence-list equality. -/
theorem dae_occurrences_spec (view : DaeVariableRefinementView) :
    project_dae_variables view ⦃ facts =>
      facts.val.map DaeVariableFact.occurrence =
        view.entries.val.map DaeVariableRefinementEntry.source_occurrence ⦄ := by
  step with RumocaDaeFactContract.view_projection_spec as ⟨facts, faithful⟩
  apply mapped_fields_eq _ _ _ _ faithful.1
  intro index source fact hsource hfact
  exact (faithful.2 index source fact hsource hfact).1

/-- This premise names the audited constructor invariant; it is not claimed for
every raw translated view. The actual projection neither invents nor merges IDs. -/
theorem dae_uniqueness_preserved (view : DaeVariableRefinementView)
    (issued : (view.entries.val.map
      DaeVariableRefinementEntry.source_occurrence).Nodup) :
    ∃ facts, project_dae_variables view = .ok facts ∧
      (facts.val.map DaeVariableFact.occurrence).Nodup := by
  obtain ⟨facts, runs, occurrences⟩ := spec_imp_exists (dae_occurrences_spec view)
  exact ⟨facts, runs, occurrences ▸ issued⟩

/-- Both projection steps are actual generated Rust functions. The original
catalog, not a supplied intermediate view, is the source of every occurrence. -/
theorem solve_occurrences_spec (model : SolveModel) :
    (do
      let view ← SolveModel.variable_refinement model
      project_solve_variables view) ⦃ facts =>
        facts.val.map SolveVariableFact.occurrence =
          model.variable_catalog.entries.val.map
            (fun entry => entry.source.source_occurrence) ⦄ := by
  obtain ⟨facts, runs, faithful⟩ :=
    RumocaSolveFactContract.root_facts_total_and_faithful model
  rw [runs]
  simp only [spec_ok]
  apply mapped_fields_eq _ _ _ _ faithful.1
  intro index source fact hsource hfact
  exact (faithful.2 index source fact hsource hfact).1

/-- Constructor uniqueness is consumed, not checked again by either projection.
No Rust constructibility or constructor-algorithm proof is implied. -/
theorem solve_uniqueness_preserved (model : SolveModel)
    (issued : (model.variable_catalog.entries.val.map
      (fun entry => entry.source.source_occurrence)).Nodup) :
    ∃ facts, (do
      let view ← SolveModel.variable_refinement model
      project_solve_variables view) = .ok facts ∧
      (facts.val.map SolveVariableFact.occurrence).Nodup := by
  obtain ⟨facts, runs, occurrences⟩ := spec_imp_exists (solve_occurrences_spec model)
  exact ⟨facts, runs, occurrences ▸ issued⟩

/-- Nonempty source-bound projection with two distinct, non-ordinal identities.
As in the predecessor fixtures, this witnesses the translated domain only. -/
theorem dae_nonempty_witness :
    ∃ facts, project_dae_variables RumocaDaeFactContract.twoEntryView = .ok facts ∧
      facts.val.length = 2 ∧ (facts.val.map DaeVariableFact.occurrence).Nodup := by
  obtain ⟨facts, runs, occurrences⟩ :=
    spec_imp_exists (dae_occurrences_spec RumocaDaeFactContract.twoEntryView)
  refine ⟨facts, runs, ?_, ?_⟩
  · have lengths := congrArg List.length occurrences
    simpa [RumocaDaeFactContract.twoEntryView] using lengths
  · rw [occurrences]
    simp [RumocaDaeFactContract.twoEntryView, RumocaDaeFactContract.firstEntry,
      RumocaDaeFactContract.secondEntry]

theorem solve_nonempty_witness :
    ∃ facts, (do
      let view ← SolveModel.variable_refinement RumocaVariableViewContract.twoEntryModel
      project_solve_variables view) = .ok facts ∧
      facts.val.length = 2 ∧ (facts.val.map SolveVariableFact.occurrence).Nodup := by
  obtain ⟨facts, runs, occurrences⟩ :=
    spec_imp_exists (solve_occurrences_spec RumocaVariableViewContract.twoEntryModel)
  refine ⟨facts, runs, ?_, ?_⟩
  · have lengths := congrArg List.length occurrences
    simpa [RumocaVariableViewContract.twoEntryModel] using lengths
  · rw [occurrences]
    simp [RumocaVariableViewContract.twoEntryModel]

/-- info: 'RumocaOccurrenceContract.mapped_fields_eq' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms mapped_fields_eq

/-- info: 'RumocaOccurrenceContract.dae_occurrences_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dae_occurrences_spec

/-- info: 'RumocaOccurrenceContract.dae_uniqueness_preserved' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dae_uniqueness_preserved

/-- info: 'RumocaOccurrenceContract.solve_occurrences_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms solve_occurrences_spec

/-- info: 'RumocaOccurrenceContract.solve_uniqueness_preserved' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms solve_uniqueness_preserved

/-- info: 'RumocaOccurrenceContract.dae_nonempty_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms dae_nonempty_witness

/-- info: 'RumocaOccurrenceContract.solve_nonempty_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms solve_nonempty_witness

end RumocaOccurrenceContract
