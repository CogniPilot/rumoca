import RumocaVariableViewContract

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve
open rumoca_ir_solve.model rumoca_ir_solve.variable_catalog
open variable_catalog_refinement

namespace RumocaSolveFactContract

set_option autoImplicit false
set_option maxHeartbeats 1000000

/-- Read original closed-view storage, not a second projection or its getters. -/
def ViewEntryMatches (source : SolveVariableRefinementEntry) (fact : SolveVariableFact) : Prop :=
  fact.occurrence = source.source_occurrence ∧
  fact.fixed = source.fixed ∧
  fact.state_initialization = source.state_initialization ∧
  fact.variability = source.variability ∧
  fact.tunable = source.tunable ∧
  fact.causality = source.causality ∧
  fact.dimensions.val = source.dimensions.val ∧
  fact.role = source.role ∧
  fact.value_kind = source.value_kind ∧
  fact.storage = source.storage

def ViewFactsMatch (entries : List SolveVariableRefinementEntry)
    (facts : List SolveVariableFact) : Prop :=
  facts.length = entries.length ∧
  ∀ (index : Nat) source fact, entries[index]? = some source →
    facts[index]? = some fact → ViewEntryMatches source fact

/-- The composition's destination is the original catalog, including exact
dimension contents and propositional storage equality (F64 storage bits). -/
def CatalogEntryMatches (source : SolveVariableCatalogEntry) (fact : SolveVariableFact) : Prop :=
  fact.occurrence = source.source.source_occurrence ∧
  fact.fixed = source.attributes.fixed ∧
  fact.state_initialization = source.state_initialization ∧
  fact.variability = source.attributes.variability ∧
  fact.tunable = source.attributes.tunable ∧
  fact.causality = source.attributes.causality ∧
  fact.dimensions.val = source.source.dimensions.val ∧
  fact.role = source.declaration.role ∧
  fact.value_kind = source.declaration.value_kind ∧
  fact.storage = source.storage

def CatalogFactsMatch (entries : List SolveVariableCatalogEntry)
    (facts : List SolveVariableFact) : Prop :=
  facts.length = entries.length ∧
  ∀ (index : Nat) source fact, entries[index]? = some source →
    facts[index]? = some fact → CatalogEntryMatches source fact

theorem projection_loop_spec (entries : Slice SolveVariableRefinementEntry) :
    project_solve_variables_loop entries (alloc.vec.Vec.new _) 0#usize
      ⦃ facts => ViewFactsMatch entries.val facts.val ⦄ := by
  unfold project_solve_variables_loop
  apply loop.spec_decr_nat
    (fun (_, ordinal) => entries.val.length - ordinal.val)
    (fun (facts, ordinal) => facts.val.length = ordinal.val ∧
      ordinal.val ≤ entries.val.length ∧
      ∀ j, j < ordinal.val → ∀ source fact,
        entries.val[j]? = some source → facts.val[j]? = some fact →
          ViewEntryMatches source fact)
  · rintro ⟨facts, ordinal⟩ ⟨prefixLength, bound, matchedPrefix⟩
    dsimp
    unfold project_solve_variables_loop.body
    dsimp only
    split
    · step as ⟨entry, hentry⟩
      simp only [SolveVariableRefinementEntry.impl.source_occurrence,
        SolveVariableRefinementEntry.impl.fixed,
        SolveVariableRefinementEntry.impl.state_initialization,
        SolveVariableRefinementEntry.impl.variability,
        SolveVariableRefinementEntry.impl.tunable,
        SolveVariableRefinementEntry.impl.causality,
        SolveVariableRefinementEntry.impl.dimensions,
        alloc.boxed.Box.deref, bind_tc_ok]
      step with alloc.slice.Slice.to_vec_spec
        (cloneInst := core.clone.CloneU32) (s := entry.dimensions)
        (h := by intros; rfl) as ⟨dimensions, copied⟩
      simp only [SolveVariableRefinementEntry.impl.role,
        SolveVariableRefinementEntry.impl.value_kind,
        SolveVariableRefinementEntry.impl.storage, bind_tc_ok]
      step as ⟨updated, hupdated⟩
      step as ⟨next, hnext⟩
      refine ⟨?_, ?_, ?_, ?_⟩
      · simp [hupdated, prefixLength, hnext]
      · scalar_tac
      · intro j hj source fact hsource hfact
        rw [hupdated] at hfact
        by_cases same : j = ordinal.val
        · subst j
          simp [prefixLength] at hfact
          subst fact
          have original : source = entry := by grind
          subst source
          simp_all [ViewEntryMatches, alloc.vec.Vec.val]
        · have earlier : j < ordinal.val := by scalar_tac
          apply matchedPrefix j earlier source fact hsource
          simpa [List.getElem?_append, prefixLength, earlier] using hfact
      · scalar_tac
    · simp only [spec_ok]
      constructor
      · scalar_tac
      · intro j source fact hsource hfact
        have inRange : j < entries.val.length := by grind
        apply matchedPrefix j (by scalar_tac) source fact hsource hfact
  · simp

theorem view_projection_spec (view : SolveVariableRefinementView) :
    project_solve_variables view
      ⦃ facts => ViewFactsMatch view.entries.val facts.val ⦄ := by
  unfold project_solve_variables
  simp only [SolveVariableRefinementView.impl.entries, alloc.boxed.Box.deref,
    alloc.vec.Vec.with_capacity, bind_tc_ok]
  exact projection_loop_spec view.entries

theorem matching_entries_compose (source : SolveVariableCatalogEntry)
    (entry : SolveVariableRefinementEntry) (fact : SolveVariableFact)
    (first : RumocaVariableViewContract.EntryMatches source entry)
    (second : ViewEntryMatches entry fact) : CatalogEntryMatches source fact := by
  simp_all [RumocaVariableViewContract.EntryMatches, ViewEntryMatches, CatalogEntryMatches]

theorem matching_catalogs_compose (catalog : List SolveVariableCatalogEntry)
    (view : List SolveVariableRefinementEntry) (facts : List SolveVariableFact)
    (first : RumocaVariableViewContract.CatalogMatches catalog view)
    (second : ViewFactsMatch view facts) : CatalogFactsMatch catalog facts := by
  rcases first with ⟨viewLength, viewMatches⟩
  rcases second with ⟨factLength, factMatches⟩
  constructor
  · omega
  · intro index source fact hsource hfact
    cases middle : view[index]? with
    | none =>
      have present : index < facts.length := by grind
      have absent : view.length ≤ index := by grind
      omega
    | some entry =>
      exact matching_entries_compose source entry fact
        (viewMatches index source entry hsource middle)
        (factMatches index entry fact middle hfact)

/-- Model-only statement: both intermediates are returned by actual generated
functions. No supplied view, supplied facts, or successful-projection premise. -/
theorem root_facts_spec (model : SolveModel) :
    (do
      let view ← SolveModel.variable_refinement model
      project_solve_variables view)
      ⦃ facts => CatalogFactsMatch model.variable_catalog.entries.val facts.val ⦄ := by
  step with RumocaVariableViewContract.root_projection_spec as ⟨view, viewMatches⟩
  step with view_projection_spec as ⟨facts, factMatches⟩
  exact matching_catalogs_compose _ _ _ viewMatches factMatches

theorem root_facts_total_and_faithful (model : SolveModel) :
    ∃ facts, (do
      let view ← SolveModel.variable_refinement model
      project_solve_variables view) = .ok facts ∧
      CatalogFactsMatch model.variable_catalog.entries.val facts.val := by
  exact spec_imp_exists (root_facts_spec model)

/-- The existing two-entry translated-domain witness cannot decay to empty.
Production Rust constructibility is established separately by the Rust fixture. -/
theorem nonempty_root_witness :
    ∃ facts, (do
      let view ← SolveModel.variable_refinement RumocaVariableViewContract.twoEntryModel
      project_solve_variables view) = .ok facts ∧
      CatalogFactsMatch RumocaVariableViewContract.twoEntryModel.variable_catalog.entries.val
        facts.val ∧ facts.val.length = 2 := by
  obtain ⟨facts, runs, faithful⟩ :=
    root_facts_total_and_faithful RumocaVariableViewContract.twoEntryModel
  exact ⟨facts, runs, faithful,
    by simpa [RumocaVariableViewContract.twoEntryModel] using faithful.1⟩

/-- info: 'RumocaSolveFactContract.projection_loop_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_loop_spec

/-- info: 'RumocaSolveFactContract.view_projection_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms view_projection_spec

/-- info: 'RumocaSolveFactContract.root_facts_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms root_facts_spec

/-- info: 'RumocaSolveFactContract.root_facts_total_and_faithful' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms root_facts_total_and_faithful

/-- info: 'RumocaSolveFactContract.nonempty_root_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms nonempty_root_witness

end RumocaSolveFactContract
