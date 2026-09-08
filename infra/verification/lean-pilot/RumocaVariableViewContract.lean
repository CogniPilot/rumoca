import RumocaPhaseSolve.Funs
import RumocaRootValueFixture

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve
open rumoca_ir_solve.model rumoca_ir_solve.variable_catalog

namespace RumocaVariableViewContract

set_option autoImplicit false
set_option maxHeartbeats 1000000

/-- The ten admitted facts, read from original catalog storage, not getters or
another projection. Dimension-list equality includes rank, order and extents. -/
def EntryMatches (source : SolveVariableCatalogEntry)
    (fact : SolveVariableRefinementEntry) : Prop :=
  fact.source_occurrence = source.source.source_occurrence ∧
  fact.fixed = source.attributes.fixed ∧
  fact.state_initialization = source.state_initialization ∧
  fact.variability = source.attributes.variability ∧
  fact.tunable = source.attributes.tunable ∧
  fact.causality = source.attributes.causality ∧
  fact.dimensions.val = source.source.dimensions.val ∧
  fact.role = source.declaration.role ∧
  fact.value_kind = source.declaration.value_kind ∧
  fact.storage = source.storage

/-- Equal length and same-index correspondence preserve every admitted fact
occurrence. Both clauses are conclusions about the returned view. -/
def CatalogMatches (sources : List SolveVariableCatalogEntry)
    (facts : List SolveVariableRefinementEntry) : Prop :=
  facts.length = sources.length ∧
  ∀ (index : Nat) source fact, sources[index]? = some source →
    facts[index]? = some fact → EntryMatches source fact

theorem projection_loop_spec (entries : Slice SolveVariableCatalogEntry) :
    SolveVariableRefinementView.new_loop entries (alloc.vec.Vec.new _) 0#usize
      ⦃ projected => CatalogMatches entries.val projected.val ⦄ := by
  unfold SolveVariableRefinementView.new_loop
  apply loop.spec_decr_nat
    (fun (_, index) => entries.val.length - index.val)
    (fun (projected, index) => projected.val.length = index.val ∧
      index.val ≤ entries.val.length ∧
      ∀ j, j < index.val → ∀ source fact,
        entries.val[j]? = some source → projected.val[j]? = some fact →
          EntryMatches source fact)
  · rintro ⟨projected, index⟩ ⟨prefixLength, bound, matchedPrefix⟩
    dsimp
    unfold SolveVariableRefinementView.new_loop.body
    dsimp only
    split
    · step as ⟨entry, hentry⟩
      simp only [SolveVariableCatalogEntry.source_occurrence,
        SolveVariableCatalogEntry.fixed,
        SolveVariableCatalogEntry.impl.state_initialization,
        SolveVariableCatalogEntry.variability, SolveVariableCatalogEntry.is_tunable,
        SolveVariableCatalogEntry.causality, SolveVariableCatalogEntry.dimensions,
        alloc.boxed.Box.deref, bind_tc_ok]
      step with alloc.slice.Slice.to_vec_spec
        (cloneInst := core.clone.CloneU32) (s := entry.source.dimensions)
        (h := by intros; rfl) as ⟨dimensions, copied⟩
      simp only [alloc.vec.FromBoxSliceVec.from, SolveVariableCatalogEntry.role,
        SolveVariableDeclaration.impl.role, SolveVariableCatalogEntry.value_kind,
        SolveVariableDeclaration.impl.value_kind,
        SolveVariableCatalogEntry.impl.storage, bind_tc_ok]
      step as ⟨updated, hupdated⟩
      step as ⟨next, hnext⟩
      refine ⟨?_, ?_, ?_, ?_⟩
      · simp [hupdated, prefixLength, hnext]
      · scalar_tac
      · intro j hj source fact hsource hfact
        rw [hupdated] at hfact
        by_cases same : j = index.val
        · subst j
          simp [prefixLength] at hfact
          subst fact
          have original : source = entry := by grind
          subst source
          simp_all [EntryMatches, alloc.vec.Vec.val]
        · have earlier : j < index.val := by scalar_tac
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

theorem constructor_spec (entries : Slice SolveVariableCatalogEntry) :
    SolveVariableRefinementView.new entries
      ⦃ view => CatalogMatches entries.val view.entries.val ⦄ := by
  unfold SolveVariableRefinementView.new
  simp only [alloc.vec.Vec.with_capacity]
  step with projection_loop_spec as ⟨projected, faithful⟩
  simpa [core.convert.IntoFrom.into, core.convert.FromBoxSliceVec,
    alloc.vec.FromBoxSliceVec.from, bind_tc_ok, alloc.vec.Vec.val] using faithful

theorem root_projection_spec (model : SolveModel) :
    SolveModel.variable_refinement model
      ⦃ view => CatalogMatches model.variable_catalog.entries.val view.entries.val ⦄ := by
  unfold SolveModel.variable_refinement
  simp only [SolveVariableCatalog.impl.entries, alloc.boxed.Box.deref, bind_tc_ok]
  exact constructor_spec model.variable_catalog.entries

theorem root_projection_total_and_faithful (model : SolveModel) :
    ∃ view, SolveModel.variable_refinement model = .ok view ∧
      CatalogMatches model.variable_catalog.entries.val view.entries.val := by
  exact spec_imp_exists (root_projection_spec model)

/-- A nonempty input in the translated domain, not a claim of Rust root
constructibility. The production Rust fixture exercises that separate boundary. -/
def twoEntryModel : SolveModel :=
  let base := RumocaRootValueFixture.base_rumoca_ir_solve_variable_catalog_SolveVariableCatalogEntry
  let first := { base with source := { base.source with
    source_occurrence := ⟨⟨41#u32⟩⟩
    dimensions := Slice.from [1#u32] (by scalar_tac) } }
  let second := { base with source := { base.source with
    source_occurrence := ⟨⟨17#u32⟩⟩
    dimensions := Slice.from [1#u32, 1#u32] (by scalar_tac) } }
  { RumocaRootValueFixture.baseModel with variable_catalog :=
      ⟨Slice.from [first, second] (by scalar_tac)⟩ }

theorem nonempty_root_witness :
    ∃ view, SolveModel.variable_refinement twoEntryModel = .ok view ∧
      CatalogMatches twoEntryModel.variable_catalog.entries.val view.entries.val ∧
      view.entries.val.length = 2 := by
  obtain ⟨view, runs, faithful⟩ := root_projection_total_and_faithful twoEntryModel
  exact ⟨view, runs, faithful, by simpa [twoEntryModel] using faithful.1⟩

/-- info: 'RumocaVariableViewContract.projection_loop_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_loop_spec

/-- info: 'RumocaVariableViewContract.constructor_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms constructor_spec

/-- info: 'RumocaVariableViewContract.root_projection_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms root_projection_spec

/-- info: 'RumocaVariableViewContract.root_projection_total_and_faithful' depends on axioms: [propext,
 Classical.choice,
 Quot.sound] -/
#guard_msgs in
#print axioms root_projection_total_and_faithful

/-- info: 'RumocaVariableViewContract.nonempty_root_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms nonempty_root_witness

end RumocaVariableViewContract
