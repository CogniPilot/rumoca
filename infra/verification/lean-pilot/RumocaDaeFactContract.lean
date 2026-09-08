import RumocaPhaseSolve

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve rumoca_ir_dae.model.view
open variable_catalog_refinement

namespace RumocaDaeFactContract

set_option autoImplicit false
set_option maxHeartbeats 1000000

/-- Original closed-view storage, not another projection or its getters.
Stored scalar-count fidelity does not prove upstream dimension arithmetic. -/
def EntryMatches (source : DaeVariableRefinementEntry) (fact : DaeVariableFact) : Prop :=
  fact.occurrence = source.source_occurrence ∧
  fact.role = source.role ∧
  fact.fixed = source.fixed ∧
  fact.variability = source.variability ∧
  fact.is_tunable = source.is_tunable ∧
  fact.causality = source.causality ∧
  fact.scalar_type = source.scalar_type ∧
  fact.dimensions.val = source.dimensions.val ∧
  fact.scalar_count = source.scalar_count

def FactsMatch (entries : List DaeVariableRefinementEntry)
    (facts : List DaeVariableFact) : Prop :=
  facts.length = entries.length ∧
  ∀ (index : Nat) source fact, entries[index]? = some source →
    facts[index]? = some fact → EntryMatches source fact

theorem projection_loop_spec (entries : Slice DaeVariableRefinementEntry) :
    project_dae_variables_loop entries (alloc.vec.Vec.new _) 0#usize
      ⦃ facts => FactsMatch entries.val facts.val ⦄ := by
  unfold project_dae_variables_loop
  apply loop.spec_decr_nat
    (fun (_, ordinal) => entries.val.length - ordinal.val)
    (fun (facts, ordinal) => facts.val.length = ordinal.val ∧
      ordinal.val ≤ entries.val.length ∧
      ∀ j, j < ordinal.val → ∀ source fact,
        entries.val[j]? = some source → facts.val[j]? = some fact →
          EntryMatches source fact)
  · rintro ⟨facts, ordinal⟩ ⟨prefixLength, bound, matchedPrefix⟩
    dsimp
    unfold project_dae_variables_loop.body
    dsimp only
    split
    · step as ⟨entry, hentry⟩
      simp only [DaeVariableRefinementEntry.impl.source_occurrence,
        DaeVariableRefinementEntry.impl.role,
        DaeVariableRefinementEntry.impl.fixed,
        DaeVariableRefinementEntry.impl.variability,
        DaeVariableRefinementEntry.impl.is_tunable,
        DaeVariableRefinementEntry.impl.causality,
        DaeVariableRefinementEntry.impl.scalar_type,
        DaeVariableRefinementEntry.impl.dimensions,
        alloc.boxed.Box.deref, bind_tc_ok]
      step with alloc.slice.Slice.to_vec_spec
        (cloneInst := core.clone.CloneU32) (s := entry.dimensions)
        (h := by intros; rfl) as ⟨dimensions, copied⟩
      simp only [DaeVariableRefinementEntry.impl.scalar_count, bind_tc_ok]
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
          simp_all [EntryMatches, alloc.vec.Vec.val]
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

theorem view_projection_spec (view : DaeVariableRefinementView) :
    project_dae_variables view
      ⦃ facts => FactsMatch view.entries.val facts.val ⦄ := by
  unfold project_dae_variables
  simp only [DaeVariableRefinementView.impl.entries, alloc.boxed.Box.deref,
    alloc.vec.Vec.with_capacity, bind_tc_ok]
  exact projection_loop_spec view.entries

theorem projection_total_and_faithful (view : DaeVariableRefinementView) :
    ∃ facts, project_dae_variables view = .ok facts ∧
      FactsMatch view.entries.val facts.val := by
  exact spec_imp_exists (view_projection_spec view)

def firstEntry : DaeVariableRefinementEntry := {
  source_occurrence := { value := { value := 41#u32 } }
  role := .Parameter
  fixed := .Fixed
  variability := .Parameter
  is_tunable := false
  causality := .Parameter
  scalar_type := .Real
  dimensions := Slice.from [2#u32] (by scalar_tac)
  scalar_count := 2#usize
}

def secondEntry : DaeVariableRefinementEntry := {
  source_occurrence := { value := { value := 17#u32 } }
  role := .Constant
  fixed := .Free
  variability := .Constant
  is_tunable := false
  causality := .Local
  scalar_type := .Integer
  dimensions := Slice.from [3#u32, 1#u32] (by scalar_tac)
  scalar_count := 3#usize
}

/-- A nonempty translated-domain witness. Only the separate Rust fixture
establishes production constructibility; consistent extents are not that proof. -/
def twoEntryView : DaeVariableRefinementView :=
  ⟨Slice.from [firstEntry, secondEntry] (by scalar_tac)⟩

theorem nonempty_view_witness :
    ∃ facts, project_dae_variables twoEntryView = .ok facts ∧
      FactsMatch twoEntryView.entries.val facts.val ∧ facts.val.length = 2 := by
  obtain ⟨facts, runs, faithful⟩ := projection_total_and_faithful twoEntryView
  exact ⟨facts, runs, faithful, by simpa [twoEntryView] using faithful.1⟩

theorem empty_view_witness :
    ∃ facts, project_dae_variables ⟨Slice.from [] (by scalar_tac)⟩ = .ok facts ∧
      facts.val = [] := by
  obtain ⟨facts, runs, faithful⟩ :=
    projection_total_and_faithful ⟨Slice.from [] (by scalar_tac)⟩
  exact ⟨facts, runs, List.length_eq_zero_iff.mp (by simpa using faithful.1)⟩

/-- info: 'RumocaDaeFactContract.projection_loop_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_loop_spec

/-- info: 'RumocaDaeFactContract.view_projection_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms view_projection_spec

/-- info: 'RumocaDaeFactContract.projection_total_and_faithful' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms projection_total_and_faithful

/-- info: 'RumocaDaeFactContract.nonempty_view_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms nonempty_view_witness

/-- info: 'RumocaDaeFactContract.empty_view_witness' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms empty_view_witness

/-- The actual optional-ID projection preserves presence and every raw identity.
This does not assert that arbitrary raw IDs are constructible arena members. -/
theorem optional_expression_id_preserves_identity (raw : Option U32) :
    (do
      let result ← optional_expression_id raw
      pure (result.map (fun id => id.raw))) = Result.ok raw := by
  cases raw <;> rfl

/-- info: 'RumocaDaeFactContract.optional_expression_id_preserves_identity' does not depend on any axioms -/
#guard_msgs in
#print axioms optional_expression_id_preserves_identity

end RumocaDaeFactContract
