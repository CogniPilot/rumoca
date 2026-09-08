import RumocaVariableEqualityContract
import RumocaVariableClassificationContract
import RumocaDimensionCheckContract
import RumocaCompleteFactContract

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve variable_catalog_refinement rumoca_core.ir_primitives
open rumoca_ir_dae.model rumoca_ir_solve.model rumoca_ir_solve.variable_catalog
open RumocaVariableEqualityContract RumocaVariableClassificationContract
open RumocaDimensionCheckContract
open RumocaCompleteFactContract (outcomeMatches)

namespace RumocaVariableCheckContract

set_option autoImplicit false
set_option maxHeartbeats 1000000

/-- Role correspondence only; input-causality overriding is outside this relation. -/
def RoleMatches (source : VariableRole) (target : SolveVariableStorageRole) : Prop :=
  match target with
  | .Parameter => source = .Parameter
  | .Constant => source = .Constant
  | .ExternalInput => source = .Input
  | .State => source = .State
  | .Algebraic => source = .Algebraic
  | .Output => source = .Output
  | .DiscreteReal => source = .DiscreteReal
  | .DiscreteValue => source = .DiscreteValue

/-- Agreement of supplied fields, not well-formedness or provenance of either fact. -/
def FieldsMatch (source : DaeVariableFact) (target : SolveVariableFact)
    (storage : SolveVariableStorageRun) : Prop :=
  RoleMatches source.role target.role ∧
  source.fixed = target.fixed ∧
  InitializationMatches source.role source.fixed target.state_initialization ∧
  VariabilityMatches source.variability source.is_tunable target.variability ∧
  source.is_tunable = target.tunable ∧
  CausalityMatches source.causality target.causality ∧
  KindMatches source.scalar_type target.value_kind ∧
  SameShape source.dimensions.val target.dimensions.val ∧
  source.scalar_count = target.storage.scalar_count ∧
  target.storage = storage

theorem role_spec (source : DaeVariableFact) (nonInput : source.causality ≠ .Input) :
    expected_role source ⦃ RoleMatches source.role ⦄ := by
  simp only [expected_role, dae_causality_eq, bind_tc_ok, nonInput, decide_false,
    Bool.false_eq_true, ↓reduceIte]
  cases role : source.role <;> simp [RoleMatches]

theorem role_accepts_iff (source : DaeVariableFact) (nonInput : source.causality ≠ .Input)
    (target : SolveVariableStorageRole) :
    expected_role source = .ok target ↔ RoleMatches source.role target := by
  simp only [expected_role, dae_causality_eq, bind_tc_ok, nonInput, decide_false,
    Bool.false_eq_true, ↓reduceIte]
  cases role : source.role <;> cases target <;> simp [RoleMatches]

private theorem initialization_accepts_iff (source : DaeVariableFact)
    (target : SolveStateInitialization) :
    expected_state_initialization source = .ok target ↔
      InitializationMatches source.role source.fixed target := by
  cases role : source.role <;> cases fixed : source.fixed <;> cases target <;>
    simp [expected_state_initialization, InitializationMatches, role, fixed]

private theorem variability_accepts_iff (source : DaeVariableFact)
    (target : SolveVariableVariability) :
    expected_variability source = .ok target ↔
      VariabilityMatches source.variability source.is_tunable target := by
  cases variability : source.variability <;> cases tunable : source.is_tunable <;>
    cases target <;>
    simp [expected_variability, VariabilityMatches, variability, tunable]

private theorem causality_accepts_iff (source : VariableCausality)
    (target : SolveVariableCausality) :
    expected_causality source = .ok target ↔ CausalityMatches source target := by
  cases source <;> cases target <;> simp [expected_causality, CausalityMatches]

/-- Totality and exact acceptance over supplied closed facts. The caller owns
occurrence association and derivation of the supplied expected storage. -/
theorem variable_spec (occurrence : SourceOccurrenceId) (source : DaeVariableFact)
    (target : SolveVariableFact) (storage : SolveVariableStorageRun)
    (nonInput : source.causality ≠ .Input) :
    check_variable occurrence source target storage
      ⦃ result => outcomeMatches result (FieldsMatch source target storage) ⦄ := by
  obtain ⟨role, roleRuns, _⟩ := spec_imp_exists (role_spec source nonInput)
  obtain ⟨initialization, initializationRuns, _⟩ := spec_imp_exists (initialization_spec source)
  obtain ⟨variability, variabilityRuns, _⟩ := spec_imp_exists (variability_spec source)
  obtain ⟨causality, causalityRuns, _⟩ := spec_imp_exists (causality_spec source.causality)
  obtain ⟨kind, kindRuns, kindCorrect⟩ := spec_imp_exists (kind_spec occurrence source.scalar_type)
  have roleMatches := role_accepts_iff source nonInput target.role
  have initializationMatches := initialization_accepts_iff source target.state_initialization
  have variabilityMatches := variability_accepts_iff source target.variability
  have causalityMatches := causality_accepts_iff source.causality target.causality
  have kindMatches := kind_accepts_iff occurrence source.scalar_type target.value_kind
  simp only [roleRuns, initializationRuns, variabilityRuns, causalityRuns, kindRuns,
    Result.ok.injEq] at roleMatches initializationMatches variabilityMatches causalityMatches kindMatches
  have fields : FieldsMatch source target storage ↔
      role = target.role ∧ source.fixed = target.fixed ∧
      initialization = target.state_initialization ∧ variability = target.variability ∧
      source.is_tunable = target.tunable ∧ causality = target.causality ∧
      kind = .Ok target.value_kind ∧ source.dimensions.val = target.dimensions.val ∧
      source.scalar_count = target.storage.scalar_count ∧ target.storage = storage := by
    rw [FieldsMatch, ← roleMatches, ← initializationMatches, ← variabilityMatches,
      ← causalityMatches, ← kindMatches, same_shape_iff_equal]
  unfold check_variable
  simp only [roleRuns, initializationRuns, variabilityRuns, causalityRuns, kindRuns,
    dimension_gate_equation, core.cmp.PartialEq.ne.trait_default,
    core.cmp.PartialEq.ne.default,
    role_eq, fixity_eq, initialization_eq, variability_eq, causality_eq, kind_eq,
    storage_eq, bind_tc_ok]
  cases kind <;>
    simp only [core.result.Result.Insts.CoreOpsTry.branch,
      core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual,
      core.convert.FromSame.from, bind_tc_ok]
  all_goals
    simp only [fields]
    clear fields roleMatches initializationMatches variabilityMatches causalityMatches
      kindMatches roleRuns initializationRuns variabilityRuns causalityRuns kindRuns kindCorrect
    repeat' split
    all_goals
      simp_all only [outcomeMatches, spec_ok, decide_eq_true_eq, bne_iff_ne, bind_tc_ok]
      grind

theorem variables_total_and_faithful (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) (storage : SolveVariableStorageRun)
    (nonInput : source.causality ≠ .Input) :
    ∃ result, check_variable occurrence source target storage = .ok result ∧
      outcomeMatches result (FieldsMatch source target storage) :=
  spec_imp_exists (variable_spec occurrence source target storage nonInput)

theorem variables_accept_iff (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) (storage : SolveVariableStorageRun)
    (nonInput : source.causality ≠ .Input) :
    check_variable occurrence source target storage = .ok (.Ok ()) ↔
      FieldsMatch source target storage := by
  obtain ⟨result, runs, correct⟩ :=
    variables_total_and_faithful occurrence source target storage nonInput
  cases result with
  | Ok value => cases value; simp_all [outcomeMatches]
  | Err error => simp_all [outcomeMatches]

theorem variables_refuse_iff (occurrence : SourceOccurrenceId)
    (source : DaeVariableFact) (target : SolveVariableFact) (storage : SolveVariableStorageRun)
    (nonInput : source.causality ≠ .Input) :
    (∃ error, check_variable occurrence source target storage = .ok (.Err error)) ↔
      ¬ FieldsMatch source target storage := by
  obtain ⟨result, runs, correct⟩ :=
    variables_total_and_faithful occurrence source target storage nonInput
  cases result with
  | Ok value => cases value; simp_all [outcomeMatches]
  | Err error => simp_all [outcomeMatches]

/-- An entire family of supplied local-state facts is accepted, not just one
concrete test vector. This does not assert the facts are constructible IR roots. -/
theorem matching_local_state (occurrence : SourceOccurrenceId)
    (dimensions : alloc.vec.Vec U32) (count : Usize) :
    check_variable occurrence (sourceFact dimensions count) (targetFact dimensions count)
      (targetFact dimensions count).storage = .ok (.Ok ()) := by
  apply (variables_accept_iff _ _ _ _ (by simp [sourceFact])).mpr
  simp [FieldsMatch, sourceFact, targetFact, RoleMatches, InitializationMatches,
    VariabilityMatches, CausalityMatches, KindMatches, same_shape_iff_equal]

theorem wrong_storage_column_refused (occurrence : SourceOccurrenceId) :
    ∃ error, check_variable occurrence fixedState
      { (targetFact (alloc.vec.Vec.new U32) 1#usize) with
        storage := { (targetFact (alloc.vec.Vec.new U32) 1#usize).storage with base := .P 0#usize } }
      (targetFact (alloc.vec.Vec.new U32) 1#usize).storage = .ok (.Err error) := by
  apply (variables_refuse_iff _ _ _ _ (by simp [fixedState])).mpr
  simp [FieldsMatch, fixedState, targetFact]

/-- info: 'RumocaVariableCheckContract.variable_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms variable_spec
/-- info: 'RumocaVariableCheckContract.variables_total_and_faithful' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms variables_total_and_faithful
/-- info: 'RumocaVariableCheckContract.variables_accept_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms variables_accept_iff
/-- info: 'RumocaVariableCheckContract.variables_refuse_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms variables_refuse_iff
/-- info: 'RumocaVariableCheckContract.matching_local_state' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms matching_local_state
/-- info: 'RumocaVariableCheckContract.wrong_storage_column_refused' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms wrong_storage_column_refused
/-- info: 'RumocaVariableCheckContract.role_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms role_spec
/-- info: 'RumocaVariableCheckContract.role_accepts_iff' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms role_accepts_iff

end RumocaVariableCheckContract
