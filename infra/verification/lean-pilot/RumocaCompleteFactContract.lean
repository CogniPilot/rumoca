import RumocaFactContract

open Aeneas Aeneas.Std Aeneas.Std.WP
open rumoca_phase_solve.scalar_constant_derivative_refinement
open RumocaKernelPilot RumocaFactContract

namespace RumocaCompleteFactContract

set_option maxHeartbeats 1000000

/-- Successful checking means the positive relation; refusal means its negation.
The surrounding WP judgment separately guarantees termination without panic. -/
def outcomeMatches {T E : Type} (result : core.result.Result T E) (property : Prop) : Prop :=
  match result with
  | .Ok _ => property
  | .Err _ => ¬ property

@[step]
theorem require_spec (field : diagnostics.SolveMetadataField) (expected actual : Usize) :
    require_metadata field expected actual ⦃ result => outcomeMatches result (actual = expected) ⦄ := by
  unfold require_metadata
  split <;> simp [outcomeMatches] <;> scalar_tac

def eventPermitted (facts : SolveMetadataFacts) : Prop :=
  facts.event_root_memory_targets = 0#usize ∧
  facts.event_root_zero_domains = 0#usize ∧
  facts.event_root_refresh_roles = 0#usize ∧
  facts.event_condition_memories = 0#usize ∧
  facts.event_scheduled_roots = 0#usize ∧
  facts.event_scheduled_times = 0#usize ∧
  facts.event_dynamic_time_names = 0#usize ∧
  facts.event_actions = 0#usize ∧
  facts.event_has_terminal = 0#usize ∧
  facts.event_delay_targets = 0#usize ∧
  facts.event_delay_discrete_flags = 0#usize ∧
  facts.clock_schedules = 0#usize ∧
  facts.clock_activation_parameters = 0#usize

@[step]
theorem event_spec (facts : SolveMetadataFacts) :
    check_event_metadata facts ⦃ result => outcomeMatches result (eventPermitted facts) ⦄ := by
  unfold check_event_metadata
  repeat' (
    step as ⟨result, correct⟩
    cases result <;>
      simp_all only [outcomeMatches, eventPermitted,
        core.result.Result.Insts.CoreOpsTry.branch,
        core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual,
        core.convert.FromSame.from, bind_tc_ok, spec_ok,
        true_and, false_and, not_false_eq_true])

def discretePermitted (facts : SolveMetadataFacts) : Prop :=
  facts.discrete_update_targets = 0#usize ∧
  facts.discrete_event_iteration_runs = 0#usize ∧
  facts.discrete_runtime_assignment_targets = 0#usize ∧
  facts.discrete_runtime_assignment_roles = 0#usize ∧
  facts.discrete_post_commit_targets = 0#usize ∧
  facts.discrete_post_commit_runtime_rows = 0#usize ∧
  facts.discrete_row_roles = 0#usize ∧
  facts.discrete_pre_modes = 0#usize ∧
  facts.discrete_observation_refresh = 0#usize ∧
  facts.discrete_observation_refresh_reads_y = 0#usize ∧
  facts.discrete_integrator_history_effects = 0#usize ∧
  facts.discrete_clock_owners = 0#usize ∧
  facts.discrete_structured_updates = 0#usize ∧
  facts.discrete_guarded_assignments = 0#usize ∧
  facts.discrete_event_transactions = 0#usize ∧
  facts.discrete_clock_partition_order = 0#usize ∧
  facts.discrete_clock_intermediate_targets = 0#usize ∧
  facts.discrete_clock_intermediate_clocks = 0#usize

@[step]
theorem discrete_spec (facts : SolveMetadataFacts) :
    check_discrete_metadata facts ⦃ result => outcomeMatches result (discretePermitted facts) ⦄ := by
  unfold check_discrete_metadata
  repeat' (
    step as ⟨result, correct⟩
    cases result <;>
      simp_all only [outcomeMatches, discretePermitted,
        core.result.Result.Insts.CoreOpsTry.branch,
        core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual,
        core.convert.FromSame.from, bind_tc_ok, spec_ok,
        true_and, false_and, not_false_eq_true])

def generalPermitted (facts : SolveMetadataFacts) : Prop :=
  facts.pure_call_owners = 0#usize ∧
  facts.implicit_row_targets = 0#usize ∧
  facts.algebraic_projection_blocks = 0#usize ∧
  facts.manifold_projection_blocks = 0#usize ∧
  facts.initialization_projection_unknowns = 0#usize ∧
  facts.initialization_projection_blocks = 0#usize ∧
  facts.initialization_update_targets = 0#usize ∧
  facts.continuous_refresh_rows = 0#usize ∧
  facts.continuous_refresh_static_parameters = 0#usize ∧
  facts.mass_matrix = .Identity

@[step]
theorem general_spec (facts : SolveMetadataFacts) :
    check_general_metadata facts ⦃ result => outcomeMatches result (generalPermitted facts) ⦄ := by
  unfold check_general_metadata
  repeat' (
    step as ⟨result, correct⟩
    cases result <;>
      simp_all only [outcomeMatches, generalPermitted,
        core.result.Result.Insts.CoreOpsTry.branch,
        core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual,
        core.convert.FromSame.from, bind_tc_ok, spec_ok,
        true_and, false_and, not_false_eq_true])

  all_goals cases h : facts.mass_matrix <;> simp_all

def structuralPermitted (facts : SolveMetadataFacts) : Prop :=
  facts.structural_implicit = 0#usize ∧
  facts.structural_algebraic_projection_blocks = 0#usize ∧
  facts.structural_manifold = 0#usize ∧
  facts.structural_manifold_projection_blocks = 0#usize ∧
  facts.structural_derivative = .Empty 1#u32 1#u32 ∧
  facts.initialization_structural_residual = 0#usize ∧
  facts.initialization_structural_projection_blocks = 0#usize

@[step]
theorem structural_spec (facts : SolveMetadataFacts) :
    check_structural_metadata facts ⦃ result => outcomeMatches result (structuralPermitted facts) ⦄ := by
  cases h : facts.structural_derivative <;> unfold check_structural_metadata
  all_goals repeat' first
    | (step as ⟨result, correct⟩
       cases result <;>
         simp_all only [outcomeMatches, structuralPermitted,
           core.result.Result.Insts.CoreOpsTry.branch,
           core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual,
           core.convert.FromSame.from, bind_tc_ok, spec_ok,
           true_and, false_and, not_false_eq_true])
    | (split <;> simp_all)
  all_goals simp_all [show ((1#32)#uscalar : U32) = 1#u32 from rfl]

/-- Independent positive metadata target. Exhaustive construction makes an
added fact field a proof update, rather than a silently ignored input. -/
def canonicalMetadata : SolveMetadataFacts := {
  pure_call_owners := 0#usize
  implicit_row_targets := 0#usize
  algebraic_projection_blocks := 0#usize
  manifold_projection_blocks := 0#usize
  initialization_projection_unknowns := 0#usize
  initialization_projection_blocks := 0#usize
  initialization_update_targets := 0#usize
  continuous_refresh_rows := 0#usize
  continuous_refresh_static_parameters := 0#usize
  structural_implicit := 0#usize
  structural_algebraic_projection_blocks := 0#usize
  structural_manifold := 0#usize
  structural_manifold_projection_blocks := 0#usize
  initialization_structural_residual := 0#usize
  initialization_structural_projection_blocks := 0#usize
  discrete_update_targets := 0#usize
  discrete_event_iteration_runs := 0#usize
  discrete_runtime_assignment_targets := 0#usize
  discrete_runtime_assignment_roles := 0#usize
  discrete_post_commit_targets := 0#usize
  discrete_post_commit_runtime_rows := 0#usize
  discrete_row_roles := 0#usize
  discrete_pre_modes := 0#usize
  discrete_observation_refresh := 0#usize
  discrete_observation_refresh_reads_y := 0#usize
  discrete_integrator_history_effects := 0#usize
  discrete_clock_owners := 0#usize
  discrete_structured_updates := 0#usize
  discrete_guarded_assignments := 0#usize
  discrete_event_transactions := 0#usize
  discrete_clock_partition_order := 0#usize
  discrete_clock_intermediate_targets := 0#usize
  discrete_clock_intermediate_clocks := 0#usize
  event_root_memory_targets := 0#usize
  event_root_zero_domains := 0#usize
  event_root_refresh_roles := 0#usize
  event_condition_memories := 0#usize
  event_scheduled_roots := 0#usize
  event_scheduled_times := 0#usize
  event_dynamic_time_names := 0#usize
  event_actions := 0#usize
  event_has_terminal := 0#usize
  event_delay_targets := 0#usize
  event_delay_discrete_flags := 0#usize
  clock_schedules := 0#usize
  clock_activation_parameters := 0#usize
  mass_matrix := .Identity
  structural_derivative := .Empty 1#u32 1#u32
}

theorem metadata_exact (facts : SolveMetadataFacts) :
    generalPermitted facts ∧ structuralPermitted facts ∧
      discretePermitted facts ∧ eventPermitted facts ↔ facts = canonicalMetadata := by
  cases facts
  simp only [generalPermitted, structuralPermitted, discretePermitted, eventPermitted,
    canonicalMetadata, SolveMetadataFacts.mk.injEq]
  tauto

def canonicalOwners : Array Usize 28#usize :=
  Array.make 28#usize
    [0#usize, 0#usize, 0#usize, 1#usize, 0#usize, 0#usize, 0#usize,
     0#usize, 0#usize, 0#usize, 0#usize, 0#usize, 0#usize, 0#usize,
     0#usize, 0#usize, 0#usize, 0#usize, 0#usize, 0#usize, 0#usize,
     1#usize, 0#usize, 1#usize, 0#usize, 0#usize, 0#usize, 0#usize]

@[step]
theorem owner_spec (facts : SolveFacts) :
    check_solve_owner_census facts ⦃ result =>
      outcomeMatches result
        (facts.owners = canonicalOwners ∧ facts.metadata = canonicalMetadata) ⦄ := by
  unfold check_solve_owner_census
  step as ⟨first, hfirst⟩
  step as ⟨second, hsecond⟩
  step as ⟨expected, hexpected⟩
  have expected_exact : expected = canonicalOwners := by
    subst first second expected
    rfl
  rw [expected_exact]
  step with counts_spec as ⟨result, correct⟩
  cases result with
  | Ok value =>
    simp only [resultSpec] at correct
    have owners_exact : facts.owners = canonicalOwners :=
      Aeneas.Std.Array.ext _ _ correct
    repeat' (
      step as ⟨result, relation⟩
      cases result <;>
        simp_all only [outcomeMatches,
          core.result.Result.Insts.CoreOpsTry.branch,
          core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual,
          core.convert.FromSame.from, bind_tc_ok, spec_ok])
    all_goals simp_all [← metadata_exact]
  | Err error =>
    simp only [resultSpec] at correct
    step as ⟨label, hlabel⟩
    simp only [outcomeMatches]
    intro accepted
    have same : facts.owners.val = canonicalOwners.val := congrArg Array.val accepted.1
    have mismatch := correct.2.2.2.1
    rw [same] at correct
    grind

/-- Reuse existing acceptance and totality results in the framework's WP form. -/
theorem outcome_spec {E : Type} (call : Result (core.result.Result Unit E))
    (property : Prop)
    (total : ∃ result, call = .ok result)
    (accepted : call = .ok (.Ok ()) ↔ property) :
    call ⦃ result => outcomeMatches result property ⦄ := by
  obtain ⟨result, runs⟩ := total
  cases result with
  | Ok value => cases value; simp_all [outcomeMatches]
  | Err error => simp_all [outcomeMatches]

@[step]
theorem kernel_spec (facts : ScalarBlockFacts 2#usize) (bits : U64) :
    check_kernel facts bits ⦃ result => outcomeMatches result (permitted facts bits) ⦄ :=
  outcome_spec _ _ (checker_total facts bits) (accepted_iff_permitted facts bits)

@[step]
theorem jacobian_spec (facts : ScalarBlockFacts 3#usize) (bits : U64) :
    check_full_jacobian facts bits ⦃ result =>
      outcomeMatches result (jacobianPermitted facts bits) ⦄ :=
  outcome_spec _ _ (jacobian_total facts bits) (jacobian_accepted_iff facts bits)

@[step]
theorem visible_spec (facts : ScalarBlockFacts 2#usize) :
    check_visible_rows facts ⦃ result => outcomeMatches result (visiblePermitted facts) ⦄ :=
  outcome_spec _ _ (visible_total facts) (visible_accepted_iff facts)

/-- The complete expected facts, not a restatement of the checker guard chain. -/
def canonicalFacts (profile : AdmittedScalarConstantDerivativeProfile) : SolveFacts :=
  let ⟨start, derivative⟩ := profile
  {
    catalog_start_present := true
    catalog_start_width := 1#usize
    catalog_start_bits := start
    initial_y_width := 1#usize
    initial_y_bits := start
    kernel := canonical derivative
    full_jacobian := .Exact (Array.make 3#usize
      [.Constant 0#u32 derivative, .Constant 1#u32 0#u64, .StoreOutput 1#u32])
    visible_rows := .Exact (Array.make 2#usize
      [.LoadY 0#u32 0#usize, .StoreOutput 0#u32])
    owners := canonicalOwners
    metadata := canonicalMetadata
  }

theorem facts_exact (profile : AdmittedScalarConstantDerivativeProfile) (facts : SolveFacts) :
    (facts.catalog_start_present = true ∧
      facts.catalog_start_width = 1#usize ∧ facts.catalog_start_bits = profile.start_bits ∧
      facts.initial_y_width = 1#usize ∧ facts.initial_y_bits = profile.start_bits ∧
      permitted facts.kernel profile.derivative_constant_bits ∧
      jacobianPermitted facts.full_jacobian profile.derivative_constant_bits ∧
      visiblePermitted facts.visible_rows ∧
      facts.owners = canonicalOwners ∧ facts.metadata = canonicalMetadata) ↔
      facts = canonicalFacts profile := by
  cases profile
  rcases facts with ⟨present, width, start, ywidth, ystart, kernel, jacobian, visible, owners, metadata⟩
  cases kernel <;> cases jacobian <;> cases visible
  all_goals simp [canonicalFacts, canonical, permitted, jacobianPermitted, visiblePermitted,
    SolveFacts.mk.injEq, Aeneas.Std.Array.eq_iff]

theorem complete_spec (profile : AdmittedScalarConstantDerivativeProfile) (facts : SolveFacts) :
    check_scalar_constant_derivative_refinement profile facts ⦃ result =>
      outcomeMatches result (facts = canonicalFacts profile) ⦄ := by
  simp only [← facts_exact]
  unfold check_scalar_constant_derivative_refinement
  split_ifs <;> simp_all [outcomeMatches]
  all_goals repeat' (
    step as ⟨result, relation⟩
    cases result <;>
      simp_all only [outcomeMatches,
        core.result.Result.Insts.CoreOpsTry.branch,
        core.result.Result.Insts.CoreOpsTryTraitFromResidualResultInfallible.from_residual,
        core.convert.FromSame.from, bind_tc_ok, spec_ok])
  all_goals scalar_tac

/-- The contract of the actual extracted comparison. It assumes
no projection, helper acceptance, execution or source representability. -/
def WholeContract : Prop :=
  ∀ (profile : AdmittedScalarConstantDerivativeProfile) (facts : SolveFacts),
    (check_scalar_constant_derivative_refinement profile facts =
      .ok (.Ok ⟨()⟩) ↔ facts = canonicalFacts profile) ∧
    (∃ result, check_scalar_constant_derivative_refinement profile facts = .ok result)

theorem whole_contract : WholeContract := by
  intro profile facts
  obtain ⟨result, runs, correct⟩ := spec_imp_exists (complete_spec profile facts)
  cases result with
  | Ok receipt =>
    cases receipt with
    | mk value => cases value; simp_all [outcomeMatches]
  | Err error => simp_all [outcomeMatches]

/-- Complete comparison acceptance composes the three existing bit-transfer
semantics results. This does not assume that the actual IR was faithfully
projected, or that a production runtime implements this interpreter. -/
theorem complete_executes (profile : AdmittedScalarConstantDerivativeProfile) (facts : SolveFacts)
    (accepted : check_scalar_constant_derivative_refinement profile facts = .ok (.Ok ⟨()⟩))
    (inputs : Usize → Option U64) :
    facts.catalog_start_bits = profile.start_bits ∧
    facts.initial_y_bits = profile.start_bits ∧
    executeKernel inputs facts.kernel = some [profile.derivative_constant_bits] ∧
    (match facts.full_jacobian with
     | .UnsupportedShape => none
     | .Exact operations => execute inputs operations.val (fun _ => none)) = some [0#u64] ∧
    executeKernel inputs facts.visible_rows = (inputs 0#usize).map (fun bits => [bits]) := by
  have exact_facts := (whole_contract profile facts).1.mp accepted
  obtain ⟨_, _, start, _, ystart, kernel, jacobian, visible, _, _⟩ :=
    (facts_exact profile facts).mpr exact_facts
  exact ⟨start, ystart,
    RumocaKernelPilot.accepted_executes _ _
      ((accepted_iff_permitted _ _).mpr kernel) inputs,
    (by
      cases h : facts.full_jacobian with
      | UnsupportedShape => simp_all [jacobianPermitted]
      | Exact operations =>
        have accepted_jacobian := (jacobian_accepted_iff (.Exact operations)
          profile.derivative_constant_bits).mpr (by simpa only [h] using jacobian)
        exact jacobian_executes _ _ accepted_jacobian inputs),
    visible_executes _ ((visible_accepted_iff _).mpr visible) inputs⟩

theorem complete_canonical_accepted (profile : AdmittedScalarConstantDerivativeProfile) :
    check_scalar_constant_derivative_refinement profile (canonicalFacts profile) =
      .ok (.Ok ⟨()⟩) :=
  (whole_contract profile (canonicalFacts profile)).1.mpr rfl

/-- info: 'RumocaCompleteFactContract.complete_spec' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms complete_spec
/-- info: 'RumocaCompleteFactContract.whole_contract' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms whole_contract
/-- info: 'RumocaCompleteFactContract.complete_executes' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms complete_executes
/-- info: 'RumocaCompleteFactContract.complete_canonical_accepted' depends on axioms: [propext, Classical.choice, Quot.sound] -/
#guard_msgs in
#print axioms complete_canonical_accepted

end RumocaCompleteFactContract
