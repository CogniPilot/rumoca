import RumocaPhaseSolve

/-! # Root value fixture

One explicit `SolveModel` and the single-field variations built from it. This
module deliberately does **not** import `RumocaRootValueContract`, so a mutant
witness can reuse the same base without also importing a theorem that the mutant
is meant to invalidate.

Nothing here is a claim. A `SolveModel` value is a value of the generated type;
it is not evidence that any Modelica source, `lower_solve_model` call, or
production caller can produce such a root. The Rust fixtures carry reachability.

The `base_*` definitions were derived mechanically from the generated
`RumocaPhaseSolve/Types.lean`, giving each field a neutral value: empty vectors
and slices, `none`, zero scalars, and the smallest constructor of each inductive.
`NonZero` payloads carry `1` rather than `0` so the base stays inside the range a
real Rust value could occupy, since the model forgets that refinement. That is a
narrowing of one field, not a demonstration that the whole root satisfies every
Rust construction invariant; no such demonstration is offered here.

These values determine what the witnesses in `RumocaRootValueWitness` mean, so
they are part of what a reviewer must read, not scaffolding beneath it.
-/

open Aeneas Aeneas.Std
open rumoca_phase_solve
open rumoca_phase_solve.scalar_constant_derivative_refinement

namespace RumocaRootValueFixture

def base_rumoca_core_ir_primitives_Fixity : rumoca_core.ir_primitives.Fixity := rumoca_core.ir_primitives.Fixity.Fixed

def base_rumoca_core_ir_primitives_Span : rumoca_core.ir_primitives.Span :=
  {
    source := 0#u64,
    start := 0#usize,
    «end» := 0#usize
  }

def base_rumoca_core_matrix_multiply_RealMatrixMultiplySemantics : rumoca_core.matrix_multiply.RealMatrixMultiplySemantics := rumoca_core.matrix_multiply.RealMatrixMultiplySemantics.SeparateMulAddAscendingFirstProduct

def base_rumoca_ir_solve_ScalarProgramBlock : rumoca_ir_solve.ScalarProgramBlock :=
  {
    programs := alloc.vec.Vec.from [] (by scalar_tac),
    program_spans := alloc.vec.Vec.from [] (by scalar_tac),
    output_indices := alloc.vec.Vec.from [] (by scalar_tac),
    program_register_counts := Slice.from [] (by scalar_tac),
    program_output_sources := Slice.from [] (by scalar_tac),
    program_output_counts := Slice.from [] (by scalar_tac),
    stored_output_count := 0#usize,
    output_count := 0#usize
  }

def base_rumoca_ir_solve_layout_VarLayout : rumoca_ir_solve.layout.VarLayout :=
  {
    bindings := { entries := [], hasher := { k0 := 0#u64, k1 := 0#u64 } },
    shapes := { entries := [], hasher := { k0 := 0#u64, k1 := 0#u64 } },
    shape_spans := { entries := [], hasher := { k0 := 0#u64, k1 := 0#u64 } },
    shape_indexed_keys := { entries := [], hasher := { k0 := 0#u64, k1 := 0#u64 } },
    indexed_bindings := { entries := [], hasher := { k0 := 0#u64, k1 := 0#u64 } },
    y_scalars := 0#usize,
    p_scalars := 0#usize
  }

def base_rumoca_ir_solve_model_AlgebraicProjectionPlan : rumoca_ir_solve.model.AlgebraicProjectionPlan :=
  {
    blocks := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_refresh_PendingRefreshPlan : rumoca_ir_solve.refresh.PendingRefreshPlan :=
  {
    static_causal_sequence := { value := { value := 1#u64 } },
    dynamic_causal_sequence := { value := { value := 1#u64 } },
    simultaneous_plan := base_rumoca_ir_solve_model_AlgebraicProjectionPlan,
    simultaneous_block_indices := alloc.vec.Vec.from [] (by scalar_tac),
    value_projection_plan := base_rumoca_ir_solve_model_AlgebraicProjectionPlan,
    rows := alloc.vec.Vec.from [] (by scalar_tac),
    causal_seed_rows := Slice.from [] (by scalar_tac),
    static_causal_seed_rows := Slice.from [] (by scalar_tac),
    dynamic_causal_seed_rows := Slice.from [] (by scalar_tac),
    value_stages := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_refresh_IssuedRefreshPlan : rumoca_ir_solve.refresh.IssuedRefreshPlan :=
  {
    plan := base_rumoca_ir_solve_refresh_PendingRefreshPlan,
    causal_solution_certified := false
  }

def base_rumoca_ir_solve_refresh_RefreshRemainderRelation : rumoca_ir_solve.refresh.RefreshRemainderRelation :=
  {
    remainder := base_rumoca_ir_solve_refresh_IssuedRefreshPlan
  }

def base_rumoca_ir_solve_refresh_ContinuousRefreshOwners : rumoca_ir_solve.refresh.ContinuousRefreshOwners :=
  {
    algebraic := base_rumoca_ir_solve_refresh_IssuedRefreshPlan,
    derivative := base_rumoca_ir_solve_refresh_IssuedRefreshPlan,
    root := base_rumoca_ir_solve_refresh_IssuedRefreshPlan,
    event := base_rumoca_ir_solve_refresh_IssuedRefreshPlan,
    clock_events := alloc.vec.Vec.from [] (by scalar_tac),
    static_parameter_indices := Slice.from [] (by scalar_tac),
    exact_assignment_programs := alloc.vec.Vec.from [] (by scalar_tac),
    exact_assignment_schedules := alloc.vec.Vec.from [] (by scalar_tac),
    root_after_derivative := base_rumoca_ir_solve_refresh_RefreshRemainderRelation,
    algebraic_after_derivative := base_rumoca_ir_solve_refresh_RefreshRemainderRelation,
    clock_events_after_event := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_tensor_ComputeBlock : rumoca_ir_solve.tensor.ComputeBlock :=
  {
    nodes := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_ContinuousSolveSystem : rumoca_ir_solve.model.ContinuousSolveSystem :=
  {
    implicit_rhs := base_rumoca_ir_solve_tensor_ComputeBlock,
    implicit_row_targets := alloc.vec.Vec.from [] (by scalar_tac),
    algebraic_projection_plan := base_rumoca_ir_solve_model_AlgebraicProjectionPlan,
    residual := base_rumoca_ir_solve_tensor_ComputeBlock,
    manifold_residual := base_rumoca_ir_solve_tensor_ComputeBlock,
    manifold_projection_plan := base_rumoca_ir_solve_model_AlgebraicProjectionPlan,
    derivative_rhs := base_rumoca_ir_solve_tensor_ComputeBlock,
    refresh_owners := base_rumoca_ir_solve_refresh_ContinuousRefreshOwners
  }

def base_rumoca_ir_solve_model_EventIterationPlan : rumoca_ir_solve.model.EventIterationPlan :=
  {
    runs := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_DiscreteSolveSystem : rumoca_ir_solve.model.DiscreteSolveSystem :=
  {
    event_iteration_plan := base_rumoca_ir_solve_model_EventIterationPlan,
    runtime_assignment_source_rows := alloc.vec.Vec.from [] (by scalar_tac),
    runtime_assignment_rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    runtime_assignment_targets := alloc.vec.Vec.from [] (by scalar_tac),
    runtime_assignment_roles := alloc.vec.Vec.from [] (by scalar_tac),
    post_commit_assignment_rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    post_commit_assignment_targets := alloc.vec.Vec.from [] (by scalar_tac),
    post_commit_assignment_runtime_rows := alloc.vec.Vec.from [] (by scalar_tac),
    rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    update_targets := alloc.vec.Vec.from [] (by scalar_tac),
    row_roles := alloc.vec.Vec.from [] (by scalar_tac),
    pre_modes := alloc.vec.Vec.from [] (by scalar_tac),
    observation_refresh := alloc.vec.Vec.from [] (by scalar_tac),
    observation_refresh_reads_y := false,
    integrator_history_effects := alloc.vec.Vec.from [] (by scalar_tac),
    clock_owners := alloc.vec.Vec.from [] (by scalar_tac),
    guarded_assignments := alloc.vec.Vec.from [] (by scalar_tac),
    event_transactions := alloc.vec.Vec.from [] (by scalar_tac),
    structured_rhs := base_rumoca_ir_solve_tensor_ComputeBlock,
    structured_updates := alloc.vec.Vec.from [] (by scalar_tac),
    clock_partition_order := alloc.vec.Vec.from [] (by scalar_tac),
    clock_partition_intermediates := base_rumoca_ir_solve_ScalarProgramBlock,
    clock_partition_intermediate_targets := alloc.vec.Vec.from [] (by scalar_tac),
    clock_partition_intermediate_clocks := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_InitializationProjectionPlan : rumoca_ir_solve.model.InitializationProjectionPlan :=
  {
    blocks := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_InitializationSolveSystem : rumoca_ir_solve.model.InitializationSolveSystem :=
  {
    residual := base_rumoca_ir_solve_tensor_ComputeBlock,
    row_targets := alloc.vec.Vec.from [] (by scalar_tac),
    row_roles := alloc.vec.Vec.from [] (by scalar_tac),
    mandatory_row_count := 0#usize,
    projection_unknowns := alloc.vec.Vec.from [] (by scalar_tac),
    projection_plan := base_rumoca_ir_solve_model_InitializationProjectionPlan,
    update_rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    update_targets := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_SolveClockPartition : rumoca_ir_solve.model.SolveClockPartition :=
  {
    periodic_event_schedules := alloc.vec.Vec.from [] (by scalar_tac),
    activation_parameter_indices := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_SolveDelayPartition : rumoca_ir_solve.model.SolveDelayPartition :=
  {
    source_rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    delay_time_rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    delay_max_rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    value_parameter_indices := alloc.vec.Vec.from [] (by scalar_tac),
    source_is_discrete := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_SolveEventPartition : rumoca_ir_solve.model.SolveEventPartition :=
  {
    root_conditions := base_rumoca_ir_solve_ScalarProgramBlock,
    root_relation_memory_targets := alloc.vec.Vec.from [] (by scalar_tac),
    root_zero_domains := alloc.vec.Vec.from [] (by scalar_tac),
    root_relation_refresh_roles := alloc.vec.Vec.from [] (by scalar_tac),
    condition_memory_parameter_indices := alloc.vec.Vec.from [] (by scalar_tac),
    scheduled_root_conditions := alloc.vec.Vec.from [] (by scalar_tac),
    scheduled_time_events := alloc.vec.Vec.from [] (by scalar_tac),
    dynamic_time_event_names := alloc.vec.Vec.from [] (by scalar_tac),
    dynamic_time_event_rhs := base_rumoca_ir_solve_ScalarProgramBlock,
    action_conditions := base_rumoca_ir_solve_ScalarProgramBlock,
    actions := alloc.vec.Vec.from [] (by scalar_tac),
    has_terminal_event := false,
    delays := base_rumoca_ir_solve_model_SolveDelayPartition
  }

def base_rumoca_ir_solve_model_SolverNameIndexMaps : rumoca_ir_solve.model.SolverNameIndexMaps :=
  {
    names := alloc.vec.Vec.from [] (by scalar_tac),
    name_to_idx := { entries := [], hasher := { k0 := 0#u64, k1 := 0#u64 } },
    base_to_indices := { entries := [], hasher := { k0 := 0#u64, k1 := 0#u64 } }
  }

def base_rumoca_ir_solve_model_SolveLayout : rumoca_ir_solve.model.SolveLayout :=
  {
    solver_maps := base_rumoca_ir_solve_model_SolverNameIndexMaps,
    variable_storage_runs := alloc.vec.Vec.from [] (by scalar_tac),
    variable_declarations := alloc.vec.Vec.from [] (by scalar_tac),
    state_scalar_count := 0#usize,
    algebraic_scalar_count := 0#usize,
    output_scalar_count := 0#usize,
    parameter_count := 0#usize,
    static_parameter_names := alloc.vec.Vec.from [] (by scalar_tac),
    compiled_parameter_len := 0#usize,
    input_scalar_names := alloc.vec.Vec.from [] (by scalar_tac),
    discrete_real_scalar_names := alloc.vec.Vec.from [] (by scalar_tac),
    discrete_valued_scalar_names := alloc.vec.Vec.from [] (by scalar_tac),
    relation_memory_parameter_indices := alloc.vec.Vec.from [] (by scalar_tac),
    initial_event_parameter_index := none,
    terminal_event_parameter_index := none,
    initial_homotopy_parameter_index := none,
    pre_param_bindings := alloc.vec.Vec.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_SolveProblem : rumoca_ir_solve.SolveProblem :=
  {
    schema_version := 0#u16,
    layout := base_rumoca_ir_solve_layout_VarLayout,
    solve_layout := base_rumoca_ir_solve_model_SolveLayout,
    continuous := base_rumoca_ir_solve_model_ContinuousSolveSystem,
    initialization := base_rumoca_ir_solve_model_InitializationSolveSystem,
    discrete := base_rumoca_ir_solve_model_DiscreteSolveSystem,
    events := base_rumoca_ir_solve_model_SolveEventPartition,
    clocks := base_rumoca_ir_solve_model_SolveClockPartition
  }

def base_rumoca_ir_solve_model_ContinuousStructuralArtifacts : rumoca_ir_solve.model.ContinuousStructuralArtifacts :=
  {
    implicit := none,
    algebraic_projection := Slice.from [] (by scalar_tac),
    algebraic_invalidates_earlier := Slice.from [] (by scalar_tac),
    manifold := none,
    manifold_projection := Slice.from [] (by scalar_tac),
    derivative := none
  }

def base_rumoca_ir_solve_model_MassMatrix : rumoca_ir_solve.model.MassMatrix := rumoca_ir_solve.model.MassMatrix.Identity

def base_rumoca_ir_solve_model_ContinuousSolveArtifacts : rumoca_ir_solve.model.ContinuousSolveArtifacts :=
  {
    structural := base_rumoca_ir_solve_model_ContinuousStructuralArtifacts,
    mass_matrix := base_rumoca_ir_solve_model_MassMatrix,
    implicit_jacobian_v := base_rumoca_ir_solve_tensor_ComputeBlock,
    implicit_jacobian_v_scalar := base_rumoca_ir_solve_ScalarProgramBlock,
    manifold_jacobian_v := base_rumoca_ir_solve_tensor_ComputeBlock,
    full_jacobian_v := base_rumoca_ir_solve_ScalarProgramBlock
  }

def base_rumoca_ir_solve_model_InitializationStructuralArtifacts : rumoca_ir_solve.model.InitializationStructuralArtifacts :=
  {
    residual := none,
    projection := Slice.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_InitializationSolveArtifacts : rumoca_ir_solve.model.InitializationSolveArtifacts :=
  {
    structural := base_rumoca_ir_solve_model_InitializationStructuralArtifacts,
    residual_jacobian_v := base_rumoca_ir_solve_tensor_ComputeBlock
  }

def base_rumoca_ir_solve_model_SolveArtifacts : rumoca_ir_solve.model.SolveArtifacts :=
  {
    continuous := base_rumoca_ir_solve_model_ContinuousSolveArtifacts,
    initialization := base_rumoca_ir_solve_model_InitializationSolveArtifacts
  }

def base_rumoca_ir_solve_model_SolveDensePrefixes : rumoca_ir_solve.model.SolveDensePrefixes :=
  {
    state_names := Slice.from [] (by scalar_tac),
    initial_state_values := Slice.from [] (by scalar_tac),
    state_nominal_values := Slice.from [] (by scalar_tac),
    static_parameter_values := Slice.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_typed_program_types_SolveIntegerDomain : rumoca_ir_solve.typed_program.types.SolveIntegerDomain :=
  {
    minimum := 0#i64,
    maximum := 0#i64
  }

def base_rumoca_ir_solve_typed_program_types_SolveRealFormat : rumoca_ir_solve.typed_program.types.SolveRealFormat := rumoca_ir_solve.typed_program.types.SolveRealFormat.Binary32

def base_rumoca_ir_solve_typed_program_types_SolveArithmeticProfile : rumoca_ir_solve.typed_program.types.SolveArithmeticProfile :=
  {
    real_format := base_rumoca_ir_solve_typed_program_types_SolveRealFormat,
    integer_domain := base_rumoca_ir_solve_typed_program_types_SolveIntegerDomain,
    real_matrix_multiply := base_rumoca_core_matrix_multiply_RealMatrixMultiplySemantics
  }

def base_rumoca_ir_solve_typed_program_call_SolvePureCallTable : rumoca_ir_solve.typed_program.call.SolvePureCallTable :=
  {
    arithmetic := base_rumoca_ir_solve_typed_program_types_SolveArithmeticProfile,
    owners := Slice.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_variable_catalog_SolveVariableCatalog : rumoca_ir_solve.variable_catalog.SolveVariableCatalog :=
  {
    entries := Slice.from [] (by scalar_tac)
  }

def base_rumoca_ir_solve_model_SolveModel : rumoca_ir_solve.model.SolveModel :=
  {
    problem := base_rumoca_ir_solve_SolveProblem,
    pure_calls := base_rumoca_ir_solve_typed_program_call_SolvePureCallTable,
    artifacts := base_rumoca_ir_solve_model_SolveArtifacts,
    initial_y := alloc.vec.Vec.from [] (by scalar_tac),
    solver_nominals := alloc.vec.Vec.from [] (by scalar_tac),
    solver_scales := Slice.from [] (by scalar_tac),
    parameters := alloc.vec.Vec.from [] (by scalar_tac),
    dense_prefixes := base_rumoca_ir_solve_model_SolveDensePrefixes,
    visible_value_rows := base_rumoca_ir_solve_ScalarProgramBlock,
    variable_catalog := base_rumoca_ir_solve_variable_catalog_SolveVariableCatalog
  }

def base_rumoca_ir_solve_model_SolveVariableStorageRole : rumoca_ir_solve.model.SolveVariableStorageRole := rumoca_ir_solve.model.SolveVariableStorageRole.Parameter

def base_rumoca_ir_solve_model_SolveVariableTimeDomain : rumoca_ir_solve.model.SolveVariableTimeDomain := rumoca_ir_solve.model.SolveVariableTimeDomain.Static

def base_rumoca_ir_solve_model_SolveVariableValueKind : rumoca_ir_solve.model.SolveVariableValueKind := rumoca_ir_solve.model.SolveVariableValueKind.Real

def base_rumoca_ir_solve_model_SolveVariableDeclaration : rumoca_ir_solve.model.SolveVariableDeclaration :=
  {
    role := base_rumoca_ir_solve_model_SolveVariableStorageRole,
    value_kind := base_rumoca_ir_solve_model_SolveVariableValueKind,
    time_domain := base_rumoca_ir_solve_model_SolveVariableTimeDomain
  }

def base_rumoca_ir_solve_model_SolveVariableStorageRun : rumoca_ir_solve.model.SolveVariableStorageRun :=
  {
    base := rumoca_ir_solve.layout.SolveStorageCoordinate.P 0#usize,
    scalar_count := 0#usize,
    role := base_rumoca_ir_solve_model_SolveVariableStorageRole,
    value_kind := base_rumoca_ir_solve_model_SolveVariableValueKind
  }

def base_rumoca_ir_solve_variable_catalog_SolveStateInitialization : rumoca_ir_solve.variable_catalog.SolveStateInitialization := rumoca_ir_solve.variable_catalog.SolveStateInitialization.NotState

def base_rumoca_ir_solve_variable_catalog_SolveVariableEvaluatedValues : rumoca_ir_solve.variable_catalog.SolveVariableEvaluatedValues :=
  {
    start := none,
    minimum := none,
    maximum := none,
    nominal := none
  }

def base_rumoca_ir_solve_variable_catalog_SolveVariableSource : rumoca_ir_solve.variable_catalog.SolveVariableSource :=
  {
    source_occurrence := { value := { value := 1#u32 } },
    «name» := "",
    dimensions := Slice.from [] (by scalar_tac),
    scalar_names := Slice.from [] (by scalar_tac),
    provenance := base_rumoca_core_ir_primitives_Span
  }

def base_rumoca_ir_solve_variable_catalog_SolveVariableCausality : rumoca_ir_solve.variable_catalog.SolveVariableCausality := rumoca_ir_solve.variable_catalog.SolveVariableCausality.Input

def base_rumoca_ir_solve_variable_catalog_SolveVariableVariability : rumoca_ir_solve.variable_catalog.SolveVariableVariability := rumoca_ir_solve.variable_catalog.SolveVariableVariability.Constant

def base_rumoca_ir_solve_variable_catalog_SolveVariableSourceAttributes : rumoca_ir_solve.variable_catalog.SolveVariableSourceAttributes :=
  {
    causality := base_rumoca_ir_solve_variable_catalog_SolveVariableCausality,
    variability := base_rumoca_ir_solve_variable_catalog_SolveVariableVariability,
    tunable := false,
    unit := none,
    description := none,
    fixed := base_rumoca_core_ir_primitives_Fixity
  }

def base_rumoca_ir_solve_variable_catalog_SolveVariableCatalogEntry : rumoca_ir_solve.variable_catalog.SolveVariableCatalogEntry :=
  {
    id := 0#u32,
    source := base_rumoca_ir_solve_variable_catalog_SolveVariableSource,
    attributes := base_rumoca_ir_solve_variable_catalog_SolveVariableSourceAttributes,
    values := base_rumoca_ir_solve_variable_catalog_SolveVariableEvaluatedValues,
    declaration := base_rumoca_ir_solve_model_SolveVariableDeclaration,
    storage := base_rumoca_ir_solve_model_SolveVariableStorageRun,
    state_initialization := base_rumoca_ir_solve_variable_catalog_SolveStateInitialization
  }

def base_scalar_constant_derivative_refinement_DerivativePatternFact : scalar_constant_derivative_refinement.DerivativePatternFact := scalar_constant_derivative_refinement.DerivativePatternFact.Absent

def base_scalar_constant_derivative_refinement_MassMatrixFact : scalar_constant_derivative_refinement.MassMatrixFact := scalar_constant_derivative_refinement.MassMatrixFact.Identity

def base_scalar_constant_derivative_refinement_SolveMetadataFacts : scalar_constant_derivative_refinement.SolveMetadataFacts :=
  {
    pure_call_owners := 0#usize,
    implicit_row_targets := 0#usize,
    algebraic_projection_blocks := 0#usize,
    manifold_projection_blocks := 0#usize,
    initialization_projection_unknowns := 0#usize,
    initialization_projection_blocks := 0#usize,
    initialization_update_targets := 0#usize,
    discrete_update_targets := 0#usize,
    discrete_event_iteration_runs := 0#usize,
    discrete_runtime_assignment_targets := 0#usize,
    discrete_runtime_assignment_roles := 0#usize,
    discrete_post_commit_targets := 0#usize,
    discrete_post_commit_runtime_rows := 0#usize,
    discrete_row_roles := 0#usize,
    discrete_pre_modes := 0#usize,
    discrete_observation_refresh := 0#usize,
    discrete_observation_refresh_reads_y := 0#usize,
    discrete_integrator_history_effects := 0#usize,
    discrete_clock_owners := 0#usize,
    discrete_structured_updates := 0#usize,
    discrete_guarded_assignments := 0#usize,
    discrete_event_transactions := 0#usize,
    discrete_clock_partition_order := 0#usize,
    discrete_clock_intermediate_targets := 0#usize,
    discrete_clock_intermediate_clocks := 0#usize,
    event_root_memory_targets := 0#usize,
    event_root_zero_domains := 0#usize,
    event_root_refresh_roles := 0#usize,
    event_condition_memories := 0#usize,
    event_scheduled_roots := 0#usize,
    event_scheduled_times := 0#usize,
    event_dynamic_time_names := 0#usize,
    event_actions := 0#usize,
    event_has_terminal := 0#usize,
    event_delay_targets := 0#usize,
    event_delay_discrete_flags := 0#usize,
    clock_schedules := 0#usize,
    clock_activation_parameters := 0#usize,
    continuous_refresh_rows := 0#usize,
    continuous_refresh_static_parameters := 0#usize,
    mass_matrix := base_scalar_constant_derivative_refinement_MassMatrixFact,
    structural_implicit := 0#usize,
    structural_algebraic_projection_blocks := 0#usize,
    structural_manifold := 0#usize,
    structural_manifold_projection_blocks := 0#usize,
    structural_derivative := base_scalar_constant_derivative_refinement_DerivativePatternFact,
    initialization_structural_residual := 0#usize,
    initialization_structural_projection_blocks := 0#usize
  }


abbrev SolveModel := rumoca_ir_solve.model.SolveModel
abbrev Block := rumoca_ir_solve.ScalarProgramBlock

/-- Forwarded premise. Nothing claims where a production caller obtains it. -/
def owners0 : Aeneas.Std.Array Usize 28#usize :=
  Aeneas.Std.Array.mk (Data.ListN.ListN.fromList (List.replicate 28 (0#usize)))

/-- Forwarded premise, as above. -/
abbrev meta0 : SolveMetadataFacts :=
  base_scalar_constant_derivative_refinement_SolveMetadataFacts

/-- The single explicit root. Every variation below replaces exactly one field. -/
abbrev baseModel : SolveModel := base_rumoca_ir_solve_model_SolveModel

def IsOk {a : Type} : Result a → Bool
  | .ok _ => true
  | _ => false

def entryWithStart (s : Option (Slice F64))
    : rumoca_ir_solve.variable_catalog.SolveVariableCatalogEntry :=
  { base_rumoca_ir_solve_variable_catalog_SolveVariableCatalogEntry with
    values :=
      { base_rumoca_ir_solve_variable_catalog_SolveVariableEvaluatedValues with start := s } }

def modelWithStart (s : Option (Slice F64)) : SolveModel :=
  { baseModel with variable_catalog :=
      { base_rumoca_ir_solve_variable_catalog_SolveVariableCatalog with
        entries := Slice.from [entryWithStart s] (by scalar_tac) } }

def bitsModel (bits : U64) : SolveModel :=
  modelWithStart (some (Slice.from [{ storageBits := bits }] (by scalar_tac)))

def startFacts (m : SolveModel) : Option (Bool × Usize × U64) :=
  match project_solve_values_with_owner_premises m owners0 meta0 with
  | .ok f => some (f.catalog_start_present, f.catalog_start_width, f.catalog_start_bits)
  | _ => none

def prog2 : alloc.vec.Vec rumoca_ir_solve.linear_op.LinearOp :=
  alloc.vec.Vec.from
    [rumoca_ir_solve.linear_op.LinearOp.LoadY 0#u32 0#usize,
     rumoca_ir_solve.linear_op.LinearOp.StoreOutput 0#u32] (by scalar_tac)

/-- A block the projection classifies exactly rather than refusing on shape. -/
def exactBlock2 : Block :=
  { base_rumoca_ir_solve_ScalarProgramBlock with
    output_indices := alloc.vec.Vec.from [0#usize] (by scalar_tac),
    programs := alloc.vec.Vec.from [prog2] (by scalar_tac) }

def modelWithVisible (b : Block) : SolveModel :=
  { baseModel with visible_value_rows := b }

end RumocaRootValueFixture
