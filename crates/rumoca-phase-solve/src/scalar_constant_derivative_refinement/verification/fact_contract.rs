//! Independent positive relation for the complete C61 pure fact comparison.
#[cfg(kani)]
mod proof;
#[cfg(all(test, not(kani)))]
mod tests;

use super::super::{
    AdmittedScalarConstantDerivativeProfile, CheckedDaeSolveScalarConstantDerivativeRefinement,
    DerivativePatternFact, MassMatrixFact, OperationFact, ScalarBlockFacts,
    ScalarConstantDerivativeMismatch, SolveFacts, SolveMetadataFacts,
    check_scalar_constant_derivative_refinement,
};

// Only checker facts are constructed here, never an IR root or installable
// lowering carrier. The actual DAE admission and root correlation stay outside
// this comparison proof. Both residual shapes normalize at actual DAE admission.
fn profile(
    start_bits: u64,
    derivative_constant_bits: u64,
) -> AdmittedScalarConstantDerivativeProfile {
    AdmittedScalarConstantDerivativeProfile {
        start_bits,
        derivative_constant_bits,
    }
}

// Exhaustive field patterns intentionally differ from the shipping sequence
// of guards. A new field cannot silently inherit admission through `..`.
fn metadata_matches_profile(metadata: &SolveMetadataFacts) -> bool {
    matches!(
        metadata,
        SolveMetadataFacts {
            pure_call_owners: 0,
            implicit_row_targets: 0,
            algebraic_projection_blocks: 0,
            manifold_projection_blocks: 0,
            initialization_projection_unknowns: 0,
            initialization_projection_blocks: 0,
            initialization_update_targets: 0,
            discrete_update_targets: 0,
            discrete_event_iteration_runs: 0,
            discrete_runtime_assignment_targets: 0,
            discrete_runtime_assignment_roles: 0,
            discrete_post_commit_targets: 0,
            discrete_post_commit_runtime_rows: 0,
            discrete_row_roles: 0,
            discrete_pre_modes: 0,
            discrete_observation_refresh: 0,
            discrete_observation_refresh_reads_y: 0,
            discrete_integrator_history_effects: 0,
            discrete_clock_owners: 0,
            discrete_structured_updates: 0,
            discrete_guarded_assignments: 0,
            discrete_event_transactions: 0,
            discrete_clock_partition_order: 0,
            discrete_clock_intermediate_targets: 0,
            discrete_clock_intermediate_clocks: 0,
            event_root_memory_targets: 0,
            event_root_zero_domains: 0,
            event_root_refresh_roles: 0,
            event_condition_memories: 0,
            event_scheduled_roots: 0,
            event_scheduled_times: 0,
            event_dynamic_time_names: 0,
            event_actions: 0,
            event_has_terminal: 0,
            event_delay_targets: 0,
            event_delay_discrete_flags: 0,
            clock_schedules: 0,
            clock_activation_parameters: 0,
            continuous_refresh_rows: 0,
            continuous_refresh_static_parameters: 0,
            structural_implicit: 0,
            structural_algebraic_projection_blocks: 0,
            structural_manifold: 0,
            structural_manifold_projection_blocks: 0,
            initialization_structural_residual: 0,
            initialization_structural_projection_blocks: 0,
            mass_matrix: MassMatrixFact::Identity,
            structural_derivative: DerivativePatternFact::Empty {
                rows: 1,
                columns: 1
            },
        }
    )
}

fn facts_match_profile(dae: &AdmittedScalarConstantDerivativeProfile, solve: &SolveFacts) -> bool {
    let SolveFacts {
        catalog_start_present: true,
        catalog_start_width: 1,
        catalog_start_bits,
        initial_y_width: 1,
        initial_y_bits,
        kernel:
            ScalarBlockFacts::Exact(
                [
                    OperationFact::Constant {
                        destination: 0,
                        bits: derivative,
                    },
                    OperationFact::StoreOutput { source: 0 },
                ],
            ),
        full_jacobian:
            ScalarBlockFacts::Exact(
                [
                    OperationFact::Constant {
                        destination: 0,
                        bits: primal,
                    },
                    OperationFact::Constant {
                        destination: 1,
                        bits: 0,
                    },
                    OperationFact::StoreOutput { source: 1 },
                ],
            ),
        visible_rows:
            ScalarBlockFacts::Exact(
                [
                    OperationFact::LoadY {
                        destination: 0,
                        index: 0,
                    },
                    OperationFact::StoreOutput { source: 0 },
                ],
            ),
        // The only executable owners are derivative, full JVP and visible:
        // positions 3, 21 and 23 of the existing complete owner vocabulary.
        owners:
            [
                0,
                0,
                0,
                1,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
                1,
                0,
                1,
                0,
                0,
                0,
                0,
            ],
        metadata,
    } = solve
    else {
        return false;
    };
    *catalog_start_bits == dae.start_bits
        && *initial_y_bits == dae.start_bits
        && *derivative == dae.derivative_constant_bits
        && *primal == dae.derivative_constant_bits
        && metadata_matches_profile(metadata)
}

fn assert_complete_contract(
    dae: &AdmittedScalarConstantDerivativeProfile,
    solve: &SolveFacts,
) -> Result<CheckedDaeSolveScalarConstantDerivativeRefinement, ScalarConstantDerivativeMismatch> {
    let result = check_scalar_constant_derivative_refinement(dae, solve);
    assert_eq!(result.is_ok(), facts_match_profile(dae, solve));
    result
}
