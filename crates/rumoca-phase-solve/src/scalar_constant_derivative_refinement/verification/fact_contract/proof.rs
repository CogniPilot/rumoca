//! Symbolic domain for the complete pure comparison, not full IR projection.
use super::*;

fn operation() -> OperationFact {
    let kind: u8 = kani::any();
    kani::assume(kind < 4);
    match kind {
        0 => OperationFact::Constant {
            destination: kani::any(),
            bits: kani::any(),
        },
        1 => OperationFact::LoadY {
            destination: kani::any(),
            index: kani::any(),
        },
        2 => OperationFact::StoreOutput {
            source: kani::any(),
        },
        _ => OperationFact::Unsupported,
    }
}

fn block<const WIDTH: usize>() -> ScalarBlockFacts<WIDTH> {
    if kani::any() {
        ScalarBlockFacts::UnsupportedShape
    } else {
        ScalarBlockFacts::Exact(std::array::from_fn(|_| operation()))
    }
}

fn mass_matrix() -> MassMatrixFact {
    let kind: u8 = kani::any();
    kani::assume(kind < 3);
    match kind {
        0 => MassMatrixFact::Identity,
        1 => MassMatrixFact::Diagonal {
            entry_count: kani::any(),
        },
        _ => MassMatrixFact::Sparse {
            entry_count: kani::any(),
        },
    }
}

fn derivative_pattern() -> DerivativePatternFact {
    let kind: u8 = kani::any();
    kani::assume(kind < 3);
    match kind {
        0 => DerivativePatternFact::Absent,
        1 => DerivativePatternFact::Empty {
            rows: kani::any(),
            columns: kani::any(),
        },
        _ => DerivativePatternFact::Other {
            kind: nonempty_pattern_kind(),
            rows: kani::any(),
            columns: kani::any(),
        },
    }
}

fn nonempty_pattern_kind() -> crate::NonemptyDerivativePatternKind {
    use crate::NonemptyDerivativePatternKind;
    let kind: u8 = kani::any();
    kani::assume(kind < 5);
    match kind {
        0 => NonemptyDerivativePatternKind::Full,
        1 => NonemptyDerivativePatternKind::Diagonal,
        2 => NonemptyDerivativePatternKind::Banded,
        3 => NonemptyDerivativePatternKind::Csr,
        _ => NonemptyDerivativePatternKind::Affine,
    }
}

fn metadata() -> SolveMetadataFacts {
    SolveMetadataFacts {
        pure_call_owners: kani::any(),
        implicit_row_targets: kani::any(),
        algebraic_projection_blocks: kani::any(),
        manifold_projection_blocks: kani::any(),
        initialization_projection_unknowns: kani::any(),
        initialization_projection_blocks: kani::any(),
        initialization_update_targets: kani::any(),
        discrete_update_targets: kani::any(),
        discrete_event_iteration_runs: kani::any(),
        discrete_runtime_assignment_targets: kani::any(),
        discrete_runtime_assignment_roles: kani::any(),
        discrete_post_commit_targets: kani::any(),
        discrete_post_commit_runtime_rows: kani::any(),
        discrete_row_roles: kani::any(),
        discrete_pre_modes: kani::any(),
        discrete_observation_refresh: kani::any(),
        discrete_observation_refresh_reads_y: kani::any(),
        discrete_integrator_history_effects: kani::any(),
        discrete_clock_owners: kani::any(),
        discrete_structured_updates: kani::any(),
        discrete_guarded_assignments: kani::any(),
        discrete_event_transactions: kani::any(),
        discrete_clock_partition_order: kani::any(),
        discrete_clock_intermediate_targets: kani::any(),
        discrete_clock_intermediate_clocks: kani::any(),
        event_root_memory_targets: kani::any(),
        event_root_zero_domains: kani::any(),
        event_root_refresh_roles: kani::any(),
        event_condition_memories: kani::any(),
        event_scheduled_roots: kani::any(),
        event_scheduled_times: kani::any(),
        event_dynamic_time_names: kani::any(),
        event_actions: kani::any(),
        event_has_terminal: kani::any(),
        event_delay_targets: kani::any(),
        event_delay_discrete_flags: kani::any(),
        clock_schedules: kani::any(),
        clock_activation_parameters: kani::any(),
        continuous_refresh_rows: kani::any(),
        continuous_refresh_static_parameters: kani::any(),
        structural_implicit: kani::any(),
        structural_algebraic_projection_blocks: kani::any(),
        structural_manifold: kani::any(),
        structural_manifold_projection_blocks: kani::any(),
        initialization_structural_residual: kani::any(),
        initialization_structural_projection_blocks: kani::any(),
        mass_matrix: mass_matrix(),
        structural_derivative: derivative_pattern(),
    }
}

#[kani::proof]
#[kani::unwind(32)]
fn c61_complete_fact_checker_matches_positive_profile() {
    let dae = profile(kani::any(), kani::any());
    let solve = SolveFacts {
        catalog_start_present: kani::any(),
        catalog_start_width: kani::any(),
        catalog_start_bits: kani::any(),
        initial_y_width: kani::any(),
        initial_y_bits: kani::any(),
        kernel: block(),
        full_jacobian: block(),
        visible_rows: block(),
        owners: kani::any(),
        metadata: metadata(),
    };
    let result = assert_complete_contract(&dae, &solve);
    let (accepted, mass_refused, owner_refused, constant_refused, visible_refused) = match result {
        Ok(_) => (true, false, false, false, false),
        Err(ScalarConstantDerivativeMismatch::MassMatrix { .. }) => {
            (false, true, false, false, false)
        }
        Err(ScalarConstantDerivativeMismatch::SolveOwner { .. }) => {
            (false, false, true, false, false)
        }
        Err(ScalarConstantDerivativeMismatch::DerivativeConstantBits { .. }) => {
            (false, false, false, true, false)
        }
        Err(ScalarConstantDerivativeMismatch::VisibleRowOperations) => {
            (false, false, false, false, true)
        }
        Err(_) => (false, false, false, false, false),
    };
    kani::cover!(accepted, "the complete normalized profile accepts");
    kani::cover!(
        mass_refused,
        "nonidentity metadata reaches its rejecting comparison"
    );
    kani::cover!(
        owner_refused,
        "a foreign executable owner reaches its rejecting comparison"
    );
    kani::cover!(
        constant_refused,
        "wrong derivative bits reach the composed kernel comparison"
    );
    kani::cover!(
        visible_refused,
        "wrong visible operations reach the final composed comparison"
    );
}
