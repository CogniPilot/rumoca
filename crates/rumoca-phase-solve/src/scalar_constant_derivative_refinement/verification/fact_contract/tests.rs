mod diagnostics;

use super::*;

fn empty_metadata() -> SolveMetadataFacts {
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
            columns: 1,
        },
    }
}

fn conforming_facts(start_bits: u64, constant_bits: u64) -> SolveFacts {
    SolveFacts {
        catalog_start_present: true,
        catalog_start_width: 1,
        catalog_start_bits: start_bits,
        initial_y_width: 1,
        initial_y_bits: start_bits,
        kernel: ScalarBlockFacts::Exact([
            OperationFact::Constant {
                destination: 0,
                bits: constant_bits,
            },
            OperationFact::StoreOutput { source: 0 },
        ]),
        full_jacobian: ScalarBlockFacts::Exact([
            OperationFact::Constant {
                destination: 0,
                bits: constant_bits,
            },
            OperationFact::Constant {
                destination: 1,
                bits: 0,
            },
            OperationFact::StoreOutput { source: 1 },
        ]),
        visible_rows: ScalarBlockFacts::Exact([
            OperationFact::LoadY {
                destination: 0,
                index: 0,
            },
            OperationFact::StoreOutput { source: 0 },
        ]),
        owners: [
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0,
        ],
        metadata: empty_metadata(),
    }
}

#[test]
fn complete_relation_preserves_normalized_profile_bits() {
    for bits in [
        0,
        1,
        1 << 63,
        1.0_f64.to_bits(),
        0x7ff8_0000_0000_0042,
        u64::MAX,
    ] {
        let dae = profile(bits, bits ^ 1);
        let solve = conforming_facts(bits, bits ^ 1);
        assert!(assert_complete_contract(&dae, &solve).is_ok());
    }
}

#[test]
fn complete_relation_rejects_each_foreign_owner() {
    let dae = profile(2.0_f64.to_bits(), 1.0_f64.to_bits());
    for index in 0..28 {
        let mut solve = conforming_facts(dae.start_bits, dae.derivative_constant_bits);
        solve.owners[index] ^= 1;
        assert!(matches!(
            assert_complete_contract(&dae, &solve),
            Err(ScalarConstantDerivativeMismatch::SolveOwner { .. })
        ));
    }
}

#[test]
fn complete_relation_rejects_empty_nonidentity_mass_matrix() {
    let dae = profile(0, 0);
    let mut solve = conforming_facts(0, 0);
    solve.metadata.mass_matrix = MassMatrixFact::Sparse { entry_count: 0 };
    assert!(matches!(
        assert_complete_contract(&dae, &solve),
        Err(ScalarConstantDerivativeMismatch::MassMatrix { .. })
    ));
}

#[test]
fn complete_relation_rejects_foreign_metadata_and_wrong_program() {
    let dae = profile(0, 0);
    let mut solve = conforming_facts(0, 0);
    solve.metadata.event_delay_targets = 1;
    assert!(matches!(
        assert_complete_contract(&dae, &solve),
        Err(ScalarConstantDerivativeMismatch::SolveMetadata { .. })
    ));
    solve.metadata.event_delay_targets = 0;
    solve.kernel = ScalarBlockFacts::Exact([
        OperationFact::LoadY {
            destination: 0,
            index: 0,
        },
        OperationFact::StoreOutput { source: 0 },
    ]);
    assert!(matches!(
        assert_complete_contract(&dae, &solve),
        Err(ScalarConstantDerivativeMismatch::KernelOperations)
    ));
}

#[test]
fn complete_relation_rejects_start_substitution_and_negative_zero_tangent() {
    let dae = profile(0, 0);
    let mut solve = conforming_facts(0, 0);
    solve.initial_y_bits = 1 << 63;
    assert!(matches!(
        assert_complete_contract(&dae, &solve),
        Err(ScalarConstantDerivativeMismatch::InitialYBits { .. })
    ));
    solve.initial_y_bits = 0;
    solve.full_jacobian = ScalarBlockFacts::Exact([
        OperationFact::Constant {
            destination: 0,
            bits: 0,
        },
        OperationFact::Constant {
            destination: 1,
            bits: 1 << 63,
        },
        OperationFact::StoreOutput { source: 1 },
    ]);
    assert!(matches!(
        assert_complete_contract(&dae, &solve),
        Err(ScalarConstantDerivativeMismatch::FullJacobianTangentBits { .. })
    ));
}
