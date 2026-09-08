//! Runtime projection and event regression tests.

use super::*;

#[test]
fn prepared_refresh_projection_rejects_missing_foreign_and_duplicate_citations() {
    let source = solve::RefreshScalarProgramSource::checked(0, 0).unwrap();
    let foreign = solve::RefreshScalarProgramSource::checked(0, 1).unwrap();
    assert_eq!(
        PreparedRefreshProgramRow::checked(source, 3, Some(source))
            .unwrap()
            .index(),
        3
    );
    for projected in [None, Some(foreign)] {
        let error = PreparedRefreshProgramRow::checked(source, 3, projected).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("foreign final scalar projection")
        );
    }
    let missing = PreparedRefreshProgramCatalog::construct(&[]).unwrap();
    let error = missing.bind(source).unwrap_err();
    assert!(error.to_string().contains("no final scalar projection"));
    let error =
        PreparedRefreshProgramCatalog::construct(&[Some(source), Some(source)]).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("repeats a continuous refresh source identity")
    );
}

#[test]
fn interpreter_execution_owner_inventory_is_exhaustive_and_unique() {
    let plan = InterpreterExecutionPlan::selected();
    let inventory = plan.owner_inventory();
    assert_eq!(inventory, ALL_INTERPRETER_EXECUTION_OWNERS);
    let unique = inventory.into_iter().collect::<BTreeSet<_>>();
    assert_eq!(unique.len(), ALL_INTERPRETER_EXECUTION_OWNERS.len());
    assert_eq!(
        ALL_INTERPRETER_EXECUTION_OWNERS.map(InterpreterExecutionOwner::catalog_ordinal),
        std::array::from_fn(|index| index),
    );
}
use crate::test_support::empty_binary64_first_product_model;

fn valid_algebraic_refresh_plan(model: &solve::SolveModel) -> solve::IssuedRefreshPlan {
    model
        .problem()
        .continuous()
        .refresh_owners()
        .algebraic()
        .clone()
}

fn test_span(name: &'static str) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2)
}

fn spanned_block(rows: Vec<Vec<solve::LinearOp>>, name: &'static str) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_source_span(
        rows,
        test_span(name)
            .require_provenance("solve-runtime fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn mirror_scalar_implicit_jvp(model: &mut crate::test_support::SolveModelFixture) {
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .clone(),
    );
}

fn set_test_implicit_jvp(
    model: solve::SolveModel,
    rows: Vec<Vec<solve::LinearOp>>,
    name: &'static str,
) -> solve::SolveModel {
    crate::test_support::reseal_with(model, |fixture| {
        let output_indices = fixture
            .problem
            .continuous()
            .implicit_rhs()
            .produced_output_indices("test implicit JVP source")
            .expect("fixture implicit source has a checked output inventory");
        let spans = vec![test_span(name); rows.len()];
        fixture.artifacts.continuous.implicit_jacobian_v_scalar =
            solve::ScalarProgramBlock::with_output_indices(rows, spans, output_indices)
                .expect("test implicit JVP exactly covers its source rows");
        mirror_scalar_implicit_jvp(fixture);
    })
}

fn warm_start_test_model() -> solve::SolveModel {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "z".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let implicit_span = test_span("solver_y_warm_start.mo")
        .require_provenance("warm-start implicit fixture")
        .expect("fixture span is source-backed");
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                vec![shifted_variable_residual_row(1, 0.0)],
                vec![implicit_span.into()],
                vec![1],
            )
            .expect("warm-start implicit program is computable"),
        ),
        implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![derivative_placeholder_row(0)],
            "solver_y_warm_start_derivative.mo",
        )),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("warm-start fixture satisfies the checked root contract"),
        initial_y: vec![1.0, 2.0],
        solver_nominals: vec![1.0; 2],
        ..empty_binary64_first_product_model()
    };
    set_test_implicit_jvp(
        model,
        vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "solver_y_warm_start_jvp.mo",
    )
}

#[test]
fn solver_y_warm_start_preserves_algebraic_guess() {
    let model = warm_start_test_model();
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let mut solver_y = vec![10.0, 42.0];

    runtime
        .update_solver_y_guess_from_state(&mut solver_y, &[3.0])
        .expect("state update should preserve a compatible algebraic guess");

    assert_eq!(solver_y, vec![3.0, 42.0]);
}

#[test]
fn row_eval_context_carries_the_model_pure_call_table() {
    let model = warm_start_test_model();
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");

    let context = runtime
        .execution_plan
        .interpreter
        .discrete_scalar_rows
        .row_eval_context(&runtime);
    let table = context
        .pure_calls
        .expect("every runtime row must inherit its model-owned pure-call table");

    assert!(std::ptr::eq(table, runtime.model().pure_calls()));
}

#[test]
fn solver_y_warm_start_rejects_layout_mismatch() {
    let model = warm_start_test_model();
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let mut solver_y = vec![9.0];

    let error = runtime
        .update_solver_y_guess_from_state(&mut solver_y, &[3.0])
        .expect_err("an established warm start must match the Solve-IR layout");

    assert!(error.to_string().contains("expected 2, got 1"));
    assert_eq!(solver_y, vec![9.0]);
}

#[test]
fn refresh_proof_rejects_invalid_native_stride_metadata() {
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("bad.mo"), 3, 8);
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "i".to_string(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            domain: domain.clone(),
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("valid dense output map"),
            base_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 99,
                terms: Vec::new(),
            }],
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span,
        }],
    };
    let solve_layout = solve::SolveLayout::default();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let mut continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: block,
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let error = continuous
        .try_refresh_plans(&solve_layout, &discrete, &events, &clocks)
        .expect_err("invalid native stride metadata must not receive an owner proof");
    assert!(
        error
            .to_string()
            .contains("load stride at op 99 of 2 targets missing op"),
        "error should explain invalid native metadata: {error}"
    );
    assert_eq!(error.source_span(), Some(span));
}

mod event_iteration;
#[test]
fn derivative_refresh_keeps_coupled_dependency_block_but_drops_unrelated_output() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec![
                "x".to_string(),
                "a".to_string(),
                "b".to_string(),
                "unrelated".to_string(),
            ],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 2,
        output_scalar_count: 1,
        ..Default::default()
    };
    let implicit_span = test_span("derivative_dependency_slice.mo")
        .require_provenance("derivative dependency fixture")
        .expect("fixture span is source-backed");
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                vec![
                    add_assignment_residual_row(1, 2, 1.0),
                    scale_assignment_residual_row(2, 0, 2.0),
                    scale_assignment_residual_row(3, 0, 3.0),
                ],
                vec![rumoca_core::Span::from(implicit_span); 3],
                vec![1, 2, 3],
            )
            .expect("dependency-slice implicit block is computable"),
        ),
        implicit_row_targets: vec![
            None,
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
            Some(solve::scalar_slot_y(3)),
        ],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![1, 2],
                    y_indices: vec![1, 2],
                    tearing: None,
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![3],
                    y_indices: vec![3],
                    tearing: None,
                },
            ],
        },
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![derivative_placeholder_row(1)],
            "derivative_dependency_slice_rhs.mo",
        )),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let plan = continuous.refresh_owners().derivative().clone();

    assert_eq!(
        plan.rows()
            .iter()
            .map(|row| row.target_index())
            .collect::<Vec<_>>(),
        vec![2, 1]
    );
    assert!(!plan.causal_solution_certified());
    assert_eq!(plan.simultaneous_plan().blocks.len(), 1);
    assert_eq!(plan.simultaneous_block_indices(), [0]);
    assert_eq!(plan.simultaneous_plan().blocks[0].rows, vec![1, 2]);
    assert_eq!(plan.simultaneous_plan().blocks[0].y_indices, vec![1, 2]);
}

#[test]
fn construction_rejects_missing_owner_without_exact_isolation() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "a".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![nonlinear_target_residual_row(1, 0)],
            "missing_exact_dependency_owner.mo",
        )),
        implicit_row_targets: vec![None],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![derivative_placeholder_row(1)],
            "missing_exact_dependency_owner_rhs.mo",
        )),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let error = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(Default::default(), 2, 0),
        solve_layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        discrete,
        events,
        clocks,
    )
    .expect_err("an implicit projection row without an exact target owner must not seal");

    assert!(
        error.to_string().contains("explicit row/target pairs"),
        "constructor should report missing exact ownership: {error}"
    );
}

#[test]
fn refresh_plan_accepts_scaled_affine_residual_target() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "a".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        parameter_count: 2,
        static_parameter_names: vec!["scale".to_string(), "offset".to_string()],
        compiled_parameter_len: 2,
        ..Default::default()
    };
    let implicit_span = test_span("scaled_affine_residual.mo")
        .require_provenance("scaled affine fixture")
        .expect("fixture span is source-backed");
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                vec![scaled_assignment_residual_row()],
                vec![implicit_span.into()],
                vec![1],
            )
            .expect("scaled affine program is computable"),
        ),
        implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            1,
            test_span("scaled_affine_constant_state.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 2),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("scaled affine fixture satisfies the checked root contract"),
        initial_y: vec![0.0; 2],
        solver_nominals: vec![1.0; 2],
        parameters: vec![0.0; 2],
        ..empty_binary64_first_product_model()
    };
    let block =
        PreparedScalarProgramBlock::from_compute_block(model.problem().continuous().implicit_rhs())
            .expect("valid implicit RHS should prepare");

    let plan = valid_algebraic_refresh_plan(&model);
    let value = block
        .eval_target_assignment_row_with_context(
            0,
            1,
            &[0.0, 0.0],
            &[6.0, 2.0],
            0.0,
            RowEvalContext::default(),
        )
        .expect("scaled residual should evaluate");

    assert_eq!(plan.rows().len(), 1);
    assert_eq!(plan.rows()[0].source().program(), 0);
    assert_eq!(plan.rows()[0].target_index(), 1);
    assert_eq!(value, Some(3.0));
}

#[test]
fn causal_certificate_keeps_equation_rows_distinct_from_solver_y_indices() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["state".to_string(), "algebraic".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![shifted_variable_residual_row(1, 3.0)],
            "distinct_equation_and_y_namespaces.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            1,
            test_span("equation_target_constant_state.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("equation-target fixture satisfies the checked root contract"),
        initial_y: vec![0.0; 2],
        solver_nominals: vec![1.0; 2],
        ..empty_binary64_first_product_model()
    };
    let plan = valid_algebraic_refresh_plan(&model);

    assert!(plan.causal_solution_certified());
    assert_eq!(plan.rows().len(), 1);
    assert_eq!(plan.rows()[0].equation_index(), 0);
    assert_eq!(plan.rows()[0].target_index(), 1);
}

#[test]
fn batched_assignment_refresh_preserves_row_order_dependencies() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "a".to_string(), "b".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        output_scalar_count: 1,
        ..Default::default()
    };
    let span = test_span("batched_assignment_refresh.mo")
        .require_provenance("batched assignment fixture")
        .expect("fixture span is source-backed");
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                vec![
                    add_assignment_residual_row(1, 0, 2.0),
                    scale_assignment_residual_row(2, 1, 3.0),
                ],
                vec![rumoca_core::Span::from(span); 2],
                vec![1, 2],
            )
            .expect("batched assignment block is computable"),
        ),
        implicit_row_targets: vec![
            None,
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
        ],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                    tearing: None,
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![2],
                    y_indices: vec![2],
                    tearing: None,
                },
            ],
        },
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            1,
            test_span("batched_assignment_constant_state.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 3, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("batched assignment fixture satisfies the checked root contract"),
        initial_y: vec![1.0, 0.0, 0.0],
        solver_nominals: vec![1.0; 3],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime = SolveRuntime::new(std::sync::Arc::clone(&model)).expect("runtime should prepare");
    assert!(runtime.algebraic_refresh.causal_solution_certified());
    let mut solver_y = model.initial_y().to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 4)
        .expect("batched assignment refresh should evaluate");

    assert_eq!(solver_y, vec![1.0, 3.0, 9.0]);
}

#[test]
fn certified_assignment_refresh_rejects_nonfinite_value_and_restores_input() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![nonfinite_assignment_residual_row(0)],
            "nonfinite_certified_assignment.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 1, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("nonfinite assignment fixture satisfies the checked root contract"),
        initial_y: vec![7.0],
        solver_nominals: vec![1.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime = SolveRuntime::new(std::sync::Arc::clone(&model)).expect("runtime should prepare");
    assert!(runtime.algebraic_refresh.causal_solution_certified());
    let mut solver_y = model.initial_y().to_vec();

    let error = runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 4)
        .expect_err("nonfinite direct assignment must not take the certified fast exit");

    assert!(
        error.to_string().contains("inf"),
        "unexpected error: {error}"
    );
    assert_eq!(solver_y, vec![7.0]);
}

#[test]
fn construction_rejects_swapped_blt_equation_target_pairs() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "y".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 2,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![
                shifted_variable_residual_row(0, 1.0),
                shifted_variable_residual_row(1, 2.0),
            ],
            "swapped_blt_pairs.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![1],
                    tearing: None,
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![0],
                    tearing: None,
                },
            ],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let error = continuous
        .try_seal(&solve_layout, &discrete, &events, &clocks)
        .expect_err("swapped BLT ownership must not seal");

    assert!(
        error
            .to_string()
            .contains("not owned by its canonical algebraic projection"),
        "constructor should report swapped target ownership: {error}"
    );
}

fn uncertified_seed_projection_rows() -> Vec<Vec<solve::LinearOp>> {
    vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Binary {
                dst: 1,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 0,
            },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Sub,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::StoreOutput { src: 3 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::LoadY { dst: 3, index: 1 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
    ]
}

fn uncertified_seed_projection_model() -> solve::SolveModel {
    let span = test_span("uncertified_seed_projection.mo");
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["a".to_string(), "b".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 2,
        parameter_count: 1,
        static_parameter_names: vec!["seed".to_string()],
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                uncertified_seed_projection_rows(),
                vec![span, span],
                vec![0, 1],
            )
            .expect("fixture scalar programs satisfy register flow"),
        ),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    tearing: None,
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                    tearing: None,
                },
            ],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 1),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("uncertified-seed fixture satisfies the checked root contract"),
        initial_y: vec![1.0, 0.0],
        solver_nominals: vec![1.0; 2],
        parameters: vec![9.0],
        ..empty_binary64_first_product_model()
    }
}

#[test]
fn uncertified_seed_keeps_its_projection_block_after_dependency_projection() {
    let model = uncertified_seed_projection_model();
    let model = std::sync::Arc::new(model);
    let runtime = SolveRuntime::new(std::sync::Arc::clone(&model)).expect("runtime should prepare");
    assert!(!runtime.algebraic_refresh.causal_solution_certified());
    assert_eq!(
        runtime
            .algebraic_refresh
            .value_projection_plan()
            .blocks
            .len(),
        2
    );
    let mut solver_y = model.initial_y().to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, model.parameters(), 1.0e-12, 8)
        .expect("uncertified seeds should be followed by the complete projection");

    assert!((solver_y[0] - 3.0).abs() <= 1.0e-12);
    assert!((solver_y[1] - 6.0).abs() <= 1.0e-12);
}

#[test]
fn solve_model_rejects_implicit_full_jvp_seed_past_y_and_parameter_domain() {
    let model = uncertified_seed_projection_model();
    let mut fixture = crate::test_support::SolveModelFixture::from_model(model);
    fixture.artifacts.continuous.implicit_jacobian_v_scalar = spanned_block(
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 3 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        "implicit_full_jvp_seed_bounds.mo",
    );

    let error = fixture
        .try_seal()
        .expect_err("a seed beyond the exact [solver Y | parameter] domain must be refused");
    let solve::SolveModelConstructionError::Shape(shape) = *error else {
        panic!("expected an artifact shape error");
    };
    assert_eq!(
        *shape,
        solve::SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context: "artifacts.continuous.implicit_jacobian_v_scalar",
            storage: "seed",
            index: 3,
            extent: 3,
            span: Some(test_span("implicit_full_jvp_seed_bounds.mo")),
        }
    );
}

#[test]
fn refresh_plan_accepts_direct_affine_residual_target() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["T1".to_string(), "T2".to_string(), "q".to_string()],
            ..Default::default()
        },
        state_scalar_count: 2,
        algebraic_scalar_count: 1,
        parameter_count: 1,
        static_parameter_names: vec!["conductance".to_string()],
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let span = test_span("direct_affine_residual.mo")
        .require_provenance("direct affine fixture")
        .expect("fixture span is source-backed");
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_output_indices(
                vec![direct_assignment_residual_row()],
                vec![span.into()],
                vec![2],
            )
            .expect("direct affine program is computable"),
        ),
        implicit_row_targets: vec![None, None, Some(solve::scalar_slot_y(2))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![2],
                y_indices: vec![2],
                tearing: None,
            }],
        },
        derivative_rhs: crate::test_support::zero_derivative_rhs(
            2,
            test_span("direct_affine_constant_states.mo"),
        ),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 3, 1),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("direct-affine fixture satisfies the checked root contract"),
        initial_y: vec![0.0; 3],
        solver_nominals: vec![1.0; 3],
        parameters: vec![0.0],
        ..empty_binary64_first_product_model()
    };
    let block =
        PreparedScalarProgramBlock::from_compute_block(model.problem().continuous().implicit_rhs())
            .expect("valid implicit RHS should prepare");

    let plan = valid_algebraic_refresh_plan(&model);
    let value = block
        .eval_target_assignment_row_with_context(
            0,
            2,
            &[373.15, 273.15, 0.0],
            &[10.0],
            0.0,
            RowEvalContext::default(),
        )
        .expect("direct affine residual should evaluate");

    assert_eq!(plan.rows().len(), 1);
    assert_eq!(plan.rows()[0].source().program(), 0);
    assert_eq!(plan.rows()[0].target_index(), 2);
    assert_eq!(value, Some(-1000.0));
}

#[test]
fn refresh_residual_fallback_solves_positive_unit_coefficient() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 1,
        parameter_count: 1,
        static_parameter_names: vec!["forcing".to_string()],
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::LoadP { dst: 1, index: 0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            "positive_residual.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 1, 1),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("positive-residual fixture satisfies the checked root contract"),
        initial_y: vec![10.0],
        solver_nominals: vec![1.0],
        parameters: vec![0.0],
        ..empty_binary64_first_product_model()
    };
    let model = set_test_implicit_jvp(
        model,
        vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "positive_residual_jvp.mo",
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y().to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[4.0], 1.0e-12, 1)
        .expect("positive-coefficient residual should refresh");

    assert_eq!(solver_y[0], -4.0);
}

fn mode_dependent_repivot_residual_rows() -> Vec<Vec<solve::LinearOp>> {
    vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::Const { dst: 5, value: 1.0 },
            solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::Sub,
                lhs: 4,
                rhs: 5,
            },
            solve::LinearOp::StoreOutput { src: 6 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 3.0 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
    ]
}

fn mode_dependent_repivot_jvp_rows() -> Vec<Vec<solve::LinearOp>> {
    vec![
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 1 },
            solve::LinearOp::LoadSeed { dst: 1, index: 0 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::LoadSeed { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    ]
}

fn mode_dependent_repivot_model() -> solve::SolveModel {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "y".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 2,
        parameter_count: 1,
        static_parameter_names: vec!["mode".to_string()],
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            mode_dependent_repivot_residual_rows(),
            "mode_dependent_repivot.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 1),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("mode-dependent fixture satisfies the checked root contract"),
        initial_y: vec![0.0, 0.0],
        solver_nominals: vec![1.0; 2],
        parameters: vec![0.0],
        ..empty_binary64_first_product_model()
    };
    set_test_implicit_jvp(
        model,
        mode_dependent_repivot_jvp_rows(),
        "mode_dependent_repivot_jvp.mo",
    )
}

#[test]
fn refresh_newton_repivots_mode_dependent_coupled_residuals() {
    // At k=0, row 0 is structurally incident on x but numerically independent
    // of it. The complete Jacobian remains nonsingular.
    let model = mode_dependent_repivot_model();
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    assert!(runtime.algebraic_refresh.rows().is_empty());
    assert_eq!(
        runtime.algebraic_refresh.simultaneous_plan().blocks.len(),
        1
    );

    let mut solver_y = model.initial_y().to_vec();
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[0.0], 1.0e-10, 4)
        .expect("coupled Newton solve should dynamically repivot the residuals");

    assert!((solver_y[0] - 2.0).abs() <= 1.0e-9);
    assert!((solver_y[1] - 1.0).abs() <= 1.0e-9);
}

fn uncertified_causal_seed_jvp_rows() -> Vec<Vec<solve::LinearOp>> {
    vec![
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 1 },
            solve::LinearOp::LoadSeed { dst: 1, index: 2 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::LoadSeed { dst: 3, index: 0 },
            solve::LinearOp::LoadY { dst: 4, index: 0 },
            solve::LinearOp::Binary {
                dst: 5,
                op: solve::BinaryOp::Mul,
                lhs: 4,
                rhs: 4,
            },
            solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::Div,
                lhs: 3,
                rhs: 5,
            },
            solve::LinearOp::Binary {
                dst: 7,
                op: solve::BinaryOp::Add,
                lhs: 2,
                rhs: 6,
            },
            solve::LinearOp::StoreOutput { src: 7 },
        ],
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 2 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::LoadSeed { dst: 2, index: 1 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
    ]
}

fn uncertified_causal_seed_model() -> solve::SolveModel {
    // The causal order can produce a finite epsilon, but it is uncertified and
    // therefore cannot be executed as a value-dependent warm-up before the
    // authoritative residual projection.
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["eps".to_string(), "x".to_string(), "y".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 3,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![
                assignment_residual_for_constant(0, 1.0),
                assignment_residual_with_reciprocal(1, 2, 0),
                scale_and_offset_assignment_residual_row(2, 1, 2.0, 1.0),
            ],
            "causal_newton_seed.mo",
        )),
        implicit_row_targets: vec![
            Some(solve::scalar_slot_y(0)),
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
        ],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1, 2],
                y_indices: vec![0, 1, 2],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 3, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("causal-seed fixture satisfies the checked root contract"),
        initial_y: vec![0.0, 0.0, 0.0],
        solver_nominals: vec![1.0; 3],
        ..empty_binary64_first_product_model()
    };
    set_test_implicit_jvp(
        model,
        uncertified_causal_seed_jvp_rows(),
        "causal_newton_seed_jvp.mo",
    )
}

#[test]
fn uncertified_causal_values_are_not_runtime_seed_executors() {
    // The causal order can produce a finite epsilon, but it is uncertified and
    // therefore cannot be executed as a value-dependent warm-up before the
    // authoritative residual projection.
    let model = uncertified_causal_seed_model();
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    assert!(!runtime.algebraic_refresh.causal_solution_certified());
    let mut solver_y = model.initial_y().to_vec();
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 8)
        .expect("the authoritative residual projection should solve from the incoming coordinate");
    assert!((solver_y[0] - 1.0).abs() <= 1.0e-9);
    assert!((solver_y[1] + 2.0).abs() <= 1.0e-9);
    assert!((solver_y[2] + 3.0).abs() <= 1.0e-9);
}

mod condition_memory_seed;
mod refresh_projection_cases;
mod visibility;

fn non_assignment_targeted_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Binary {
            dst: 1,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        solve::LinearOp::Const { dst: 2, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn parameter_assignment_residual_row(target: usize, parameter: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadP {
            dst: 1,
            index: parameter,
        },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn assignment_residual_for_constant(target: usize, value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::Const { dst: 1, value },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn assignment_residual_with_reciprocal(
    target: usize,
    source: usize,
    denominator: usize,
) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const { dst: 2, value: 1.0 },
        solve::LinearOp::LoadY {
            dst: 3,
            index: denominator,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Div,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

fn sqrt_assignment_residual_row(target: usize, source: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Unary {
            dst: 2,
            op: solve::UnaryOp::Sqrt,
            arg: 1,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn scale_and_offset_assignment_residual_row(
    target: usize,
    source: usize,
    scale: f64,
    offset: f64,
) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const {
            dst: 2,
            value: scale,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Const {
            dst: 4,
            value: offset,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Add,
            lhs: 3,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

fn add_assignment_residual_row(target: usize, source: usize, offset: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const {
            dst: 2,
            value: offset,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn scale_assignment_residual_row(target: usize, source: usize, scale: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const {
            dst: 2,
            value: scale,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn nonfinite_assignment_residual_row(target: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Const { dst: 2, value: 0.0 },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Div,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn shifted_variable_residual_row(index: usize, value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index },
        solve::LinearOp::Const { dst: 1, value },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn bilinear_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::LoadY { dst: 1, index: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadY { dst: 3, index: 2 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn scaled_assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::Unary {
            dst: 1,
            op: solve::UnaryOp::Neg,
            arg: 0,
        },
        solve::LinearOp::LoadP { dst: 2, index: 1 },
        solve::LinearOp::LoadY { dst: 3, index: 1 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Mul,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 4,
        },
        solve::LinearOp::StoreOutput { src: 5 },
    ]
}

fn const_visible_value_row(value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn direct_y_visible_value_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn direct_param_visible_value_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn indexed_param_root_row() -> Vec<solve::LinearOp> {
    vec![
        // A one-based runtime coordinate selects the second scalar from the
        // packed parameter values (p[2] in Modelica notation).
        solve::LinearOp::Const { dst: 0, value: 2.0 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::LoadP { dst: 2, index: 1 },
        solve::LinearOp::LoadIndexedRegister {
            dst: 3,
            base: 1,
            stride: 1,
            dimensions: Box::new([2]),
            indices: Box::new([solve::TensorIndex::Runtime(0)]),
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn direct_time_visible_value_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn param_minus_time_root_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::LoadTime { dst: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn constant_expression_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 2.0 },
        solve::LinearOp::Const { dst: 1, value: 3.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn time_plus_one_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn positive_sum_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::LoadY { dst: 1, index: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn nonlinear_target_residual_row(target: usize, source: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: target,
        },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadY {
            dst: 3,
            index: source,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn derivative_placeholder_row(y_index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: y_index,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn direct_assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 2 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::LoadY { dst: 2, index: 0 },
        solve::LinearOp::LoadY { dst: 3, index: 1 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Mul,
            lhs: 1,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

// Build a state-only-eligible model: state x (slot 0), algebraic a (slot 1),
// with der(x) = a and the projection a = k*x. The exact reduced state
// Jacobian is d(der)/dx = d(a)/dx = k, which a states-only seed would miss
// (it would yield 0) — so this pins the projection forward-sensitivity.

mod projection_output_mapping;
mod projection_sensitivity;
mod slot_updates;
