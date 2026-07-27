//! Unit tests for the solve-lowering stages, exercising the lowering entry
//! points, the inspection probes, and the structural diagnosis through the
//! re-exported stage modules.

use rumoca_core::{BuiltinFunction, Expression, OpBinary, SourceId, Span, Subscript, VarName};
use rumoca_ir_dae as dae;
use rumoca_solver::{SimOptions, SimSolverMode};

use super::diagnostics::{
    EX001_SOLVER_FAILURE, EX002_RUNTIME_PREPARATION, EX003_INVALID_OVERRIDE,
    SIM_RUNTIME_DIAGNOSTIC_CODES, SimulationDiagnosticError,
};
use super::entry::{lower_dae_for_simulation, lower_dae_for_simulation_with_stage_timing};
use super::probe::{eval_dae_at, jacobian_for_dae};
use super::structural_lowering::{
    FunnelCopyBudget, StructurallyLoweredDae, metadata_attachment_lower_error,
    structurally_lower_dae_for_simulation,
};

fn sim_source_span(source: u64, start: usize, end: usize) -> Span {
    let source_name = format!("sim_solve_lowering_source_{source}.mo");
    Span::from_offsets(SourceId::from_source_name(&source_name), start, end)
}

#[test]
fn simulation_structural_lowering_keeps_observations_for_torn_variables() {
    let dae = symbolic_loop_dae();
    let model = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("torn loop should lower to solve IR");

    assert_eq!(model.visible_names, ["a", "b", "c"]);
    assert_eq!(model.visible_value_rows.len(), model.visible_names.len());
    assert_eq!(model.problem.solve_layout.solver_maps.names.len(), 1);
}

#[test]
fn simulation_structural_lowering_restores_shared_observation_computation_as_causal_slot() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        VarName::new("u"),
        dae::Variable::new(VarName::new("u"), fixture_span()),
    );
    for name in ["x", "y"] {
        dae.variables.algebraics.insert(
            VarName::new(name),
            dae::Variable::new(VarName::new(name), fixture_span()),
        );
    }
    dae.continuous
        .equations
        .push(eq(sub(der(var("u")), real(0.0))));
    let shared = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var("u")],
        span: fixture_span(),
    };
    dae.continuous.equations.push(dae::Equation::explicit(
        reference("x"),
        shared.clone(),
        fixture_span(),
        "shared x definition",
    ));
    dae.continuous.equations.push(dae::Equation::explicit(
        reference("y"),
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var("x")),
            rhs: Box::new(var("x")),
            span: fixture_span(),
        },
        fixture_span(),
        "dependent y definition",
    ));

    let lowered = structurally_lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("shared observation computation should lower as a causal DAG");

    assert!(
        lowered
            .dae
            .variables
            .algebraics
            .contains_key(&VarName::new("x"))
    );
    assert!(
        lowered
            .dae
            .variables
            .algebraics
            .contains_key(&VarName::new("y"))
    );
    let restored = lowered
        .dae
        .continuous
        .equations
        .iter()
        .filter(|equation| equation.origin == "causal reconstruction after structural elimination")
        .collect::<Vec<_>>();
    assert_eq!(restored.len(), 2);
    assert!(rumoca_core::expressions_semantically_equal(
        &restored[0].rhs,
        &shared
    ));
    assert!(rumoca_core::expressions_semantically_equal(
        &restored[1].rhs,
        &Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var("x")),
            rhs: Box::new(var("x")),
            span: fixture_span(),
        }
    ));
    let visible_y = lowered
        .visible_expressions
        .iter()
        .find(|visible| visible.name == "y")
        .expect("y remains observable");
    let expected_y = var("y");
    assert!(rumoca_core::expressions_semantically_equal(
        &visible_y.expr,
        &expected_y
    ));
}

#[test]
fn simulation_structural_lowering_reports_blt_singularity() {
    let mut dae = dae::Dae::new();
    dae.variables.algebraics.insert(
        VarName::new("a"),
        dae::Variable::new(
            VarName::new("a"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.variables.algebraics.insert(
        VarName::new("b"),
        dae::Variable::new(
            VarName::new("b"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var("a")),
            rhs: Box::new(var("b")),
            span: fixture_span(),
        },
        span: fixture_span(),
        origin: "singular test".to_string(),
        scalar_count: 1,
    });

    let mut dae = dae;
    rumoca_phase_dae::attach_dae_reference_metadata(&mut dae)
        .expect("fixture DAE reference metadata should normalize");
    let err = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect_err("BLT singularity must not be silently skipped");

    assert!(
        err.to_string().contains("structural lowering failed"),
        "got: {err}"
    );
    assert!(err.to_string().contains("structurally singular system"));
    // SPEC_0008: a structural failure keeps its own ES0xx code all the way to
    // the user-facing sink instead of being relabeled by the sim facade.
    assert_eq!(err.code(), "ES010");
    assert_eq!(
        SimulationDiagnosticError::SolveLowering(err).diagnostic_code(),
        "ES010"
    );
}

#[test]
fn metadata_attachment_lower_error_preserves_dae_source_span() {
    let span = sim_source_span(9, 21, 34);
    let err = metadata_attachment_lower_error(
        rumoca_phase_dae::ToDaeError::runtime_metadata_violation_at(
            "missing reference metadata",
            span,
        ),
    );

    assert_eq!(err.source_span(), Some(span));
    assert!(
        matches!(
            err,
            rumoca_phase_solve::SolveModelLowerError::Lower(
                rumoca_phase_solve::lower::LowerError::ContractViolation {
                    span: actual,
                    ..
                }
            ) if actual == span
        ),
        "metadata attachment error should preserve the DAE error span"
    );
}

#[test]
fn metadata_attachment_lower_error_keeps_unspanned_dae_error_unspanned() {
    let err = metadata_attachment_lower_error(
        rumoca_phase_dae::ToDaeError::runtime_metadata_violation("metadata-only corruption"),
    );

    assert_eq!(err.source_span(), None);
    assert!(
        matches!(
            err,
            rumoca_phase_solve::SolveModelLowerError::Lower(
                rumoca_phase_solve::lower::LowerError::UnspannedContractViolation { .. }
            )
        ),
        "metadata-only error must not receive fabricated provenance"
    );
}

#[test]
fn simulation_structural_singularity_carries_unmatched_variable_span() {
    let span = sim_source_span(7, 100, 110);
    let mut dae = dae::Dae::new();
    for name in ["a", "b"] {
        dae.variables.algebraics.insert(
            VarName::new(name),
            dae::Variable {
                source_span: span,
                ..dae::Variable::new(
                    VarName::new(name),
                    rumoca_core::Span::from_offsets(
                        rumoca_core::SourceId::from_source_name(file!()),
                        1,
                        2,
                    ),
                )
            },
        );
    }
    // One equation (`0 = a + b`), two unknowns -> structurally singular
    // (an additive constraint cannot be alias-eliminated).
    dae.continuous.equations.push(eq(Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(var("a")),
        rhs: Box::new(var("b")),
        span: fixture_span(),
    }));

    let mut dae = dae;
    rumoca_phase_dae::attach_dae_reference_metadata(&mut dae)
        .expect("fixture DAE reference metadata should normalize");
    let err = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect_err("singular system should error");
    assert_eq!(
        err.source_span(),
        Some(span),
        "structural singularity should carry the unmatched variable span: {err:?}"
    );
}

#[test]
fn simulation_structural_lowering_demotes_unresolved_derivative_alias_state() {
    let mut dae = derivative_alias_state_dae();
    rumoca_phase_dae::attach_dae_reference_metadata(&mut dae)
        .expect("fixture DAE reference metadata should normalize");
    let model = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("derivative alias state should lower without an underdetermined solver slot");

    assert_eq!(model.state_scalar_count(), 0);
    assert!(
        !model
            .problem
            .solve_layout
            .solver_maps
            .names
            .contains(&"dx".to_string())
    );
}

#[test]
fn simulation_structural_lowering_keeps_cross_coupled_ode_states() {
    let dae = oscillator_dae();
    let model = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("cross-coupled ODE states should lower");

    assert_eq!(model.state_scalar_count(), 2);
    assert_eq!(model.problem.solve_layout.solver_maps.names, ["x", "v"]);
}

#[test]
fn simulation_direct_lowering_accepts_state_only_ode() {
    let dae = state_only_ode_dae();
    let opts = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        ..Default::default()
    };
    let mut stages = Vec::new();
    let (model, timings) =
        lower_dae_for_simulation_with_stage_timing(&dae, &opts, |stage| stages.push(stage))
            .expect("state-only ODE should lower directly");

    assert_eq!(timings.structural_dae_seconds, 0.0);
    assert_eq!(model.state_scalar_count(), 1);
    assert_eq!(model.problem.solve_layout.algebraic_scalar_count(), 0);
    assert!(stages.contains(&"ir_solve_direct"));
    assert!(!stages.contains(&"ir_solve_structural_dae"));
}

#[test]
fn simulation_direct_lowering_falls_back_for_projected_derivative_dependency() {
    let dae = explicit_algebraic_ode_dae();
    let opts = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        ..Default::default()
    };
    let mut stages = Vec::new();
    let (model, _) =
        lower_dae_for_simulation_with_stage_timing(&dae, &opts, |stage| stages.push(stage))
            .expect("algebraic derivative dependency should structurally lower for now");

    assert_eq!(model.state_scalar_count(), 1);
    assert!(stages.contains(&"ir_solve_direct"));
    assert!(stages.contains(&"ir_solve_structural_dae"));
}

#[test]
fn simulation_direct_lowering_falls_back_for_state_selection() {
    let dae = constrained_state_dae();
    let opts = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        ..Default::default()
    };
    let mut stages = Vec::new();
    let (model, _) =
        lower_dae_for_simulation_with_stage_timing(&dae, &opts, |stage| stages.push(stage))
            .expect("constrained state model should fall back to structural lowering");

    assert_eq!(model.state_scalar_count(), 2);
    assert!(stages.contains(&"ir_solve_direct"));
    assert!(stages.contains(&"ir_solve_structural_dae"));
}

#[test]
fn simulation_state_selection_prefers_physical_coordinates_for_conservation_states() {
    let mut dae = preferred_conservation_state_dae();
    dae.variables
        .states
        .get_mut(&VarName::new("mass"))
        .expect("mass fixture state exists")
        .fixed = Some(true);
    dae.variables
        .algebraics
        .get_mut(&VarName::new("level"))
        .expect("level fixture algebraic exists")
        .fixed = Some(false);
    dae.initialization.equations.push(dae::Equation {
        origin: "fixed start initialization for mass".to_string(),
        ..eq(sub(var("mass"), real(1.0)))
    });
    let lowered = structurally_lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("preferred physical coordinates should structurally lower");

    assert_eq!(lowered.dae.variables.states.len(), 2);
    for name in ["level", "temperature"] {
        assert!(
            lowered
                .dae
                .variables
                .states
                .contains_key(&VarName::new(name)),
            "preferred physical coordinate `{name}` should be selected as a state"
        );
    }
    for name in ["mass", "energy"] {
        assert!(
            !lowered
                .dae
                .variables
                .states
                .contains_key(&VarName::new(name)),
            "conserved quantity `{name}` should not remain an independent state"
        );
    }
    assert_eq!(
        lowered
            .dae
            .variables
            .algebraics
            .get(&VarName::new("mass"))
            .and_then(|variable| variable.fixed),
        Some(true)
    );
    assert_eq!(
        lowered
            .dae
            .variables
            .states
            .get(&VarName::new("level"))
            .and_then(|variable| variable.fixed),
        Some(false)
    );
    assert_eq!(lowered.dae.initialization.equations.len(), 1);
    assert_eq!(
        lowered.dae.initialization.equations[0].origin,
        "fixed start initialization for mass"
    );
}

#[test]
fn simulation_structural_lowering_demotes_vector_state_with_only_alias_rows() {
    let dae = vector_alias_state_dae();
    let lowered = structurally_lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("vector alias state should structurally lower");

    assert!(
        !lowered
            .dae
            .variables
            .states
            .contains_key(&VarName::new("imc.is")),
        "vector alias state without retained derivative rows should be demoted"
    );
    assert!(
        lowered
            .dae
            .variables
            .algebraics
            .contains_key(&VarName::new("imc.is"))
    );
}

#[test]
fn simulation_structural_lowering_keeps_metadata_equations_compact() {
    let mut dae = dae::Dae::new();
    dae.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(VarName::new("x"), fixture_span()),
    );
    let mut values = dae::Variable::new(VarName::new("values"), fixture_span());
    values.dims = vec![2];
    dae.variables
        .algebraics
        .insert(VarName::new("values"), values);
    dae.continuous.equations.push(dae::Equation {
        origin: "binding equation for values".to_string(),
        ..eq_with_scalar_count(sub(var("values"), array(vec![real(1.0), real(2.0)])), 2)
    });
    dae.continuous
        .equations
        .push(eq(sub(der(var("x")), time())));

    let lowered = structurally_lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("compact metadata must not participate in solve scalarization");

    let binding = lowered
        .metadata_dae
        .continuous
        .equations
        .iter()
        .find(|equation| equation.origin == "binding equation for values")
        .expect("metadata retains the source binding");
    assert_eq!(binding.scalar_count, 2);
    assert!(matches!(
        &binding.rhs,
        Expression::Binary { rhs, .. } if matches!(rhs.as_ref(), Expression::Array { elements, .. } if elements.len() == 2)
    ));
}

#[test]
fn simulation_structural_lowering_differentiates_vector_function_constraint_for_coupled_state() {
    let dae = quaternion_constraint_dae();
    let model = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("vector function constraint should provide the missing coupled state row");

    assert_eq!(model.state_scalar_count(), 4);
    assert!(["Q[1]", "Q[2]", "Q[3]", "Q[4]"].iter().all(|name| {
        model
            .problem
            .solve_layout
            .solver_maps
            .names
            .contains(&name.to_string())
    }));
}

#[test]
fn simulation_structural_lowering_reports_state_metadata_before_elimination() {
    let dae = exact_alias_state_dae();
    let model = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("exact alias state model should lower");

    let x_meta = model
        .variable_meta
        .iter()
        .find(|meta| meta.name == "x")
        .expect("x should remain visible");
    let y_meta = model
        .variable_meta
        .iter()
        .find(|meta| meta.name == "y")
        .expect("y should remain visible");

    let selected_count = usize::from(x_meta.is_state) + usize::from(y_meta.is_state);
    assert_eq!(
        selected_count, 1,
        "exact alias component should report one selected state"
    );
}

#[test]
fn simulation_metadata_reports_constrained_state_as_unselected() {
    let dae = constrained_state_dae();
    let model = lower_dae_for_simulation(&dae, &SimOptions::default())
        .expect("direct constrained state model should lower");

    let x1_meta = model
        .variable_meta
        .iter()
        .find(|meta| meta.name == "x1")
        .expect("x1 should remain visible");
    let x2_meta = model
        .variable_meta
        .iter()
        .find(|meta| meta.name == "x2")
        .expect("x2 should remain visible");
    let x3_meta = model
        .variable_meta
        .iter()
        .find(|meta| meta.name == "x3")
        .expect("x3 should remain visible");

    assert_eq!(model.state_scalar_count(), 2);
    assert!(!x1_meta.is_state);
    assert_eq!(x1_meta.role, "algebraic");
    assert!(x2_meta.is_state);
    assert!(x3_meta.is_state);
}

#[test]
fn simulation_lowering_preserves_source_span_for_shape_errors() {
    let mut model = dae::Dae::new();
    model.variables.algebraics.insert(
        VarName::new("A"),
        dae::Variable {
            dims: vec![3, 3],
            ..dae::Variable::new(
                VarName::new("A"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            )
        },
    );
    model.variables.algebraics.insert(
        VarName::new("b"),
        dae::Variable {
            dims: vec![2],
            ..dae::Variable::new(
                VarName::new("b"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            )
        },
    );
    let span = sim_source_span(4, 40, 45);
    let rhs = sub(var("A").with_span(span), var("b").with_span(span));
    model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs,
        span,
        origin: "shape mismatch".to_string(),
        scalar_count: 9,
    });

    let mut model = model;
    rumoca_phase_dae::attach_dae_reference_metadata(&mut model)
        .expect("fixture DAE reference metadata should normalize");
    let err = lower_dae_for_simulation(&model, &SimOptions::default())
        .expect_err("shape mismatch should fail during simulation lowering");
    assert_eq!(err.source_span(), Some(span), "unexpected error: {err:?}");
    let diagnostic = SimulationDiagnosticError::SolveLowering(err);
    // SPEC_0008: the lowering defect keeps the code of the phase that raised
    // it (EL001, unsupported expression) instead of the old generic
    // "this came from lowering" label.
    assert_eq!(diagnostic.diagnostic_code(), "EL001");
    assert_eq!(
        diagnostic.diagnostic_label(),
        "array operands have incompatible shapes [3, 3] and [2]"
    );
}

#[test]
fn solver_and_override_errors_have_stable_runtime_codes() {
    let solver = SimulationDiagnosticError::Solver("CVODE returned -1".to_string());
    let override_error = SimulationDiagnosticError::InvalidOverride {
        message: "unknown parameter `k`".to_string(),
    };

    assert_eq!(solver.diagnostic_code(), EX001_SOLVER_FAILURE);
    assert_eq!(override_error.diagnostic_code(), EX003_INVALID_OVERRIDE);
    for code in [solver.diagnostic_code(), override_error.diagnostic_code()] {
        assert!(
            SIM_RUNTIME_DIAGNOSTIC_CODES.contains(&code),
            "runtime code {code} is not registered in SIM_RUNTIME_DIAGNOSTIC_CODES"
        );
    }
}

#[test]
fn simulation_diagnostic_preserves_runtime_preparation_span() {
    let span = sim_source_span(8, 12, 18);
    let error = rumoca_eval_solve::EvalSolveError::Scalarization {
        message: "invalid native map metadata".to_string(),
        span: Some(span),
    };
    let diagnostic = SimulationDiagnosticError::from(error);

    assert_eq!(diagnostic.diagnostic_code(), EX002_RUNTIME_PREPARATION);
    assert!(SIM_RUNTIME_DIAGNOSTIC_CODES.contains(&diagnostic.diagnostic_code()));
    assert_eq!(diagnostic.source_span(), Some(span));
    assert_eq!(
        diagnostic.to_string(),
        "Solve-IR scalarization failed: invalid native map metadata"
    );
}

fn visible_expression_names(lowered: &StructurallyLoweredDae) -> Vec<String> {
    lowered
        .visible_expressions
        .iter()
        .map(|visible| visible.name.clone())
        .collect()
}

/// The observable inventory is declaration-based, so expanding the solver view
/// into scalar rows must not change a single visible name or expression. The
/// structural funnel relies on exactly this to resolve the visible expressions
/// straight off the borrowed source DAE instead of scalarizing a private copy
/// of it.
///
/// Both lowerings are checked against the same *independently written*
/// expectation rather than against each other: `opts` no longer has any data
/// path to `visible_expressions`, so comparing the two runs to each other could
/// not fail whatever the inventory produced.
#[test]
fn visible_expressions_are_independent_of_solver_scalarization() {
    let dae = vector_observation_dae();
    // `y` is an array algebraic that survives elimination, so the inventory
    // expands it from its own declared `dims` into two component references
    // and leaves them as references — it never reads the defining row.
    let expected: [(&str, Expression); 3] = [
        ("x", var("x")),
        ("y[1]", var_idx("y", 1)),
        ("y[2]", var_idx("y", 2)),
    ];

    for scalarize in [true, false] {
        let lowered = structurally_lower_dae_for_simulation(
            &dae,
            &SimOptions {
                scalarize,
                ..SimOptions::default()
            },
        )
        .expect("array observation model lowers");

        assert_eq!(
            visible_expression_names(&lowered),
            ["x", "y[1]", "y[2]"],
            "observable names with scalarize={scalarize}"
        );
        for (visible, (name, expected_expr)) in lowered.visible_expressions.iter().zip(&expected) {
            assert_eq!(&visible.name, name);
            assert!(
                rumoca_core::expressions_semantically_equal(&visible.expr, expected_expr),
                "observation `{name}` with scalarize={scalarize} was `{}`",
                super::expr_util::debug_render_expr(&visible.expr)
            );
        }
    }
}

/// Ratchet on the two dominant costs of the structural funnel.
///
/// `copy_budget` is measured, not declared: every whole-DAE copy the funnel
/// reaches records itself, so this pins what the funnel *does*, not what this
/// module remembers doing. For both fixtures the nine copies are
///
/// 1. `prepare_structural_daes` — the source DAE,
/// 2. `prepare_structural_daes` — the solver view copied off the source,
/// 3. `dae_prepare::index_reduce_missing_state_derivatives` — staging copy,
/// 4. `prepare_structural_daes` — the metadata DAE copied off the solver view,
/// 5. `eliminate::prepare_blt_elimination` — the BLT sort input,
/// 6. `dae_prepare::demote_states_without_assignable_derivative_rows` — staging
///    copy, from `apply_simulation_elimination`,
/// 7. `mark_state_selection_metadata` — the state-selection scratch copy,
/// 8. `dae_prepare::demote_states_without_assignable_derivative_rows` — staging
///    copy again, this time on that scratch copy,
/// 9. `eliminate::condense_scalar_algebraic_loops` — its own BLT sort input.
///
/// Only four of those are made by the funnel itself, so a budget that counted
/// this module's copy sites alone would report less than half the real cost.
/// Copy 9 is made in a module the accounting does not instrument and is
/// recorded by the funnel against the same acceptance condition — hence the
/// unscalarized array view, whose array-shaped rows make loop condensation
/// decline before it copies anything, is one lower.
#[test]
fn structural_funnel_copy_budget_is_ratcheted() {
    let expected = FunnelCopyBudget {
        dae_clones: 9,
        scalarizations: 1,
    };
    let scalar_model = structurally_lower_dae_for_simulation(
        &explicit_algebraic_ode_dae(),
        &SimOptions::default(),
    )
    .expect("scalar funnel fixture lowers");
    assert_eq!(
        scalar_model.copy_budget, expected,
        "scalar model funnel copy budget"
    );
    let array_model =
        structurally_lower_dae_for_simulation(&vector_observation_dae(), &SimOptions::default())
            .expect("array funnel fixture lowers");
    assert_eq!(
        array_model.copy_budget, expected,
        "array model funnel copy budget"
    );

    let unscalarized = structurally_lower_dae_for_simulation(
        &vector_observation_dae(),
        &SimOptions {
            scalarize: false,
            ..SimOptions::default()
        },
    )
    .expect("array funnel fixture lowers without scalarization");
    assert_eq!(
        unscalarized.copy_budget,
        FunnelCopyBudget {
            dae_clones: 8,
            scalarizations: 0,
        },
        "unscalarized funnel copy budget"
    );
}

/// The funnel records the whole-DAE copy that
/// `condense_scalar_algebraic_loops` makes internally, because that module is
/// outside the copy accounting. It records it against the same condition the
/// condensation itself uses to decide whether to copy: an array-shaped solver
/// view is declined before anything is copied. Pin that boundary here, so the
/// mirrored condition cannot drift away from the behaviour it mirrors without
/// a test failing.
#[test]
fn array_shaped_solver_view_declines_loop_condensation() {
    let mut array_view = vector_observation_dae();
    assert!(
        !super::structural_lowering::solver_view_is_fully_scalar(&array_view),
        "fixture must present an array-shaped continuous row"
    );

    let condensation =
        rumoca_phase_structural::eliminate::condense_scalar_algebraic_loops(&mut array_view)
            .expect("declining an array-shaped view is not an error");

    assert!(
        condensation.blocks.is_empty() && condensation.causal_variable_count() == 0,
        "an array-shaped solver view must be declined, not condensed"
    );
    assert_eq!(
        array_view.continuous.equations.len(),
        vector_observation_dae().continuous.equations.len(),
        "a declined view must be left unchanged"
    );
}

/// The funnel resolves visible expressions against the borrowed source DAE and
/// then releases both the source DAE and the causal plan before the metadata
/// partition is rewritten. Pin that substitution resolution still happens on
/// that reordered path: in the `a = b`, `b = c`, `c = sin(a)` torn loop only
/// `c` survives as a solver unknown, so the observations for `a` and `b` must
/// come back rewritten in terms of `c` rather than as bare self-references.
#[test]
fn visible_expressions_resolve_eliminated_aliases_after_source_release() {
    let lowered =
        structurally_lower_dae_for_simulation(&symbolic_loop_dae(), &SimOptions::default())
            .expect("torn loop fixture lowers");

    assert_eq!(visible_expression_names(&lowered), ["a", "b", "c"]);
    assert_eq!(lowered.dae.continuous.equations.len(), 1);
    for visible in &lowered.visible_expressions {
        assert!(
            rumoca_core::expressions_semantically_equal(&visible.expr, &var("c")),
            "observation `{}` was not resolved through the elimination substitutions",
            visible.name
        );
    }
}

/// One scalar state plus a two-element array algebraic defined by a single
/// array row, so the solver view genuinely differs between the scalarized and
/// unscalarized lowerings while the observable inventory must not.
fn vector_observation_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(VarName::new("x"), fixture_span()),
    );
    model.variables.algebraics.insert(
        VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..dae::Variable::new(VarName::new("y"), fixture_span())
        },
    );
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), time())));
    model.continuous.equations.push(eq_with_scalar_count(
        sub(var("y"), array(vec![var("x"), mul(real(2.0), var("x"))])),
        2,
    ));
    model
}

fn symbolic_loop_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    for name in ["a", "b", "c"] {
        model.variables.algebraics.insert(
            VarName::new(name),
            dae::Variable::new(
                VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }
    model.continuous.equations.push(eq(sub(var("a"), var("b"))));
    model.continuous.equations.push(eq(sub(var("b"), var("c"))));
    model.continuous.equations.push(eq(sub(
        var("c"),
        Expression::BuiltinCall {
            function: BuiltinFunction::Sin,
            args: vec![var("a")],
            span: fixture_span(),
        },
    )));
    model
}

fn derivative_alias_state_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    model.variables.algebraics.insert(
        VarName::new("dx"),
        dae::Variable::new(
            VarName::new("dx"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    model.continuous.equations.push(eq(sub(var("x"), time())));
    model
        .continuous
        .equations
        .push(eq(sub(var("dx"), der(var("x")))));
    model
}

fn oscillator_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    for name in ["x", "v"] {
        model.variables.states.insert(
            VarName::new(name),
            dae::Variable::new(
                VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), var("v"))));
    model.continuous.equations.push(eq(sub(
        der(var("v")),
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: Box::new(var("x")),
            span: fixture_span(),
        },
    )));
    model
}

fn exact_alias_state_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    for name in ["x", "y"] {
        model.variables.states.insert(
            VarName::new(name),
            dae::Variable::new(
                VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }
    model.variables.algebraics.insert(
        VarName::new("a"),
        dae::Variable::new(
            VarName::new("a"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    model.continuous.equations.push(eq(sub(var("x"), var("a"))));
    model.continuous.equations.push(eq(sub(var("y"), var("a"))));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), time())));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("y")), time())));
    model
}

fn vector_alias_state_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("imc.is"),
        dae::Variable {
            dims: vec![3],
            ..dae::Variable::new(
                VarName::new("imc.is"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            )
        },
    );
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    for idx in 1..=3 {
        model.variables.algebraics.insert(
            VarName::new(format!("imc.plug_sp.pin[{idx}].i")),
            dae::Variable::new(
                VarName::new(format!("imc.plug_sp.pin[{idx}].i")),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        model.continuous.equations.push(eq(sub(
            var_idx("imc.is", idx),
            var(&format!("imc.plug_sp.pin[{idx}].i")),
        )));
    }
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), time())));
    model
}

fn constrained_state_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    for name in ["x1", "x2", "x3"] {
        model.variables.states.insert(
            VarName::new(name),
            dae::Variable::new(
                VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }
    model.variables.algebraics.insert(
        VarName::new("a"),
        dae::Variable::new(
            VarName::new("a"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    model
        .continuous
        .equations
        .push(eq(sub(var("a"), sub(neg(var("x2")), var("x3")))));
    model
        .continuous
        .equations
        .push(eq(sub(var("x1"), var("a"))));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x1")), sub(neg(time()), time()))));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x2")), time())));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x3")), time())));
    model
}

fn preferred_conservation_state_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    for name in ["mass", "energy"] {
        model.variables.states.insert(
            VarName::new(name),
            dae::Variable::new(VarName::new(name), fixture_span()),
        );
    }
    for name in ["level", "temperature"] {
        model.variables.algebraics.insert(
            VarName::new(name),
            dae::Variable {
                state_select: rumoca_core::StateSelect::Prefer,
                ..dae::Variable::new(VarName::new(name), fixture_span())
            },
        );
    }

    model.continuous.equations.push(eq(sub(
        var("mass"),
        Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(mul(real(2.0), var("level"))),
            rhs: Box::new(real(1.0)),
            span: fixture_span(),
        },
    )));
    model.continuous.equations.push(eq(sub(
        var("energy"),
        mul(
            var("mass"),
            mul(real(3.0), sub(var("temperature"), real(273.15))),
        ),
    )));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("mass")), real(1.0))));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("energy")), real(2.0))));
    model
}

fn explicit_algebraic_ode_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    model.variables.algebraics.insert(
        VarName::new("a"),
        dae::Variable::new(
            VarName::new("a"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    model
        .continuous
        .equations
        .push(eq(sub(var("a"), mul(real(2.0), var("x")))));
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), var("a"))));
    model
}

fn state_only_ode_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(
            VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), mul(real(2.0), var("x")))));
    model
}

fn quaternion_constraint_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("Q"),
        dae::Variable {
            dims: vec![4],
            ..dae::Variable::new(
                VarName::new("Q"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            )
        },
    );
    model.symbols.functions.insert(
        VarName::new("orientationConstraint"),
        orientation_constraint_function(),
    );
    for idx in 1..=3 {
        model
            .continuous
            .equations
            .push(eq(sub(der(var_idx("Q", idx)), time())));
    }
    model.continuous.equations.push(eq(sub(
        array(vec![int(0)]),
        call("orientationConstraint", vec![var("Q")]),
    )));
    model
}

fn orientation_constraint_function() -> rumoca_core::Function {
    let span = fixture_span();
    let mut function = rumoca_core::Function::new("orientationConstraint", span);
    function.instance_id = Some(orientation_constraint_instance_id());
    function
        .inputs
        .push(rumoca_core::FunctionParam::new("Q", "Orientation", span));
    let mut output = rumoca_core::FunctionParam::new("residue", "Real", span);
    output.dims = vec![1];
    function.outputs.push(output);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "residue".to_string(),
                span,
                subs: Vec::new(),
            }],
            def_id: None,
        },
        value: array(vec![sub(mul(var("Q"), var("Q")), int(1))]),
        span,
    });
    function
}

fn fixture_span() -> Span {
    sim_source_span(10_001, 1, 2)
}

fn eq(rhs: Expression) -> dae::Equation {
    eq_with_scalar_count(rhs, 1)
}

fn eq_with_scalar_count(rhs: Expression, scalar_count: usize) -> dae::Equation {
    dae::Equation {
        lhs: None,
        rhs,
        span: fixture_span(),
        origin: "test".to_string(),
        scalar_count,
    }
}

fn sub(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: fixture_span(),
    }
}

fn mul(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: fixture_span(),
    }
}

fn neg(rhs: Expression) -> Expression {
    Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: Box::new(rhs),
        span: fixture_span(),
    }
}

fn array(elements: Vec<Expression>) -> Expression {
    Expression::Array {
        elements,
        is_matrix: false,
        span: fixture_span(),
    }
}

fn call(name: &str, args: Vec<Expression>) -> Expression {
    Expression::FunctionCall {
        name: reference(name).with_resolved_function(rumoca_core::ResolvedFunctionReference {
            instance_id: orientation_constraint_instance_id(),
            base_part_count: 1,
        }),
        args,
        is_constructor: false,
        span: fixture_span(),
    }
}

fn orientation_constraint_instance_id() -> rumoca_core::FunctionInstanceId {
    rumoca_core::FunctionInstanceId::new(1)
}

fn component_ref(name: &str) -> rumoca_core::ComponentReference {
    let span = fixture_span();
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
        }],
        def_id: None,
    }
}

fn reference(name: &str) -> rumoca_core::Reference {
    rumoca_core::Reference::with_component_reference(name, component_ref(name))
}

fn int(value: i64) -> Expression {
    Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: fixture_span(),
    }
}

fn var(name: &str) -> Expression {
    let span = fixture_span();
    Expression::VarRef {
        name: reference(name),
        subscripts: Vec::new(),
        span,
    }
}

fn var_idx(name: &str, idx: i64) -> Expression {
    let span = fixture_span();
    Expression::VarRef {
        name: reference(name),
        subscripts: vec![Subscript::generated_index(idx, span)],
        span,
    }
}

fn time() -> Expression {
    var("time")
}

fn der(arg: Expression) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![arg],
        span: fixture_span(),
    }
}

fn real(value: f64) -> Expression {
    Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: fixture_span(),
    }
}

fn div(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: fixture_span(),
    }
}

#[test]
fn eval_dae_at_names_nonfinite_state_derivative() {
    // der(x) = 1 / y ; der(y) = -1. At y = 0 the first derivative is inf,
    // and the probe must name it so a NaN/inf is one command away.
    let mut dae = dae::Dae::new();
    for name in ["x", "y"] {
        dae.variables.states.insert(
            VarName::new(name),
            dae::Variable::new(
                VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }
    dae.continuous
        .equations
        .push(eq(sub(der(var("x")), div(real(1.0), var("y")))));
    dae.continuous
        .equations
        .push(eq(sub(der(var("y")), real(-1.0))));

    // Override the state `y` by name (positional ordering is never used).
    let probe = eval_dae_at(&dae, &SimOptions::default(), &[("y".to_string(), 0.0)], 0.5)
        .expect("model should lower and evaluate");
    let report = &probe.report;

    assert_eq!(report.state_count, 2);
    assert_eq!(probe.state_names, vec!["x".to_string(), "y".to_string()]);
    let y_index = probe.state_names.iter().position(|s| s == "y").unwrap();
    assert_eq!(probe.state_used[y_index], 0.0);

    let der_x = report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(x)")
        .expect("der(x) present");
    assert!(!der_x.is_finite(), "der(x)=1/0 should be non-finite");

    assert!(report.has_nonfinite());
    let nonfinite_names: Vec<_> = report
        .nonfinite()
        .map(|(_, slot)| slot.name.clone())
        .collect();
    assert!(
        nonfinite_names.iter().any(|name| name == "der(x)"),
        "non-finite report should name der(x): {nonfinite_names:?}"
    );

    let der_y = report
        .derivatives
        .iter()
        .find(|slot| slot.name == "der(y)")
        .expect("der(y) present");
    assert!(der_y.is_finite(), "der(y) should stay finite");
}

#[test]
fn jacobian_for_dae_assembles_named_matrix_and_flags_zero_pivots() {
    // Oscillator der(x)=v, der(v)=-x -> J = [[0,1],[-1,0]]: both diagonal
    // pivots are zero, no structurally-singular columns.
    let probe = jacobian_for_dae(&oscillator_dae(), &SimOptions::default(), &[], 0.0)
        .expect("oscillator jacobian should assemble");
    let report = &probe.report;

    assert_eq!(report.dim(), 2);
    assert_eq!(probe.state_names, vec!["x".to_string(), "v".to_string()]);
    assert!(
        report.singular_columns().is_empty(),
        "both states affect a derivative"
    );
    assert_eq!(
        report.zero_pivots(),
        vec![0, 1],
        "d(der(x))/dx and d(der(v))/dv are both zero"
    );
    // Off-diagonal structure: d(der(x))/dv = 1, d(der(v))/dx = -1.
    let entries: std::collections::HashMap<(usize, usize), f64> = report
        .nonzero_entries()
        .map(|(r, c, v)| ((r, c), v))
        .collect();
    assert!((entries[&(0, 1)] - 1.0).abs() < 1e-4, "{entries:?}");
    assert!((entries[&(1, 0)] + 1.0).abs() < 1e-4, "{entries:?}");
    assert!(report.error.is_none());
}

#[test]
fn eval_dae_at_rejects_unknown_state_name() {
    let err = eval_dae_at(
        &oscillator_dae(),
        &SimOptions::default(),
        &[("nope".to_string(), 1.0)],
        0.0,
    )
    .expect_err("unknown state name should error");
    let message = err.to_string();
    assert!(message.contains("`nope` is not a state"), "{message}");
    assert!(message.contains('x') && message.contains('v'), "{message}");
}

#[test]
fn eval_dae_at_reports_finite_values_from_initial_state() {
    // No overrides: states keep their model initial value (here 0).
    let probe = eval_dae_at(&oscillator_dae(), &SimOptions::default(), &[], 0.0)
        .expect("oscillator should lower and evaluate");
    let report = &probe.report;

    assert_eq!(report.state_count, 2);
    assert!(!report.has_nonfinite());
    assert!(report.error.is_none());
    // der(x) = v, der(v) = -x; at the zero initial state both are 0.
    let der_x = report
        .derivatives
        .iter()
        .find(|s| s.name == "der(x)")
        .unwrap();
    let der_v = report
        .derivatives
        .iter()
        .find(|s| s.name == "der(v)")
        .unwrap();
    assert_eq!(der_x.value, 0.0);
    assert_eq!(der_v.value, 0.0);
}

/// A model whose *source* view cannot be scalarized while its solver view can.
///
/// `y` is a two-element algebraic with no source provenance, constrained twice
/// by the same array row: once written with a spanned reference and once with
/// an unspanned one. Projecting an array reference into scalar components needs
/// a span, and for the duplicate there is none on the expression, the
/// reference or the variable — so the source view cannot be scalarized. The
/// solver view can, because `remove_duplicate_continuous_equations` deletes the
/// duplicate (equation identity ignores spans) and keeps the spanned row.
/// (These are the only `Span::DUMMY` fixtures in this file: modelling *absent*
/// provenance is their entire point.)
fn unscalarizable_source_row_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(VarName::new("x"), fixture_span()),
    );
    model.variables.algebraics.insert(
        VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            source_span: Span::DUMMY,
            ..dae::Variable::new(VarName::new("y"), fixture_span())
        },
    );
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), real(1.0))));
    // `sin(y)` is not invertible for `y`, so the boundary phase cannot
    // eliminate the row: it survives into the scalarized solver view, which is
    // what makes the *duplicate* the only difference between the two views.
    let definition = |reference: Expression| {
        sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Sin,
                args: vec![reference],
                span: fixture_span(),
            },
            array(vec![real(1.0), real(2.0)]),
        )
    };
    model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: definition(var("y")),
        span: fixture_span(),
        origin: "spanned array row".to_string(),
        scalar_count: 2,
    });
    model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: definition(Expression::VarRef {
            name: rumoca_core::Reference::new("y"),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }),
        span: Span::DUMMY,
        origin: "unspanned duplicate array row".to_string(),
        scalar_count: 2,
    });
    model
}

/// The funnel used to scalarize a private copy of the source DAE before reading
/// the observable inventory off it, and then throw that copy away. The copy is
/// gone, and with it a whole class of spurious rejections: a model must not be
/// refused because a DAE nobody reads failed to scalarize.
///
/// This pins that deliberately. The first assertion proves the fixture really
/// is one the deleted pass would have rejected; the second proves the funnel
/// accepts it anyway, because the row that cannot be scalarized is eliminated
/// before the view that *is* scalarized is built.
#[test]
fn source_view_scalarization_failure_no_longer_rejects_the_model() {
    let mut source_view = unscalarizable_source_row_dae();
    let source_error = rumoca_phase_structural::scalarize::scalarize_equations(&mut source_view)
        .expect_err("the fixture's source view must be one that cannot be scalarized");
    assert!(
        source_error.to_string().contains("without a source span"),
        "unexpected source scalarization failure: {source_error}"
    );

    let lowered = structurally_lower_dae_for_simulation(
        &unscalarizable_source_row_dae(),
        &SimOptions::default(),
    )
    .expect("the solver view scalarizes even though the source view does not");

    assert_eq!(visible_expression_names(&lowered), ["x", "y[1]", "y[2]"]);
    assert!(
        lowered
            .dae
            .continuous
            .equations
            .iter()
            .all(|equation| equation.scalar_count == 1),
        "the surviving solver view is fully scalarized"
    );
}

/// Variables with no provenance, equations with provenance.
fn unspanned_variable_dae() -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable {
            source_span: Span::DUMMY,
            ..dae::Variable::new(VarName::new("x"), fixture_span())
        },
    );
    model
        .continuous
        .equations
        .push(eq(sub(der(var("x")), mul(real(2.0), var("x")))));
    model
}

/// The observable inventory now reads the *unscalarized* source DAE, and the
/// span it reports capacity failures against comes from `dae_model_span`, which
/// falls back to equation spans when no variable carries provenance. Pin both
/// sides of that fallback: a model whose provenance lives only on its equations
/// lowers, and a model with no provenance at all is rejected with the unspanned
/// contract violation rather than silently acquiring a dummy span.
#[test]
fn observable_inventory_falls_back_to_equation_spans() {
    let lowered =
        structurally_lower_dae_for_simulation(&unspanned_variable_dae(), &SimOptions::default())
            .expect("equation provenance is enough to build the observable inventory");
    assert_eq!(visible_expression_names(&lowered), ["x"]);

    let mut without_any_provenance = unspanned_variable_dae();
    for equation in &mut without_any_provenance.continuous.equations {
        equation.span = Span::DUMMY;
    }

    let error =
        structurally_lower_dae_for_simulation(&without_any_provenance, &SimOptions::default())
            .err()
            .expect("a model with no provenance anywhere cannot be lowered");
    assert!(
        error.to_string().contains("no source provenance"),
        "unexpected error for a provenance-free model: {error}"
    );
}

/// A model whose reference metadata cannot be attached: `ghost` is neither a
/// declared variable nor a structured reference, so `attach_dae_reference_metadata`
/// cannot resolve it.
fn unattachable_metadata_dae(with_algebraic: bool) -> dae::Dae {
    let mut model = dae::Dae::new();
    model.variables.states.insert(
        VarName::new("x"),
        dae::Variable::new(VarName::new("x"), fixture_span()),
    );
    model.continuous.equations.push(eq(sub(
        der(var("x")),
        Expression::VarRef {
            name: rumoca_core::Reference::new("ghost"),
            subscripts: Vec::new(),
            span: fixture_span(),
        },
    )));
    if with_algebraic {
        model.variables.algebraics.insert(
            VarName::new("a"),
            dae::Variable::new(VarName::new("a"), fixture_span()),
        );
        model
            .continuous
            .equations
            .push(eq(sub(var("a"), mul(real(2.0), var("x")))));
    }
    model
}

/// The direct path now runs its rejection predicates *before* it attaches
/// reference metadata, so a rejected model never pays for that copy. The
/// in-code claim that this cannot swallow a metadata-attachment failure rests
/// on the fallback: the structural funnel attaches the same metadata and maps
/// the failure through the same conversion.
///
/// Pin it from both sides of the reordered pair — a model the predicates reject
/// (it has an algebraic) and a model they accept — and require the same
/// metadata error either way.
#[test]
fn metadata_attachment_failure_is_reported_whether_or_not_the_direct_path_rejects() {
    let opts = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        ..SimOptions::default()
    };
    for with_algebraic in [true, false] {
        let error = lower_dae_for_simulation(&unattachable_metadata_dae(with_algebraic), &opts)
            .expect_err("an unresolvable reference must not lower");
        assert!(
            error
                .to_string()
                .contains("DAE reference metadata attachment failed"),
            "direct-path rejection={with_algebraic} lost the metadata error: {error}"
        );
        assert!(
            error.to_string().contains("ghost"),
            "the metadata error must name the unresolved reference: {error}"
        );
    }
}

/// A retained causal target that index reduction generated is declared by the
/// *prepared* DAE, never by the source snapshot the funnel took before the
/// structural rewrites ran.
///
/// `factor_retained_computations_in_dae` rewrites rows to read every retained
/// target, so a target this step declines to restore leaves rows naming a
/// variable no partition declares. Solve lowering reports exactly that as
/// `EL005` ("not a DAE variable"), which is how
/// `Modelica.Mechanics.Rotational.Examples.First` stopped simulating once index
/// reduction started naming `der(inertia1.w)` as `__dummyder__.inertia1.w`.
#[test]
fn causal_restore_declares_a_target_only_the_prepared_dae_knows() {
    let generated = VarName::new("__dummyder__.x");
    let mut lowered = dae::Dae::new();
    lowered.variables.states.insert(
        VarName::new("u"),
        dae::Variable::new(VarName::new("u"), fixture_span()),
    );
    lowered
        .continuous
        .equations
        .push(eq(sub(der(var("u")), var(generated.as_str()))));

    // The source snapshot predates index reduction, so it cannot name the
    // generated unknown; the prepared DAE is the one that declares it.
    let source = dae::Dae::new();
    let mut prepared = dae::Dae::new();
    let mut variable = dae::Variable::new(generated.clone(), fixture_span());
    variable.origin = dae::VariableOrigin::Generated;
    prepared
        .variables
        .algebraics
        .insert(generated.clone(), variable);

    let plan = rumoca_phase_structural::eliminate::CausalSubstitutionPlan {
        substitutions: vec![rumoca_phase_structural::eliminate::Substitution {
            var_name: generated.clone(),
            var_ref: None,
            expr: real(2.0),
            var_dims: Vec::new(),
            replacement_dims: Vec::new(),
            env_keys: vec![generated.as_str().to_string()],
        }],
        retained_targets: [generated.clone()].into_iter().collect(),
    };

    let restored = super::causal_reconstruction::restore_shared_causal_assignments(
        &mut lowered,
        &source,
        &prepared,
        &plan,
    );

    assert!(
        restored.contains(&generated),
        "the generated target must be reported as restored"
    );
    assert!(
        lowered.variables.algebraics.contains_key(&generated),
        "restoring the assignment must also declare the variable it assigns"
    );
    assert_eq!(
        lowered.continuous.equations.len(),
        2,
        "the restored assignment must be appended as its own defining row"
    );
}
