//! Unit tests for the solve-lowering stages, exercising the lowering entry
//! points, the inspection probes, and the structural diagnosis through the
//! re-exported stage modules.

use rumoca_core::{BuiltinFunction, Expression, OpBinary, SourceId, Span, Subscript, VarName};
use rumoca_ir_dae as dae;
use rumoca_solver::{SimOptions, SimSolverMode};

use super::diagnostics::SimulationDiagnosticError;
use super::entry::{lower_dae_for_simulation, lower_dae_for_simulation_with_stage_timing};
use super::probe::{eval_dae_at, jacobian_for_dae};
use super::structural_lowering::{
    metadata_attachment_lower_error, structurally_lower_dae_for_simulation,
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

    assert_eq!(model.state_scalar_count(), 1);
    assert!(stages.contains(&"ir_solve_direct"));
    assert!(stages.contains(&"ir_solve_structural_dae"));
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

    assert_eq!(model.state_scalar_count(), 1);
    assert!(!x1_meta.is_state);
    assert_eq!(x1_meta.role, "algebraic");
    assert!(x2_meta.is_state);
    assert!(!x3_meta.is_state);
    assert_eq!(x3_meta.role, "algebraic");
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
    assert_eq!(diagnostic.diagnostic_code(), "lowering");
    assert_eq!(
        diagnostic.diagnostic_label(),
        "array operands have incompatible shapes [3, 3] and [2]"
    );
}

#[test]
fn simulation_diagnostic_preserves_runtime_preparation_span() {
    let span = sim_source_span(8, 12, 18);
    let error = rumoca_eval_solve::EvalSolveError::Scalarization {
        message: "invalid native map metadata".to_string(),
        span: Some(span),
    };
    let diagnostic = SimulationDiagnosticError::from(error);

    assert_eq!(diagnostic.diagnostic_code(), "simulation");
    assert_eq!(diagnostic.source_span(), Some(span));
    assert_eq!(
        diagnostic.to_string(),
        "Solve-IR scalarization failed: invalid native map metadata"
    );
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
        .push(eq(sub(der(var("x1")), time())));
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
        name: reference(name),
        args,
        is_constructor: false,
        span: fixture_span(),
    }
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
