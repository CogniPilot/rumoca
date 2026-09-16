//! An implicit tensor block fixes a dependent coordinate without new states.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model ImplicitTensorAuxiliary
  Real theta(start=0.2,fixed=true,stateSelect=StateSelect.always);
  Real z(start=0.2);
  Real v(start=1);
  Real force;
  Real q[2];
equation
  der(theta) = 1;
  der(z) = v;
  der(v) = force;
  q*{1.0,1.0} = theta;
  q*{1.0,-1.0} = 3*theta;
  0 = q[2] + z;
end ImplicitTensorAuxiliary;
"#;

#[test]
fn coupled_tensor_auxiliary_preserves_linear_motion_with_bdf() {
    check_motion(SOURCE, SimSolverMode::Bdf);
}

#[test]
fn coupled_tensor_auxiliary_preserves_linear_motion_with_rk() {
    check_motion(SOURCE, SimSolverMode::RkLike);
}

#[test]
fn affine_tensor_map_retains_its_offset_and_both_derivatives() {
    let source = SOURCE
        .replace(
            "q*{1.0,1.0} = theta;",
            "{{1,1},{1,-1}}*q + {theta*theta,-theta} = {theta+theta*theta,2*theta};",
        )
        .replace("  q*{1.0,-1.0} = 3*theta;\n", "");
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(&source, solver);
    }
}

#[test]
fn nested_affine_tensor_maps_reconstruct_transitive_rate_dependencies() {
    let source = SOURCE
        .replace("  Real q[2];", "  Real q[2];\n  Real r[2];")
        .replace(
            "q*{1.0,1.0} = theta;",
            "{{1,1},{1,-1}}*q + {theta*theta,-theta} = r;",
        )
        .replace(
            "q*{1.0,-1.0} = 3*theta;",
            "2*r = {2*(theta+theta*theta),4*theta};",
        );
    let deeper = source
        .replace("Real r[2];", "Real r[2];\n  Real s[2];\n  Real u[2];")
        .replace(
            "2*r = {2*(theta+theta*theta),4*theta};",
            "4*u = {24*(theta+theta*theta),48*theta};\n  3*s = u;\n  2*r = s;",
        );
    for source in [&source, &deeper] {
        for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
            check_motion(source, solver);
        }
    }
}

#[test]
fn signed_zero_equations_share_derivative_and_auxiliary_normalization() {
    let source = SOURCE
        .replace("der(theta) = 1;", "0 = -(der(theta)-1);")
        .replace("der(z) = v;", "0 = 0+(der(z)-v);")
        .replace("der(v) = force;", "0 = (der(v)-force)-0;")
        .replace("q*{1.0,1.0} = theta;", "0 = theta - q*{1.0,1.0};");
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(&source, solver);
    }
}

#[test]
fn varying_tensor_coefficients_preserve_shape_and_second_derivative() {
    let source = SOURCE.replace(
        "q*{1.0,1.0} = theta;",
        "q*{1.0+theta,2.0+theta} = theta*theta;",
    );
    assert_ne!(source, SOURCE);
    for solver in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        check_motion(&source, solver);
    }
}

#[test]
fn a_second_auxiliary_derivative_does_not_resolve_the_primal_system() {
    use rumoca_ir_dae as dae;
    let source = SOURCE.replace(
        "q*{1.0,1.0} = theta;",
        "q*{1.0+theta,2.0+theta} = theta*theta;",
    );
    let compiled = Compiler::new()
        .model("ImplicitTensorAuxiliary")
        .compile_str(&source, "ImplicitTensorAuxiliary.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    prepared.inspect(|system| {
        let view = system.view;
        for residual in system.manifold {
            dae::for_each_expression(view, *residual, |_, node| {
                assert!(!matches!(
                    node.operation(),
                    dae::ExpressionOperation::Coordinate(
                        dae::CoordinateView::Algebraic(_) | dae::CoordinateView::Derivative(_)
                    )
                ));
            });
        }
        let max_depth = view
            .continuous_owners()
            .map(|owner| match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => {
                    auxiliary_solve_depth(view, equation.residual())
                }
                dae::ContinuousOwnerView::Structured { family, .. } => family
                    .bodies()
                    .iter()
                    .map(|body| auxiliary_solve_depth(view, body))
                    .max()
                    .unwrap_or(0),
            })
            .max()
            .unwrap_or(0);
        assert_eq!(
            max_depth, 2,
            "second derivative needs two tangent solves; the source equations own the primal solve"
        );
    });
}

fn auxiliary_solve_depth<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    root: rumoca_ir_dae::ExprId<'dae>,
) -> usize {
    use rumoca_ir_dae as dae;
    let mut depth = 0;
    dae::for_each_expression_pruned(view, root, |_, node| {
        let dae::ExpressionOperation::Call {
            function,
            arguments,
            ..
        } = node.operation()
        else {
            return true;
        };
        let has_solve = view
            .function(function)
            .unwrap()
            .result_values()
            .iter()
            .any(|value| {
                let mut found = false;
                dae::for_each_expression(view, value.rhs(), |_, node| {
                    found |= matches!(
                        node.operation(),
                        dae::ExpressionOperation::Builtin {
                            builtin: dae::PureBuiltin::LinearSolve,
                            ..
                        }
                    );
                });
                found
            });
        let nested = arguments
            .iter()
            .map(|arg| auxiliary_solve_depth(view, arg))
            .max()
            .unwrap_or(0);
        depth = depth.max(nested + usize::from(has_solve));
        false
    });
    depth
}

#[test]
fn a_nonlinear_tensor_block_remains_outside_the_affine_proof() {
    let source = SOURCE.replace("q*{1.0,1.0} = theta;", "q*{q[1],1.0} = theta;");
    let compiled = Compiler::new()
        .model("ImplicitTensorAuxiliary")
        .compile_str(&source, "ImplicitTensorAuxiliary.mo")
        .unwrap();
    let error = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .err()
        .expect("unknown-dependent coefficients must not receive a linear reconstruction");
    assert!(
        error.to_string().contains("structurally singular"),
        "{error}"
    );
}

#[test]
fn a_singular_auxiliary_matrix_is_rejected_before_integration() {
    let source = SOURCE.replace("q*{1.0,-1.0} = 3*theta;", "q*{1.0,1.0} = 3*theta;");
    let compiled = Compiler::new()
        .model("ImplicitTensorAuxiliary")
        .compile_str(&source, "ImplicitTensorAuxiliary.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err("a singular auxiliary matrix must refuse simulation");
        assert!(
            error
                .to_string()
                .contains("stage -2 has a singular dependent Jacobian"),
            "{error}"
        );
    }
}

fn check_motion(source: &str, solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("ImplicitTensorAuxiliary")
        .compile_str(source, "ImplicitTensorAuxiliary.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae)
        .unwrap_or_else(|error| panic!("{error:?}"));
    let counts = |view: rumoca_ir_dae::DaeView<'_>| {
        (
            view.variable_count(),
            view.continuous_owners().count(),
            view.variables()
                .filter(|(_, v)| v.role() == rumoca_ir_dae::VariableRole::State)
                .count(),
        )
    };
    assert_eq!(
        compiled.dae.inspect(counts),
        prepared.as_dae().inspect(counts)
    );
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.01),
            solver_mode,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
    for (row, &time) in result.times.iter().enumerate() {
        let theta = 0.2 + time;
        for (name, expected) in [
            ("theta", theta),
            ("z", theta),
            ("v", 1.0),
            ("force", 0.0),
            ("q[1]", 2.0 * theta),
            ("q[2]", -theta),
        ] {
            let column = result
                .names
                .iter()
                .position(|actual| actual == name)
                .unwrap();
            let actual = result.data[column][row];
            assert!(
                (actual - expected).abs() < 1e-6,
                "{solver_mode:?} {name}({time}): {actual} != {expected}"
            );
        }
    }
}
