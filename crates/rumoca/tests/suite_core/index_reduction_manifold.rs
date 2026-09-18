//! Joint MLS §8.6 initialization of retained position and velocity constraints.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

fn circular_motion(declarations: &str, initial: &str) -> String {
    format!(
        r#"
model CircularMotion
  parameter Real radius = 1;
  {declarations}
  Real lambda;
initial equation
  {initial}
equation
  der(x) = vx;
  der(y) = vy;
  der(vx) = -lambda*x;
  der(vy) = -lambda*y;
  x*x + y*y = radius*radius;
end CircularMotion;
"#
    )
}

#[test]
fn fixed_initial_values_survive_holonomic_reduction() {
    check_motion(
        &circular_motion(
            "Real x(start=1,fixed=true); Real y(start=0,fixed=true); Real vx(start=0,fixed=true); Real vy(start=1,fixed=true);",
            "",
        ),
        1.0,
        0.0,
    );
}

#[test]
fn implicit_algebraic_contact_coordinates_preserve_circular_motion() {
    check_motion(&implicit_contact_motion(), 1.0, 0.0);
}

fn implicit_contact_motion() -> String {
    circular_motion(
        "Real x(start=1,fixed=true); Real y(start=0,fixed=true); Real vx(start=0,fixed=true); Real vy(start=1,fixed=true); Real s; Real w;",
        "",
    )
    .replace(
        "x*x + y*y = radius*radius;",
        "s + w = x; s - w = y; 2*(s*s + w*w) = radius*radius;",
    )
}

#[test]
fn implicit_contact_coordinates_differentiate_changing_coefficients() {
    for coefficient in ["1+time", "1+time+time*time", "1+x*x"] {
        let mapped = format!("(({coefficient})*s+w)");
        let equations = format!(
            "{mapped} = x; s - 2*w = y; {mapped}*{mapped} + (s-2*w)*(s-2*w) = radius*radius;"
        );
        let source = implicit_contact_motion().replace(
            "s + w = x; s - w = y; 2*(s*s + w*w) = radius*radius;",
            &equations,
        );
        check_motion_with_constants(&source, 1.0, 0.0, &[("lambda", 1.0)]);
    }
}

#[test]
fn implicit_contact_reduction_preserves_inconsistent_initial_constraints() {
    let source =
        implicit_contact_motion().replace("x(start=1,fixed=true)", "x(start=0.5,fixed=true)");
    let compiled = Compiler::new()
        .model("CircularMotion")
        .compile_str(&source, "inconsistent_implicit_contact.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err("implicit coordinate reconstruction must retain the position constraint");
        assert!(
            error.to_string().contains("initial variable projection"),
            "{error}"
        );
    }
}

#[test]
fn implicit_contact_reduction_owns_one_aggregate_solve_and_both_manifolds() {
    let compiled = Compiler::new()
        .model("CircularMotion")
        .compile_str(&implicit_contact_motion(), "implicit_contact_owners.mo")
        .unwrap();
    let solve_count = |view: rumoca_ir_dae::DaeView<'_>| {
        (0..view.expression_count())
            .filter(|&index| {
                matches!(
                    view.expression(view.expression_id(index).unwrap())
                        .unwrap()
                        .operation(),
                    rumoca_ir_dae::ExpressionOperation::Builtin {
                        builtin: rumoca_ir_dae::PureBuiltin::LinearSolve,
                        ..
                    }
                )
            })
            .count()
    };
    assert_eq!(compiled.dae.inspect(solve_count), 0);
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    prepared.inspect(|system| {
        assert_eq!(solve_count(system.view), 1);
        assert_eq!(system.view.continuous_owners().count(), 7);
        assert_eq!(system.manifold.len(), 2);
        for ordinal in [4, 5] {
            let rumoca_ir_dae::ContinuousOwnerView::Residual { equation, .. } =
                system.view.continuous_owner(ordinal).unwrap()
            else {
                panic!("source scalar definition survives");
            };
            for name in ["s", "w"] {
                let (variable, _) = system
                    .view
                    .variables()
                    .find(|(_, variable)| variable.name().as_str() == name)
                    .unwrap();
                assert!(rumoca_ir_dae::expr_contains_var(
                    system.view,
                    equation.residual(),
                    variable
                ));
            }
        }
    });
}

#[test]
fn initial_equations_and_both_retained_constraints_are_solved_together() {
    check_motion(
        &circular_motion(
            "Real x(start=0.6,fixed=true); Real y(start=0.7); Real vx(start=-0.8,fixed=true); Real vy(start=0.7);",
            "",
        ),
        0.6,
        0.8,
    );
}

#[test]
fn mixed_array_equation_preserves_unconstrained_components() {
    check_mixed_motion(&mixed_array_motion());
}

#[test]
fn scalar_equations_for_the_same_mixed_array_preserve_every_component() {
    check_mixed_motion(&mixed_array_motion().replace(
        "{s,w,road[3]} = {1,2,radius*radius};",
        "s = 1; w = 2; road[3] = radius*radius;",
    ));
}

#[test]
fn mixed_array_index_reduction_retains_tensor_owners() {
    let source = mixed_array_motion();
    let compiled = Compiler::new()
        .model("CircularMotion")
        .compile_str(&source, "mixed_array_motion.mo")
        .unwrap();
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    let before = compiled.dae.inspect(tensor_owner_shapes);
    let after = prepared.as_dae().inspect(tensor_owner_shapes);
    assert_eq!(before, after);
    assert_eq!(after.len(), 2);
    prepared.inspect(|system| {
        let roots = system
            .view
            .continuous_owners()
            .filter_map(|owner| match owner {
                rumoca_ir_dae::ContinuousOwnerView::Structured { family, .. } => {
                    family.bodies().get(0)
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert!(
            matches!(
                system.view.expression(roots[0]).unwrap().operation(),
                rumoca_ir_dae::ExpressionOperation::Binary { .. }
            ),
            "the causal road definition must remain intact"
        );
        assert!(
            matches!(
                system.view.expression(roots[1]).unwrap().operation(),
                rumoca_ir_dae::ExpressionOperation::ArrayUpdate { .. }
            ),
            "only the constrained tensor component is differentiated"
        );
        assert_eq!(system.manifold.len(), 2);
        assert!(
            system.manifold.iter().all(|&id| system
                .view
                .expression(id)
                .unwrap()
                .value_type()
                .is_scalar())
        );
        assert!((0..system.view.expression_count()).any(|index| matches!(
            system.view.expression(system.view.expression_id(index).unwrap()).unwrap().operation(),
            rumoca_ir_dae::ExpressionOperation::ArrayUpdate { .. }
        )));
    });
}

#[test]
fn mixed_array_middle_component_reduction_keeps_both_neighbors() {
    check_mixed_motion(&mixed_array_motion().replace(
        "{s,w,road[3]} = {1,2,radius*radius};",
        "{s,road[3],w} = {1,radius*radius,2};",
    ));
}

#[test]
fn mixed_array_constraint_rejects_inconsistent_fixed_position() {
    let source = mixed_array_motion().replace("x(start=1,fixed=true)", "x(start=0.6,fixed=true)");
    let compiled = Compiler::new()
        .model("CircularMotion")
        .compile_str(&source, "inconsistent_tensor_constraint.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err("tensor differentiation must retain the original position constraint");
        assert!(
            error.to_string().contains("initial variable projection"),
            "{error}"
        );
    }
}

#[derive(Debug, PartialEq, Eq)]
struct TensorOwnerShape {
    domain_extents: Vec<u32>,
    body_shapes: Vec<Vec<u32>>,
}

fn tensor_owner_shapes(view: rumoca_ir_dae::DaeView<'_>) -> Vec<TensorOwnerShape> {
    view.continuous_owners()
        .filter_map(|owner| {
            let rumoca_ir_dae::ContinuousOwnerView::Structured { family, .. } = owner else {
                return None;
            };
            Some(TensorOwnerShape {
                domain_extents: view.domain(family.domain()).unwrap().extents().to_vec(),
                body_shapes: family
                    .bodies()
                    .iter()
                    .map(|id| {
                        view.expression(id)
                            .unwrap()
                            .value_type()
                            .dimensions()
                            .to_vec()
                    })
                    .collect(),
            })
        })
        .collect()
}

fn mixed_array_motion() -> String {
    circular_motion(
        "Real x(start=1,fixed=true); Real y(start=0,fixed=true); Real vx(start=0,fixed=true); Real vy(start=1,fixed=true); Real s; Real w; Real road[3];",
        "",
    )
    .replace(
        "x*x + y*y = radius*radius;",
        "road = {s,w,x*x+y*y}; {s,w,road[3]} = {1,2,radius*radius};",
    )
}

fn check_mixed_motion(source: &str) {
    check_motion_with_constants(
        source,
        1.0,
        0.0,
        &[
            ("s", 1.0),
            ("w", 2.0),
            ("road[1]", 1.0),
            ("road[2]", 2.0),
            ("road[3]", 1.0),
            ("lambda", 1.0),
        ],
    );
}

#[test]
fn a_fixed_start_uses_the_parameter_value_solved_at_initialization() {
    let source = circular_motion(
        "parameter Real p(start=2,fixed=false); Real x(start=p,fixed=true); Real y(start=0,fixed=true); Real vx(start=0,fixed=true); Real vy(start=1,fixed=true);",
        "p = 1;",
    );
    check_motion(&source, 1.0, 0.0);
}

#[test]
fn inconsistent_fixed_position_is_rejected_by_joint_initialization() {
    let source = circular_motion(
        "Real x(start=0.6,fixed=true); Real y(start=0,fixed=true); Real vx(start=0,fixed=true); Real vy(start=1,fixed=true);",
        "",
    );
    let compiled = Compiler::new()
        .model("CircularMotion")
        .compile_str(&source, "inconsistent_initial_manifold.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                ..Default::default()
            },
        )
        .expect_err("fixed initial position cannot be moved onto a different circle");
        assert!(
            error.to_string().contains("initial variable projection"),
            "{error}"
        );
    }
}

fn check_motion(source: &str, x0: f64, y0: f64) {
    check_motion_with_constants(source, x0, y0, &[]);
}

fn check_motion_with_constants(source: &str, x0: f64, y0: f64, constants: &[(&str, f64)]) {
    let compiled = Compiler::new()
        .model("CircularMotion")
        .compile_str(source, "initial_manifold.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
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
        for &(name, expected) in constants {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (&t, &actual) in result.times.iter().zip(&result.data[column]) {
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {name}({t}): {actual} != {expected}"
                );
            }
        }
        for (name, initial) in [("x", x0), ("y", y0), ("vx", -y0), ("vy", x0)] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            assert!(
                (result.data[column][0] - initial).abs() < 1e-7,
                "{solver_mode:?} {name}(0): {} != {initial}",
                result.data[column][0]
            );
            for (&t, &actual) in result.times.iter().zip(&result.data[column]) {
                let (sin, cos) = t.sin_cos();
                let expected = match name {
                    "x" => x0 * cos - y0 * sin,
                    "y" => y0 * cos + x0 * sin,
                    "vx" => -x0 * sin - y0 * cos,
                    "vy" => -y0 * sin + x0 * cos,
                    _ => unreachable!(),
                };
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {name}({t}): {actual} != {expected}"
                );
            }
        }
    }
}

#[test]
fn differentiated_time_and_invariant_constraints_keep_initial_contradictions() {
    for anchor in ["time", "5"] {
        let source = format!(
            "model ConflictingAnchor Real x(start=1,fixed=true); Real f; equation der(x)=f; x={anchor}; end ConflictingAnchor;"
        );
        let compiled = Compiler::new()
            .model("ConflictingAnchor")
            .compile_str(&source, "conflicting_anchor.mo")
            .unwrap();
        for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
            let error = simulate_dae_with_diagnostics(
                &compiled.dae,
                &SimOptions {
                    solver_mode,
                    ..Default::default()
                },
            )
            .expect_err(
                "differentiation cannot erase x(0)=1 versus the retained original equation",
            );
            assert!(
                error.to_string().contains("initial variable projection"),
                "{anchor} on {solver_mode:?}: {error}"
            );
        }
    }
}

// A holonomic constraint that is a conserved invariant of the differentiated
// dynamics (its highest differentiation is matched to a state derivative of the
// block it couples, introducing no Lagrange multiplier) keeps its source
// coordinates as states; a loop closure, whose differentiated constraint
// determines a multiplier, still reduces to an independent basis.
// See spec/SPEC_0053_CONSTRAINED_STATE_SELECTION.md section 1.
const CONSERVED_QUATERNION: &str = r#"
model ConservedQuaternion
  Real Q[4](start={1,0,0,0});
  parameter Real w1=0.1, w2=0.2, w3=0.3;
equation
  der(Q[1]) = 0.5*(-Q[2]*w1 - Q[3]*w2 - Q[4]*w3);
  der(Q[2]) = 0.5*( Q[1]*w1 + Q[3]*w3 - Q[4]*w2);
  der(Q[3]) = 0.5*( Q[1]*w2 - Q[2]*w3 + Q[4]*w1);
  Q*Q = 1;
end ConservedQuaternion;
"#;

const PLANAR_LOOP_CLOSURE: &str = r#"
model PlanarLoopClosure
  Real q[2](start={1,0});
  Real v[2](start={0,1});
  Real lambda;
equation
  der(q) = v;
  der(v) = lambda*q;
  q*q = 1;
end PlanarLoopClosure;
"#;

#[test]
fn conserved_quaternion_norm_retains_the_source_basis() {
    let compiled = Compiler::new()
        .model("ConservedQuaternion")
        .compile_str(CONSERVED_QUATERNION, "conserved_quaternion.mo")
        .unwrap();
    // The unit-norm constraint is a first integral of the kinematic rate
    // equations, so all four quaternion components are retained as states and
    // the norm is enforced by the manifold projection rather than reducing the
    // basis to three folding coordinates.
    let prepared = rumoca_phase_structural::prepare_for_solve(&compiled.dae).unwrap();
    assert!(prepared.inspect(|system| !system.manifold.is_empty()));
    let lowered = rumoca_phase_solve::lower_solve_model(
        &compiled.dae,
        &std::collections::HashMap::new(),
        |_| {},
    )
    .unwrap();
    assert_eq!(lowered.model().state_scalar_count(), 4);
    assert!(compiled.dae.inspect(|view| {
        view.variables()
            .any(|(_, variable)| variable.name().as_str() == "Q")
    }));
}

#[test]
fn planar_loop_closure_still_reduces_to_its_independent_basis() {
    let compiled = Compiler::new()
        .model("PlanarLoopClosure")
        .compile_str(PLANAR_LOOP_CLOSURE, "planar_loop_closure.mo")
        .unwrap();
    // der(v) = lambda*q reads the multiplier lambda, so the twice-differentiated
    // position constraint determines a Lagrange multiplier. This block is a loop
    // closure and reduces from four retained coordinates to its two independent
    // states.
    let lowered = rumoca_phase_solve::lower_solve_model(
        &compiled.dae,
        &std::collections::HashMap::new(),
        |_| {},
    )
    .unwrap();
    assert_eq!(lowered.model().state_scalar_count(), 2);
}
