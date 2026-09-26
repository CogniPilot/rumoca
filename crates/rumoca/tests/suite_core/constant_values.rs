//! Constant pure calls fold to literals at construction, a declaration bound
//! to a literal is read as that literal, and a product whose factor is proven
//! zero is omitted from matching and from the executed programs (SPEC_0043 §4,
//! SPEC_0032). A slider loop whose base orientation comes from a zero-argument
//! record call therefore tears into a two-angle block whose sweep settles on
//! every call, with the slide solved alone.

use rumoca::Compiler;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_dae_with_diagnostics,
};

const CONSTANT_CALLS: &str = "
model ConstantCalls
  record Orientation
    Real T[3, 3];
    Real w[3];
  end Orientation;
  function unitOrientation
    output Orientation R;
  algorithm
    R := Orientation(T = identity(3), w = zeros(3));
  end unitOrientation;
  function scaled
    input Real k;
    output Real y[3];
  algorithm
    y := {k, 2*k, 0};
  end scaled;
  Orientation R;
  Real v[3];
  Real u[3];
  Real s;
  Real a(start = 1, fixed = true);
  Real b;
equation
  R = unitOrientation();
  v = scaled(2.0);
  u = scaled(time);
  a = R.T[1, 1]*s + 1;
  b = R.T[1, 2]*s + R.T[2, 2]*a;
  der(a) = -b + v[1] + u[1];
end ConstantCalls;";

const FAILING_CALL: &str = "
model FailingCall
  function negative
    input Real k;
    output Real y;
  algorithm
    assert(k < 0, \"k must be negative\");
    y := k;
  end negative;
  Real z;
  Real x(start = 0, fixed = true);
equation
  z = negative(1.0);
  der(x) = z;
end FailingCall;";

/// A slider loop: a driven crank ends at `pb`; a link of two revolute angles
/// `a1` (about z) and `a2` (about y) reaches back to a slide `s` along x whose
/// base orientation is a zero-argument record call.
const SLIDER_LOOP: &str = "
model SliderLoop
  record Orientation
    Real T[3, 3];
    Real w[3];
  end Orientation;
  function unitOrientation
    output Orientation R;
  algorithm
    R := Orientation(T = identity(3), w = zeros(3));
  end unitOrientation;
  function planarRotation
    input Real e[3];
    input Real angle;
    output Real T[3, 3];
  algorithm
    T := outerProduct(e, e) + (identity(3) - outerProduct(e, e))*cos(angle) - skew(e)*sin(angle);
    annotation(Inline = true);
  end planarRotation;
  function resolve1
    input Real T[3, 3];
    input Real v[3];
    output Real v1[3];
  algorithm
    v1 := transpose(T)*v;
    annotation(Inline = true);
  end resolve1;
  final parameter Real ex[3] = {1, 0, 0};
  final parameter Real ey[3] = {0, 1, 0};
  final parameter Real ez[3] = {0, 0, 1};
  parameter Real r1[3] = {0, 0.5, 0.1};
  parameter Real r2[3] = {0, 0.2, 0};
  parameter Real r3[3] = {-1, 0.3, 0.1};
  parameter Real base[3] = {1.2, 0, 0};
  Orientation world;
  Real phi(start = 0, fixed = true);
  Real T1[3, 3];
  Real Ta[3, 3];
  Real Tb[3, 3];
  Real T3[3, 3];
  Real pb[3];
  Real prev[3];
  Real a1(start = 0);
  Real a2(start = 0);
  Real s(start = -0.2);
equation
  world = unitOrientation();
  der(phi) = 0.5;
  T1 = planarRotation(ex, phi);
  pb = resolve1(T1, r1);
  Ta = planarRotation(ey, a2)*world.T;
  Tb = planarRotation(ez, a1);
  T3 = Tb*Ta;
  prev = base + resolve1(world.T, ex*s) + resolve1(world.T, r2);
  pb = prev + resolve1(T3, r3);
end SliderLoop;";

fn lowered(model: &str, source: &str) -> rumoca_ir_solve::SolveModel {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile {model}: {error:#}"));
    lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("lower {model}: {error:#}"))
}

/// Pure-call operations the continuous residual programs execute.
fn pure_calls(model: &rumoca_ir_solve::SolveModel) -> usize {
    serde_json::to_string(&model.problem.continuous.implicit_rhs)
        .expect("the residual programs serialize")
        .matches("\"PureCall\"")
        .count()
}

fn slot(model: &rumoca_ir_solve::SolveModel, name: &str) -> usize {
    model
        .problem
        .solve_layout
        .solver_maps
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("`{name}` has a solver slot"))
}

#[test]
fn constant_calls_fold_and_literal_bindings_are_read_as_literals() {
    let model = lowered("ConstantCalls", CONSTANT_CALLS);
    // The zero-argument record call and the all-literal call fold; the call
    // reading `time` stays a call.
    assert!(pure_calls(&model) > 0, "`scaled(time)` is not a constant");
    let without_time = CONSTANT_CALLS.replace("u = scaled(time);", "u = zeros(3);");
    assert_eq!(pure_calls(&lowered("ConstantCalls", &without_time)), 0);
    // `R.T[1, 2]` is bound to the literal 0, so `b` does not read `s`: `s` is
    // solved alone from the row of `a`, and `b` from its own row.
    let blocks = &model.problem.continuous.algebraic_projection_plan.blocks;
    for name in ["s", "b"] {
        let index = slot(&model, name);
        let block = blocks
            .iter()
            .find(|block| block.y_indices.contains(&index))
            .unwrap_or_else(|| panic!("`{name}` is projected"));
        assert_eq!(block.y_indices, [index], "`{name}` is a singleton block");
    }
}

#[test]
fn a_constant_call_that_fails_is_a_compile_error_naming_the_call() {
    let compiled = Compiler::new()
        .model("FailingCall")
        .compile_str(FAILING_CALL, "FailingCall.mo")
        .expect("the model is well formed");
    let error = lower_dae_for_simulation(&compiled.dae, &SimOptions::default())
        .expect_err("the call fails whenever it runs");
    let message = format!("{error:#}");
    assert!(
        message.contains("constant call of `") && message.contains("negative"),
        "{message}"
    );
}

#[test]
fn a_slider_loop_tears_into_two_angles_and_settles_without_dense_fallback() {
    let model = lowered("SliderLoop", SLIDER_LOOP);
    let blocks = &model.problem.continuous.algebraic_projection_plan.blocks;
    let (a1, a2, s) = (slot(&model, "a1"), slot(&model, "a2"), slot(&model, "s"));
    let coupled = blocks
        .iter()
        .find(|block| block.y_indices.contains(&a1))
        .expect("the link angles are projected");
    let tearing = coupled.tearing.as_ref().expect("the loop block is torn");
    assert!(coupled.y_indices.contains(&a2));
    assert!(!coupled.y_indices.contains(&s), "the slide leaves the loop");
    assert_eq!(tearing.tear_y_indices.len(), 2);
    assert_eq!(tearing.residual_rows.len(), 2);
    let slide = blocks
        .iter()
        .find(|block| block.y_indices.contains(&s))
        .expect("the slide is projected");
    assert_eq!(slide.y_indices, [s], "the slide is solved alone");

    let compiled = Compiler::new()
        .model("SliderLoop")
        .compile_str(SLIDER_LOOP, "SliderLoop.mo")
        .unwrap();
    rumoca_solver::runtime::hotpath_stats::reset_torn_declines();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.05),
            ..Default::default()
        },
    )
    .expect("the slider loop simulates");
    assert_eq!(
        rumoca_solver::runtime::hotpath_stats::torn_declines(),
        0,
        "every torn sweep settles the loop"
    );
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    // The crank end stays on the link: |pb - prev| equals |r3|.
    let r3 = (1.0_f64 + 0.09 + 0.01).sqrt();
    for step in 0..result.times.len() {
        let gap = (1..=3)
            .map(|axis| {
                let d =
                    column(&format!("pb[{axis}]"))[step] - column(&format!("prev[{axis}]"))[step];
                d * d
            })
            .sum::<f64>()
            .sqrt();
        assert!((gap - r3).abs() < 1e-6, "loop closure {gap} at step {step}");
    }
}
