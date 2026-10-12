//! A derivative row that reads another state's derivative (MLS 3.7 §3.7.4).
//!
//! `Electrical.Analog.Basic.OpAmpDetailed` limits its slew rate with
//! `der(v_source) = smooth(0, noEvent(if der(x) > sr then sr else ...))`, where
//! `der(x)` has its own explicit equation. Only a read of the row's own target
//! derivative takes part in isolating it; any other derivative the row reads
//! is that state's defined value, which the compiler substitutes exactly as it
//! does for an algebraic row.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package DerRead
  model SlewRate
    Real x(start = 0, fixed = true);
    Real y(start = 0, fixed = true);
  equation
    der(x) = cos(time);
    der(y) = smooth(0, noEvent(if der(x) > 0.5 then 0.5 else der(x)));
  end SlewRate;
  model Affine
    Real x(start = 0, fixed = true);
    Real y(start = 0, fixed = true);
  equation
    der(x) = cos(time);
    2*der(y) + der(x)*der(x) = 1;
  end Affine;
end DerRead;
"#;

fn final_y(model: &str) -> f64 {
    let compiled = match Compiler::new()
        .model(model)
        .compile_str(MODELS, "DerRead.mo")
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("{model} compiles: {error:?}"),
    };
    let options = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let result: SimResult = match simulate_dae_with_diagnostics(&compiled.dae, &options) {
        Ok(result) => result,
        Err(error) => panic!("{model} simulates: {error:?}"),
    };
    let index = result.names.iter().position(|name| name == "y");
    let (Some(_), Some(last)) = (index, index.and_then(|i| result.data[i].last())) else {
        panic!("y is recorded with at least one sample");
    };
    *last
}

#[test]
fn an_explicit_row_reads_another_states_derivative() {
    // cos(t) > 0.5 until t = pi/3, so y(1) = 0.5.
    assert!((final_y("DerRead.SlewRate") - 0.5).abs() < 1e-5);
}

#[test]
fn an_affine_row_reads_another_states_derivative_in_its_offset() {
    // der(y) = sin(t)^2/2, so y(1) = 1/4 - sin(2)/8.
    let expected = 0.25 - 2.0f64.sin() / 8.0;
    assert!((final_y("DerRead.Affine") - expected).abs() < 1e-5);
}

#[test]
fn a_when_equation_reads_the_matched_tensor_derivative() {
    let source = "model EventDerivative Real x[2](start={1,2},each fixed=true); discrete Real sampled[2](each start=0,each fixed=true); equation der(x)={-x[1],-2*x[2]}; when time>=0.1 then sampled=der(x); end when; end EventDerivative;";
    check_event_derivative(
        source,
        "EventDerivative",
        &["sampled[1]", "sampled[2]"],
        &[-(-0.1_f64).exp(), -4.0 * (-0.2_f64).exp()],
    );
}

#[test]
fn reinitialization_reads_the_matched_derivative_without_new_storage() {
    let source = "model EventDerivative Real x(start=1,fixed=true); Real sampled(start=0,fixed=true); equation der(x)=-x; der(sampled)=0; when time>=0.1 then reinit(sampled,der(x)); end when; end EventDerivative;";
    check_event_derivative(
        source,
        "EventDerivative",
        &["sampled"],
        &[-(-0.1_f64).exp()],
    );
}

#[test]
fn a_when_equation_derivative_reads_the_current_same_tick_producer() {
    let source = "model SameTickDerivative Real x(start=0,fixed=true); discrete Real a(start=0,fixed=true); discrete Real z(start=1,fixed=true); equation der(x)=z; when sample(0,0.1) then a=der(x); z=pre(z)+1; end when; end SameTickDerivative;";
    check_same_tick_derivative(source, [2.0, 3.0, 4.0]);
}

#[test]
fn a_same_tick_derivative_follows_an_exact_algebraic_alias() {
    let source = "model SameTickDerivative Real x(start=0,fixed=true); Real rhs; discrete Real a(start=0,fixed=true); discrete Real z(start=1,fixed=true); equation rhs=z; der(x)=rhs; when sample(0,0.1) then a=der(x); z=pre(z)+1; end when; end SameTickDerivative;";
    check_same_tick_derivative(source, [2.0, 3.0, 4.0]);
}

#[test]
fn a_same_tick_derivative_preserves_its_previous_value_boundary() {
    let source = "model SameTickDerivative Real x(start=0,fixed=true); discrete Real a(start=0,fixed=true); discrete Real z(start=1,fixed=true); equation der(x)=pre(z); when sample(0,0.1) then a=der(x); z=pre(z)+1; end when; end SameTickDerivative;";
    check_same_tick_derivative(source, [1.0, 2.0, 3.0]);
}

#[test]
fn a_same_tick_algebraic_derivative_result_retains_its_matched_writer() {
    let source = "model SameTickDerivative Real x(start=0,fixed=true); Real rhs; discrete Real a(start=0,fixed=true); discrete Real z(start=1,fixed=true); equation der(x)=z; rhs=der(x); when sample(0,0.1) then a=rhs; z=pre(z)+1; end when; end SameTickDerivative;";
    check_same_tick_derivative(source, [2.0, 3.0, 4.0]);
}

fn check_same_tick_derivative(source: &str, expected_a: [f64; 3]) {
    check_same_tick_values(source, [("a", expected_a), ("z", [2.0, 3.0, 4.0])]);
}

#[test]
fn independent_tensor_derivatives_do_not_issue_a_false_same_tick_cycle() {
    let source = "model SameTickDerivative Real x[2](each start=0,each fixed=true); discrete Real a(start=0,fixed=true); discrete Real b(start=0,fixed=true); equation der(x)={10,b}; when sample(0,0.1) then b=a+1; a=der(x[1]); end when; end SameTickDerivative;";
    check_same_tick_values(source, [("a", [10.0; 3]), ("b", [11.0; 3])]);
}

#[test]
fn a_tensor_derivative_alias_keeps_its_selected_scalar_dependency() {
    let source = "model SameTickDerivative Real x[2](each start=0,each fixed=true); Real rhs[2]; discrete Real a(start=0,fixed=true); discrete Real b(start=0,fixed=true); equation rhs={10,b}; der(x)=rhs; when sample(0,0.1) then b=a+1; a=der(x[1]); end when; end SameTickDerivative;";
    check_same_tick_values(source, [("a", [10.0; 3]), ("b", [11.0; 3])]);
}

#[test]
fn a_tensor_derivative_alias_retains_the_dependent_scalar_producer() {
    let source = "model SameTickDerivative Real x[2](each start=0,each fixed=true); Real rhs[2]; discrete Real a(start=0,fixed=true); discrete Real b(start=1,fixed=true); equation rhs={10,b}; der(x)=rhs; when sample(0,0.1) then a=der(x[2]); b=pre(b)+1; end when; end SameTickDerivative;";
    check_same_tick_values(source, [("a", [2.0, 3.0, 4.0]), ("b", [2.0, 3.0, 4.0])]);
}

#[test]
fn a_tensor_derivative_alias_retains_both_requested_scalar_dependencies() {
    let source = "model SameTickDerivative Real x[2](each start=0,each fixed=true); Real rhs[2]; discrete Real a(start=0,fixed=true); discrete Real b(start=1,fixed=true); equation rhs={10,b}; der(x)=rhs; when sample(0,0.1) then a=der(x[1])+der(x[2]); b=pre(b)+1; end when; end SameTickDerivative;";
    check_same_tick_values(source, [("a", [12.0, 13.0, 14.0]), ("b", [2.0, 3.0, 4.0])]);
}

fn check_same_tick_values(source: &str, expected_values: [(&str, [f64; 3]); 2]) {
    let compiled = Compiler::new()
        .model("SameTickDerivative")
        .compile_str(source, "SameTickDerivative.mo")
        .unwrap();
    for policy in [
        rumoca_sim::SimExecutionPolicy::Interpreter,
        rumoca_sim::SimExecutionPolicy::Auto,
    ] {
        let result = rumoca_sim::simulate_dae(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.1),
                solver_mode: rumoca_sim::SimSolverMode::RkLike,
                execution_policy: policy,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{policy:?}: same-tick derivative {error}"));
        let grid = regular_grid(&result, 0.1);
        for (name, expected) in expected_values {
            let column = result.names.iter().position(|n| n == name).unwrap();
            for (&row, expected) in grid.iter().zip(expected) {
                assert_eq!(result.data[column][row], expected, "{policy:?}: {name}");
            }
        }
    }
}

fn check_event_derivative(source: &str, name: &str, names: &[&str], expected: &[f64]) {
    let compiled = rumoca::Compiler::new()
        .model(name)
        .compile_str(source, "EventDerivative.mo")
        .unwrap();
    for policy in [
        rumoca_sim::SimExecutionPolicy::Interpreter,
        rumoca_sim::SimExecutionPolicy::Auto,
    ] {
        let result = rumoca_sim::simulate_dae(
            &compiled.dae,
            &rumoca_sim::SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                atol: 1e-10,
                rtol: 1e-10,
                solver_mode: rumoca_sim::SimSolverMode::RkLike,
                execution_policy: policy,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{policy:?}: event derivative {error}"));
        regular_grid(&result, 0.05);
        for (&name, &expected) in names.iter().zip(expected) {
            let column = result.names.iter().position(|n| n == name).unwrap();
            assert_eq!(result.data[column][0], 0.0);
            let actual = *result.data[column].last().unwrap();
            assert!(
                actual.is_finite() && (actual - expected).abs() < 1e-7,
                "{policy:?}: {name}={actual}, expected {expected}"
            );
        }
    }
}

fn regular_grid(result: &SimResult, dt: f64) -> Vec<usize> {
    assert_eq!(result.times.first(), Some(&0.0));
    assert_eq!(result.times.last(), Some(&0.2));
    assert!(result.times.windows(2).all(|pair| pair[0] < pair[1]));
    assert_eq!(result.data.len(), result.names.len());
    assert!(result.data.iter().all(|column| {
        column.len() == result.times.len() && column.iter().all(|value| value.is_finite())
    }));
    (0..=(0.2 / dt).round() as usize)
        .map(|step| {
            let time = step as f64 * dt;
            result
                .times
                .iter()
                .position(|&observed| observed == time)
                .unwrap_or_else(|| panic!("required regular sample {time} is absent"))
        })
        .collect()
}
