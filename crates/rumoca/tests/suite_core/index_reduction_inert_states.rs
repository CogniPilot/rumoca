//! SPEC_0040 STRUCT-T04: a state whose derivative only a coefficient proven
//! zero multiplies is declared algebraic.
//!
//! `im` is differentiated, but its only derivative read is scaled by the final
//! parameter `k = 0`, so no equation determines `der(im)`, while `4*im = 3*w`
//! and `w = k*i` determine `im` itself. This is the shape of an MSL
//! FundamentalWave converter whose orientation is zero.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model InertState
  function scaled
    input Real a;
    input Real b;
    output Real y;
  algorithm
    y := a*b;
    annotation(Inline = true);
  end scaled;
  final parameter Real k = 0;
  Real i;
  Real v;
  Real w;
  Real re;
  Real im;
equation
  v = sin(time) - i;
  re = 2*i;
  w = k*i;
  4*im = 3*w;
  v = der(re) + scaled(k, der(im));
end InertState;
"#;

/// `v = der(re) = 2*der(i)` and `v = sin(time) - i` give
/// `2*der(i) + i = sin(time)`, `i(0) = 0`.
fn expected_current(t: f64) -> f64 {
    (t.sin() - 2.0 * t.cos() + 2.0 * (-t / 2.0).exp()) / 5.0
}

fn check(source: &str) {
    let compiled = Compiler::new()
        .model("InertState")
        .compile_str(source, "inert_state.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 2.0,
                dt: Some(0.1),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        let column = |name: &str| {
            let index = result.names.iter().position(|n| n == name).unwrap();
            &result.data[index]
        };
        for (k, &t) in result.times.iter().enumerate() {
            let i = expected_current(t);
            for (name, expected) in [("i", i), ("re", 2.0 * i), ("im", 0.0), ("w", 0.0)] {
                let actual = column(name)[k];
                assert!(
                    (actual - expected).abs() < 1e-4,
                    "{solver_mode:?}: {name}({t}) = {actual}, expected {expected}"
                );
            }
        }
    }
}

#[test]
fn derivative_scaled_by_a_zero_inlined_call_argument_is_not_a_state() {
    check(SOURCE);
}

#[test]
fn derivative_scaled_by_a_zero_factor_is_not_a_state() {
    check(&SOURCE.replace("scaled(k, der(im))", "k*der(im)"));
}

/// The coefficient is an algebraic whose own equation makes it zero wherever
/// it has a value: a product with the zero factor `k`, a quotient with such a
/// numerator, and their sum, around calls that stay calls (the shape of the
/// junction capacitances of an MSL `NPN` whose transit times and zero-bias
/// capacitances are zero). The equation still evaluates the calls.
#[test]
fn derivative_scaled_by_an_algebraic_zero_through_calls_is_not_a_state() {
    let source = SOURCE
        .replace(
            "  end scaled;\n",
            "  end scaled;\n  function growth\n    input Real a;\n    output Real y;\n  algorithm\n    y := exp(a);\n  end growth;\n",
        )
        .replace("  Real im;\n", "  Real im;\n  Real c;\n")
        .replace(
            "equation\n  v = sin",
            "equation\n  c = smooth(1, k*i/(2 + i*i)*growth(i) + k*growth(i));\n  v = sin",
        )
        .replace("scaled(k, der(im))", "c*der(im)");
    check(&source);
}

/// An initial equation reads `der(im)`, which no zero-coefficient proof
/// covers there, so the state is kept and the system stays singular rather
/// than reading that derivative as zero.
#[test]
fn derivative_read_by_an_initial_equation_keeps_the_state() {
    let source = SOURCE.replace(
        "equation\n  v = sin",
        "initial equation\n  der(im) = 0;\nequation\n  v = sin",
    );
    let compiled = Compiler::new()
        .model("InertState")
        .compile_str(&source, "inert_state_initial.mo")
        .unwrap();
    let error = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .expect_err("an initial equation reading the derivative keeps the state");
    assert!(error.to_string().contains("singular"), "{error}");
}

/// A delay source reads `der(im)`. Incidence reads the delay as its own
/// coordinate, so no column covers that read; the state is kept and the
/// system stays singular rather than delaying a derivative read as zero.
#[test]
fn derivative_read_by_a_delay_source_keeps_the_state() {
    for delay in [
        "delay(der(im), 0.1)",
        "delay(der(im), 0.1 + 0.05*sin(time), 0.2)",
    ] {
        let source = SOURCE
            .replace("  Real im;\n", "  Real im;\n  Real y;\n")
            .replace(
                "equation\n  v = sin",
                &format!("equation\n  y = {delay};\n  v = sin"),
            );
        let compiled = Compiler::new()
            .model("InertState")
            .compile_str(&source, "inert_state_delay.mo")
            .unwrap();
        let error = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                ..Default::default()
            },
        )
        .expect_err("a delay source reading the derivative keeps the state");
        assert!(error.to_string().contains("singular"), "{delay}: {error}");
    }
}
