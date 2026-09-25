//! Reverse rows and forward tangents agree where a derivative does not exist.
//!
//! Each fixture holds a stiff algebraic loop at rest on a point where a local
//! partial is undefined until a drive switches on at `t = 0.5`, one row
//! through a builtin (forward dual) and one through a function (typed
//! directional owner). The projection solver assembles the loop matrix from
//! reverse rows and certifies it with forward Jacobian-vector products, so both
//! must apply the kink rules of `rumoca_eval_solve::reverse`:
//!
//! - `Atan2OriginKink`: `atan2(x, z)` at `x = z = 0` contributes no partial.
//! - `PowNegativeBaseKink`: `(x - 1)^0.5` at `x < 1` is outside its domain and
//!   masked by `max`, so its non-finite base partial contributes zero.
//! - `PowSeededExponentKink`: `x^n` at `x = 0, n = 1`, with an exponent that
//!   moves with the loop, keeps its base partial `n * x^(n - 1) = 1` in every
//!   direction.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};
use rumoca_solver::SimExecutionPolicy;

struct Fixture {
    source: &'static str,
    file: &'static str,
    model: &'static str,
    probes: [&'static str; 3],
}

/// Simulate the fixture with each solver under `execution_policy` and return
/// the final values of its probes, one row per solver.
fn finals(
    fixture: &Fixture,
    solvers: &[SimSolverMode],
    execution_policy: SimExecutionPolicy,
) -> Vec<[f64; 3]> {
    let Fixture {
        source,
        file,
        model,
        probes,
    } = fixture;
    let compiled = match Compiler::new().model(model).compile_str(source, file) {
        Ok(compiled) => compiled,
        Err(error) => panic!("{model} must compile: {error}"),
    };
    let mut rows = Vec::new();
    for &solver_mode in solvers {
        let result = match simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                solver_mode,
                execution_policy,
                t_end: 1.0,
                dt: Some(0.01),
                ..Default::default()
            },
        ) {
            Ok(result) => result,
            Err(error) => panic!("{model} with {solver_mode:?} must simulate: {error}"),
        };
        rows.push(probes.map(|probe| {
            let Some(index) = result.names.iter().position(|name| name == probe) else {
                panic!("{model} reports {probe}");
            };
            let Some(last) = result.data[index].last() else {
                panic!("{model} records {probe}");
            };
            *last
        }));
    }
    rows
}

fn assert_charged_and_agreeing(model: &str, rows: &[[f64; 3]]) {
    let first = rows[0];
    assert!(
        first[0] > 0.1,
        "{model}: the drive charged the loop: {rows:?}"
    );
    for row in rows {
        for (value, reference) in row.iter().zip(first) {
            assert!(
                (value - reference).abs() < 1e-3 * reference.abs().max(1.0),
                "{model}: the solvers agree: {rows:?}"
            );
        }
    }
}

#[test]
fn a_stiff_loop_resting_on_the_atan2_origin_simulates() {
    let fixture = Fixture {
        source: include_str!("../fixtures/sensitivity/Atan2OriginKink.mo"),
        file: "Atan2OriginKink.mo",
        model: "Atan2OriginKink.Loop",
        probes: ["i", "x", "z"],
    };
    let rows = finals(
        &fixture,
        &[SimSolverMode::Bdf, SimSolverMode::RkLike],
        SimExecutionPolicy::Auto,
    );
    assert_charged_and_agreeing(fixture.model, &rows);
}

/// Compiled rows take `max` of a NaN operand as NaN, so only the reference
/// interpreter, which returns the other operand, evaluates this loop.
#[test]
fn a_stiff_loop_masking_a_negative_base_power_simulates() {
    let fixture = Fixture {
        source: include_str!("../fixtures/sensitivity/PowNegativeBaseKink.mo"),
        file: "PowNegativeBaseKink.mo",
        model: "PowNegativeBaseKink.Loop",
        probes: ["i", "x", "z"],
    };
    let rows = finals(
        &fixture,
        &[SimSolverMode::Bdf, SimSolverMode::RkLike],
        SimExecutionPolicy::Interpreter,
    );
    assert_charged_and_agreeing(fixture.model, &rows);
}

#[test]
fn a_stiff_loop_resting_on_a_zero_base_with_a_moving_exponent_simulates() {
    let fixture = Fixture {
        source: include_str!("../fixtures/sensitivity/PowSeededExponentKink.mo"),
        file: "PowSeededExponentKink.mo",
        model: "PowSeededExponentKink.Loop",
        probes: ["i", "x", "n"],
    };
    let rows = finals(&fixture, &[SimSolverMode::Bdf], SimExecutionPolicy::Auto);
    let [[i, x, n]] = rows[..] else {
        panic!("one BDF trajectory: {rows:?}");
    };
    assert!(i > 0.1, "the drive charged the loop: {rows:?}");
    assert!(
        (n - (1.0 + 0.1 * x)).abs() < 1e-8 && (x + 0.1 * x.powf(n) - i).abs() < 1e-6,
        "the final point solves the loop: {rows:?}"
    );
}
