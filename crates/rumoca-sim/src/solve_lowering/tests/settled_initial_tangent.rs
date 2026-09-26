//! The settled initialization residual's directional derivative.
//!
//! Each fixture has an initialization row that reads a continuous algebraic
//! solved by a nonlinear block, so the initialization projection evaluates
//! its rows on the settled view: the parameter bindings and the algebraic
//! refresh applied to a copy of the coordinates. At the settled start point,
//! the derivative along every initialization unknown must match the values
//! the symmetric difference of the settled residual gave for the same
//! fixtures, to 1e-6 relative.

use rumoca_ir_solve as solve;
use rumoca_solver::{AlgebraicSettle, SolveRuntime};

use super::super::entry::lower_dae_for_simulation;
use super::compile;
use crate::SimOptions;

const TOLERANCE: f64 = 1e-6;
const SETTLE: AlgebraicSettle = AlgebraicSettle {
    tol: 1e-10,
    max_iters: 50,
};

/// A `fixed = false` parameter, bound into a second parameter, set by an
/// initial equation on an algebraic of a coupled nonlinear loop.
const LOOP: &str = "model SettledInitLoop
  parameter Real k(fixed = false, start = 1);
  parameter Real kk = 2*k;
  Real x(start = 0.5, fixed = true);
  Real a(start = 0.2);
  Real b(start = 0.1);
equation
  der(x) = -a;
  a = 0.3*sin(b) + kk*x;
  b = 0.1*a*a + x;
initial equation
  a = 0.3;
end SettledInitLoop;";

/// A free state and a `fixed = false` parameter set through an algebraic
/// that its own row defines implicitly.
const IMPLICIT: &str = "model SettledInitImplicit
  parameter Real c(fixed = false, start = 0.5);
  Real x(start = 1, fixed = false);
  Real a(start = 1);
equation
  der(x) = a - x;
  a = exp(0.2*x) + c*sin(a);
initial equation
  a = 1.5;
  der(x) = 0;
end SettledInitImplicit;";

/// The derivative of every initialization residual row along each
/// initialization unknown at the settled start point, one column per unknown
/// in plan order.
fn settled_columns(source: &str, name: &str) -> Vec<Vec<f64>> {
    let dae = compile(source, name);
    let model = lower_dae_for_simulation(&dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("lower {name}: {error:?}"));
    let initialization = &model.problem.initialization;
    assert!(
        initialization
            .row_roles()
            .contains(&solve::InitializationRowRole::SolvedThroughAlgebraicRefresh),
        "{name}: a row is solved through the algebraic refresh"
    );
    let runtime = SolveRuntime::new(&model).expect("prepare the runtime");
    let mut y = model.initial_y.clone();
    let mut p = model.parameters.clone();
    runtime
        .settle_initialization_system(&mut y, &mut p, 0.0, SETTLE.tol, SETTLE.max_iters)
        .unwrap_or_else(|error| panic!("{name}: settle the initialization: {error:?}"));
    let rows = initialization
        .residual()
        .len()
        .expect("a checked row count");
    let unknowns = initialization
        .projection_plan()
        .blocks
        .iter()
        .flat_map(|block| block.unknowns.iter().copied())
        .map(|slot| match slot {
            solve::ScalarSlot::Y { index, .. } => index,
            solve::ScalarSlot::P { index, .. } => y.len() + index,
            other => panic!("{name}: unexpected unknown {other:?}"),
        })
        .collect::<Vec<_>>();
    unknowns
        .iter()
        .map(|&column| {
            let mut direction = vec![0.0; y.len() + p.len()];
            direction[column] = 1.0;
            let mut out = vec![0.0; rows];
            runtime
                .eval_initial_residual_jacobian_v((&y, &p, 0.0), SETTLE, &direction, &mut out)
                .unwrap_or_else(|error| panic!("{name}: the settled JVP: {error:?}"));
            out
        })
        .collect()
}

fn assert_columns(name: &str, exact: &[Vec<f64>], reference: &[&[f64]]) {
    eprintln!("{name}: {exact:?}");
    assert_eq!(
        exact.len(),
        reference.len(),
        "{name}: one column per unknown"
    );
    for (column, (exact, reference)) in exact.iter().zip(reference).enumerate() {
        assert_eq!(
            exact.len(),
            reference.len(),
            "{name}: rows of column {column}"
        );
        let scale = reference
            .iter()
            .fold(1e-3, |max, value| value.abs().max(max));
        for (row, (exact, reference)) in exact.iter().zip(reference.iter()).enumerate() {
            assert!(
                (exact - reference).abs() <= TOLERANCE * scale,
                "{name} row {row} column {column}: {exact:e} vs {reference:e}"
            );
        }
    }
}

#[test]
fn settled_initial_jvp_matches_the_symmetric_difference_values_on_a_loop() {
    assert_columns(
        "SettledInitLoop",
        &settled_columns(LOOP, "SettledInitLoop"),
        &[&[1.015_969_187_034_837_5]],
    );
}

#[test]
fn settled_initial_jvp_matches_the_symmetric_difference_values_on_an_implicit_row() {
    assert_columns(
        "SettledInitImplicit",
        &settled_columns(IMPLICIT, "SettledInitImplicit"),
        &[
            &[1.008_229_851_051_645_2, 1.008_229_851_051_645_2],
            &[0.272_877_149_824_305_8, -0.727_122_850_182_245_3],
        ],
    );
}
