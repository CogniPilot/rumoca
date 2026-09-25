//! MLS §9.4 spanning-forest regression on `Loops.Fourbar1` (SPEC_0022 §3.12).
//!
//! The MSL model and three permutations of its `connect` statements must break
//! the same optional edge, and every one must pass constrained state selection
//! and simulate to the same trajectory. The breadth-first forest closes the
//! loop inside `b3`, where both paths from `world` meet. Under that cut the
//! first derivative of the `b1` and `b3` translation equations reads frame
//! rates through `Frames.resolve1_der`; formal construction must certify those
//! rates as dependencies of that stage (SPEC_0040 STRUCT-T07) for state
//! selection to settle.

use rumoca_ir_flat::EquationOrigin;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

use super::msl_sim_regression::require_msl_compiler;

const ORIGINAL: &str = "model Fourbar1Original
  extends Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1;
end Fourbar1Original;
";

fn variants() -> [(&'static str, &'static str); 4] {
    [
        ("Fourbar1Original", ORIGINAL),
        (
            "Fourbar1ConnectOrderA",
            include_str!("../fixtures/overconstrained/Fourbar1ConnectOrderA.mo"),
        ),
        (
            "Fourbar1ConnectOrderB",
            include_str!("../fixtures/overconstrained/Fourbar1ConnectOrderB.mo"),
        ),
        (
            "Fourbar1ConnectOrderC",
            include_str!("../fixtures/overconstrained/Fourbar1ConnectOrderC.mo"),
        ),
    ]
}

/// Record pairs of every generated `equalityConstraint`, each in name order.
fn broken_edges(flat: &rumoca_ir_flat::Model) -> Vec<(String, String)> {
    let mut edges = flat
        .equations
        .iter()
        .filter_map(|equation| match &equation.origin {
            EquationOrigin::Connection { rhs, .. } if rhs.contains("equalityConstraint") => {
                let arguments = rhs.trim_end_matches(')').split_once('(')?.1;
                let (lhs, rhs) = arguments.split_once(", ")?;
                let mut pair = [lhs.to_string(), rhs.to_string()];
                pair.sort();
                let [lhs, rhs] = pair;
                Some((lhs, rhs))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    edges.sort();
    edges
}

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("simulation result missing {name}"));
    &result.data[index]
}

#[test]
fn fourbar1_connect_orders_share_one_cut_and_one_trajectory() {
    let mut reference: Option<SimResult> = None;
    for (model, source) in variants() {
        let compiled = require_msl_compiler()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .unwrap_or_else(|error| panic!("{model}: {error}"));
        assert_eq!(
            broken_edges(&compiled.flat),
            [(
                "b3.frameTranslation.frame_b.R".to_string(),
                "b3.frame_b.R".to_string()
            )],
            "{model}: the loop must close at the breadth-first cut"
        );
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 1.0,
                dt: Some(0.01),
                solver_mode: SimSolverMode::Bdf,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{model}: {error}"));
        let Some(reference) = &reference else {
            reference = Some(result);
            continue;
        };
        for name in ["j1.phi", "j1.w", "j2.s", "j3.phi", "rev.phi", "j5.phi"] {
            let delta = series(&result, name)
                .iter()
                .zip(series(reference, name))
                .map(|(lhs, rhs)| (lhs - rhs).abs())
                .fold(0.0, f64::max);
            assert!(delta < 1e-6, "{model}: {name} differs by {delta}");
        }
    }
}
