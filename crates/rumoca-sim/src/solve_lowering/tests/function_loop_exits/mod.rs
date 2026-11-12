//! Source-to-Solve controls for MLS §11.2.4 `break` in function loops.
//!
//! Every value a loop exit depends on is a state the probe sets, so no call is
//! settled at translation and each case runs the compiled loop. Each
//! expectation is computed by the same algorithm written in plain Rust.

use super::function_returns::compile_model;
use crate::{SimOptions, eval_dae_at};
use rumoca_compile::compile::CompilationResult;
use std::fmt::Write as _;

mod algorithms;
mod for_breaks;
mod while_breaks;

/// The run-time arguments of every fixture: five samples and two thresholds.
const ARGUMENTS: [&str; 7] = ["x1", "x2", "x3", "x4", "x5", "limit", "stop"];

/// The samples as one array argument.
const SAMPLES: &str = "{x1, x2, x3, x4, x5}";

/// One run-time point: the samples and the two thresholds.
#[derive(Clone, Copy, Debug)]
struct Point {
    x: [f64; 5],
    limit: f64,
    stop: f64,
}

impl Point {
    fn new(x: [f64; 5], limit: f64) -> Self {
        Self {
            x,
            limit,
            stop: 1.0e9,
        }
    }

    fn stopping_at(self, stop: f64) -> Self {
        Self { stop, ..self }
    }
}

/// A compiled model whose result `r<i>` is the derivative of a state set to
/// the `i`-th result expression.
struct Fixture {
    compiled: CompilationResult,
    results: usize,
}

impl Fixture {
    /// Compile `functions` with a model that evaluates `results`, after the
    /// algebraic `declarations` and `equations`.
    fn compile(functions: &str, declarations: &str, equations: &str, results: &[&str]) -> Self {
        let mut source = String::from(functions);
        source.push_str("\nmodel LoopExits\n");
        for name in ARGUMENTS
            .iter()
            .copied()
            .map(String::from)
            .chain((1..=results.len()).map(|ordinal| format!("r{ordinal}")))
        {
            writeln!(source, "  Real {name}(start = 0, fixed = true);").unwrap();
        }
        source.push_str(declarations);
        source.push_str("equation\n");
        for name in ARGUMENTS {
            writeln!(source, "  der({name}) = 0;").unwrap();
        }
        source.push_str(equations);
        for (ordinal, result) in results.iter().enumerate() {
            writeln!(source, "  der(r{}) = {result};", ordinal + 1).unwrap();
        }
        source.push_str("end LoopExits;\n");
        let compiled = compile_model("LoopExits", &source, "loop_exits.mo")
            .unwrap_or_else(|error| panic!("the loop exits construct checked DAE: {error}"));
        Self {
            compiled,
            results: results.len(),
        }
    }

    /// The results at `point`, through Solve IR.
    fn evaluate(&self, point: Point) -> Vec<f64> {
        let values = point.x.iter().copied().chain([point.limit, point.stop]);
        let overrides = ARGUMENTS
            .iter()
            .zip(values)
            .map(|(name, value)| ((*name).to_string(), value))
            .collect::<Vec<_>>();
        let probe = eval_dae_at(&self.compiled.dae, &SimOptions::default(), &overrides, 0.0)
            .expect("the loop exits lower through Solve IR");
        let report = probe.report;
        assert!(report.error.is_none(), "{point:?}: {:?}", report.error);
        (1..=self.results)
            .map(|ordinal| {
                let name = format!("der(r{ordinal})");
                report
                    .derivatives
                    .iter()
                    .find(|slot| slot.name == name)
                    .unwrap_or_else(|| panic!("the probe reports {name}"))
                    .value
            })
            .collect()
    }

    /// Require the compiled results at every point to equal `expected`.
    fn check(&self, points: &[Point], expected: impl Fn(Point) -> Vec<f64>) {
        for &point in points {
            assert_eq!(self.evaluate(point), expected(point), "{point:?}");
        }
    }
}
