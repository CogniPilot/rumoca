//! The generated C refinement of a `RootLocationPlan` narrows a state-event
//! bracket through the same coordinates, and ends at the same tolerance window,
//! as the linked `RootBracket`, bit for bit, over a set of crossing shapes (SPEC_0044 ME-EVENT-004).

use rumoca_ir_solve::fmi::{RootBracket, RootLocationPlan};

use crate::codegen::codegen_test_support::builtin_template;

const LOW: f64 = 0.3;
const WIDTH: f64 = 1.0e-5;
const TOLERANCE: f64 = 100.0 * f64::EPSILON * 0.125;
const SHAPES: usize = 10;

/// Indicator values of shape `shape` at `t`, written with the same operations
/// in the harness so both sides see identical values.
fn indicators(shape: usize, t: f64) -> Vec<f64> {
    let u = (t - LOW) / WIDTH;
    let u2 = u * u;
    let u4 = u2 * u2;
    match shape {
        0 => vec![u - 0.37],
        1 => vec![u2 - 0.4],
        2 => vec![u4 * u4 * u4 - 1.0e-3],
        3 => vec![(u - 0.5) * (u - 0.5) * (u - 0.5)],
        4 => vec![if u < 0.61 { -1.0 } else { 1.0 }],
        5 => vec![u - 1.0e-9],
        6 => vec![u - (1.0 - 1.0e-9)],
        7 => vec![1.0 - u],
        8 => vec![u],
        _ => vec![u - 0.6, u - 0.25, u - 0.4, u - 0.9],
    }
}

/// The linked side: the loop every executor runs over a bracket, printing
/// each evaluated coordinate and the located ends as `f64` bits.
fn linked(shape: usize) -> Vec<u64> {
    let positive = |value: f64| value > 0.0;
    let reference = indicators(shape, LOW)
        .into_iter()
        .map(positive)
        .collect::<Vec<_>>();
    let entered_at = |values: &[f64], k: usize| positive(values[k]) != reference[k];
    let plan = RootLocationPlan::STANDARD;
    let mut bracket: RootBracket = plan.open_bracket(LOW, LOW + WIDTH, TOLERANCE);
    let mut at_low = indicators(shape, LOW);
    let mut at_high = indicators(shape, LOW + WIDTH);
    let mut trace = Vec::new();
    for _ in 0..plan.refinement_iteration_cap() {
        if bracket.is_located() {
            break;
        }
        let least = (0..at_high.len())
            .filter(|&k| entered_at(&at_high, k))
            .map(|k| bracket.crossing_fraction(at_low[k], at_high[k]))
            .fold(1.0, f64::min);
        let Some(trial) = bracket.trial(least) else {
            break;
        };
        trace.push(trial.to_bits());
        let values = indicators(shape, trial);
        let entered = (0..values.len()).any(|k| entered_at(&values, k));
        bracket.narrow(trial, entered);
        if entered {
            at_high = values;
        } else {
            at_low = values;
        }
    }
    trace.push(bracket.low().to_bits());
    trace.push(bracket.high().to_bits());
    trace.push(bracket.application_window(LOW + WIDTH).to_bits());
    trace.push(bracket.application_window(bracket.high()).to_bits());
    trace
}

const HARNESS: &str = r#"{%- from "fmi-scalar-events.jinja" import root_refinement %}
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
{{ root_refinement(plan) }}
static const double LOW = {{ low }}, WIDTH = {{ width }}, TOLERANCE = {{ tolerance }};
static size_t indicators(int shape, double t, double* z) {
    double u = (t - LOW) / WIDTH, u2 = u * u, u4 = u2 * u2;
    switch (shape) {
    case 0: z[0] = u - 0.37; return 1;
    case 1: z[0] = u2 - 0.4; return 1;
    case 2: z[0] = u4 * u4 * u4 - 1.0e-3; return 1;
    case 3: z[0] = (u - 0.5) * (u - 0.5) * (u - 0.5); return 1;
    case 4: z[0] = u < 0.61 ? -1.0 : 1.0; return 1;
    case 5: z[0] = u - 1.0e-9; return 1;
    case 6: z[0] = u - (1.0 - 1.0e-9); return 1;
    case 7: z[0] = 1.0 - u; return 1;
    case 8: z[0] = u; return 1;
    default: z[0] = u - 0.6; z[1] = u - 0.25; z[2] = u - 0.4; z[3] = u - 0.9; return 4;
    }
}
static void bits(double value) {
    uint64_t word;
    memcpy(&word, &value, sizeof(word));
    printf(" %" PRIu64, word);
}
int main(void) {
    for (int shape = 0; shape < {{ shapes }}; ++shape) {
        double reference[4] = {0}, z_low[4] = {0}, z_high[4] = {0}, z[4] = {0};
        size_t n = indicators(shape, LOW, reference);
        indicators(shape, LOW, z_low);
        indicators(shape, LOW + WIDTH, z_high);
        RmcRootBracket bracket = rmc_root_open(LOW, LOW + WIDTH, TOLERANCE);
        for (size_t iter = 0; iter < {{ plan.refinement_iteration_cap }}; ++iter) {
            double least = 1.0, trial = 0.0;
            bool entered = false;
            if (rmc_root_located(&bracket)) break;
            for (size_t k = 0; k < n; ++k) {
                if ((reference[k] > 0.0) != (z_high[k] > 0.0)) least = fmin(least, rmc_root_fraction(&bracket, z_low[k], z_high[k]));
            }
            if (!rmc_root_trial(&bracket, least, &trial)) break;
            bits(trial);
            indicators(shape, trial, z);
            for (size_t k = 0; k < n; ++k) entered |= (reference[k] > 0.0) != (z[k] > 0.0);
            rmc_root_narrow(&bracket, trial, entered);
            memcpy(entered ? z_high : z_low, z, sizeof(z));
        }
        bits(bracket.low);
        bits(bracket.high);
        bits(rmc_root_window(&bracket, LOW + WIDTH));
        bits(rmc_root_window(&bracket, bracket.high));
        printf("\n");
    }
    return 0;
}
"#;

fn render() -> String {
    let mut environment = crate::codegen::create_environment();
    environment
        .add_template(
            "fmi-c-kernel.jinja",
            builtin_template("fmi3", "scalar_kernel.jinja"),
        )
        .expect("the shared kernel template parses");
    environment
        .add_template(
            "fmi-scalar-events.jinja",
            builtin_template("fmi3", "scalar_events.jinja"),
        )
        .expect("the scalar event template parses");
    environment
        .add_template("harness.c", HARNESS)
        .expect("the harness parses");
    environment
        .get_template("harness.c")
        .expect("the harness is registered")
        .render(minijinja::context! {
            plan => RootLocationPlan::STANDARD,
            low => format!("{LOW:.17e}"),
            width => format!("{WIDTH:.17e}"),
            tolerance => format!("{TOLERANCE:.17e}"),
            shapes => SHAPES,
        })
        .expect("the refinement harness renders")
}

fn run_c(source: &str) -> Vec<Vec<u64>> {
    let directory = tempfile::tempdir().expect("a temporary directory");
    let path = directory.path().join("refine.c");
    std::fs::write(&path, source).expect("write the harness");
    let binary = directory.path().join("refine");
    let compile = std::process::Command::new("cc")
        .args([
            "-std=c11",
            "-O2",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
        ])
        .arg(&path)
        .args(["-lm", "-o"])
        .arg(&binary)
        .output()
        .expect("start the C compiler");
    assert!(
        compile.status.success(),
        "the refinement C did not compile:\n{}\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = std::process::Command::new(&binary)
        .output()
        .expect("run the harness");
    assert!(run.status.success(), "the refinement C failed");
    String::from_utf8(run.stdout)
        .expect("decimal output")
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|word| word.parse().expect("a decimal word"))
                .collect()
        })
        .collect()
}

#[test]
fn the_generated_refinement_matches_the_linked_bracket_bit_for_bit() {
    let generated = run_c(&render());
    assert_eq!(generated.len(), SHAPES);
    for (shape, generated) in generated.iter().enumerate() {
        let linked = linked(shape);
        assert!(linked.len() >= 3, "shape {shape} evaluates at least once");
        assert_eq!(generated, &linked, "shape {shape}");
    }
}
