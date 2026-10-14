//! The generated C sliding-mode history of a `RootLocationPlan` decides which
//! indicators slide exactly as the linked `SlidingModes` does, after every
//! operation of a scripted event history (SPEC_0044 ME-EVENT-005).

use rumoca_ir_solve::fmi::{RootLocationPlan, SlidingModes};

use crate::codegen::codegen_test_support::builtin_template;

const INDICATORS: usize = 3;
const SCAN: f64 = 0.125;

/// One step of the history: begin an interval, or record a state event at a
/// time that changed the indicators of a bit mask.
#[derive(Clone, Copy)]
enum Step {
    Begin,
    Event(f64, u8),
}

fn script() -> Vec<Step> {
    let gap = RootLocationPlan::STANDARD.sliding_gap(SCAN);
    let mut steps = Vec::new();
    let mut events = |start: f64, spacing: f64, count: u32, mask: u8| {
        for event in 0..count {
            steps.push(Step::Begin);
            steps.push(Step::Event(start + f64::from(event) * spacing, mask));
        }
    };
    // A sliding anti-windup limit on indicator 1, then sliding events an
    // interval apart, which keep the mode.
    events(1.0, 1.0e-8, 10, 0b010);
    events(1.001, 1.0e-3, 3, 0b010);
    // Two simultaneous indicators re-crossing together.
    events(2.0, 1.0e-8, 9, 0b011);
    // Fast periodic switching on indicator 2 never slides.
    events(3.0, 1.0e-3, 12, 0b100);
    // A spacing just inside the gap continues a streak; just beyond it does
    // not.
    events(4.0, gap * (1.0 - 1.0e-6), 9, 0b001);
    events(5.0, gap * (1.0 + 1.0e-6), 9, 0b001);
    // An interval that completes without re-crossing ends the sliding mode.
    steps.push(Step::Begin);
    steps.push(Step::Begin);
    steps.push(Step::Event(6.0, 0b010));
    steps
}

/// Per step: each indicator's sliding flag, then `only_sliding` over every
/// non-empty and the empty indicator set, as `0`/`1`.
fn observe(modes: &SlidingModes) -> String {
    let mut line = String::new();
    for indicator in 0..INDICATORS {
        line.push(if modes.is_sliding(indicator) {
            '1'
        } else {
            '0'
        });
    }
    for mask in 0..(1_u8 << INDICATORS) {
        let changed = indices(mask);
        line.push(if modes.only_sliding(&changed) {
            '1'
        } else {
            '0'
        });
    }
    line
}

fn indices(mask: u8) -> Vec<usize> {
    (0..INDICATORS).filter(|k| mask & (1 << k) != 0).collect()
}

fn linked() -> Vec<String> {
    let plan = RootLocationPlan::STANDARD;
    let mut modes = SlidingModes::new(INDICATORS);
    script()
        .into_iter()
        .map(|step| {
            match step {
                Step::Begin => modes.begin_interval(),
                Step::Event(time, mask) => modes.record_event(
                    time,
                    &indices(mask),
                    plan.sliding_gap(SCAN),
                    plan.sliding_streak(),
                ),
            }
            observe(&modes)
        })
        .collect()
}

const HARNESS: &str = r#"{%- from "fmi-scalar-events.jinja" import sliding_modes_struct, sliding_modes_reset, sliding_modes %}
#include <math.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
{{ sliding_modes_struct(indicators) }};
{{ sliding_modes(plan) }}
static const int KIND[] = { {% for step in steps %}{{ step[0] }},{% endfor %} };
static const double TIME[] = { {% for step in steps %}{{ step[1] }},{% endfor %} };
static const unsigned MASK[] = { {% for step in steps %}{{ step[2] }}u,{% endfor %} };
int main(void) {
    struct RmcSlidingModes modes;
    {{ sliding_modes_reset("modes", indicators) }}
    for (size_t i = 0; i != sizeof(KIND) / sizeof(KIND[0]); ++i) {
        bool changed[{{ indicators }}];
        for (size_t k = 0; k != {{ indicators }}; ++k) changed[k] = (MASK[i] >> k) & 1u;
        if (KIND[i] == 0) rmc_slide_begin(&modes, {{ indicators }});
        else rmc_slide_record(&modes, changed, {{ indicators }}, TIME[i], rmc_slide_gap({{ scan }}));
        for (size_t k = 0; k != {{ indicators }}; ++k) putchar(modes.sliding[k] ? '1' : '0');
        for (unsigned mask = 0; mask != 1u << {{ indicators }}; ++mask) {
            for (size_t k = 0; k != {{ indicators }}; ++k) changed[k] = (mask >> k) & 1u;
            putchar(rmc_slide_only(&modes, changed, {{ indicators }}) ? '1' : '0');
        }
        putchar('\n');
    }
    return 0;
}
"#;

fn render() -> String {
    let steps = script()
        .into_iter()
        .map(|step| match step {
            Step::Begin => (0, "0.0".to_owned(), 0),
            Step::Event(time, mask) => (1, format!("{time:.17e}"), mask),
        })
        .collect::<Vec<_>>();
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
            indicators => INDICATORS,
            scan => format!("{SCAN:.17e}"),
            steps => steps,
        })
        .expect("the sliding-mode harness renders")
}

fn run_c(source: &str) -> Vec<String> {
    let directory = tempfile::tempdir().expect("a temporary directory");
    let path = directory.path().join("sliding.c");
    std::fs::write(&path, source).expect("write the harness");
    let binary = directory.path().join("sliding");
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
        "the sliding-mode C did not compile:\n{}\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = std::process::Command::new(&binary)
        .output()
        .expect("run the harness");
    assert!(run.status.success(), "the sliding-mode C failed");
    String::from_utf8(run.stdout)
        .expect("text output")
        .lines()
        .map(str::to_owned)
        .collect()
}

#[test]
fn the_generated_sliding_modes_match_the_linked_history() {
    let linked = linked();
    // The script exercises entering, keeping, and ending a sliding mode.
    assert!(linked.iter().any(|line| line.starts_with("010")));
    assert!(linked.iter().any(|line| line.starts_with("110")));
    assert!(linked.iter().any(|line| line.starts_with("100")));
    assert!(linked.iter().all(|line| line.as_bytes()[2] == b'0'));
    assert_eq!(run_c(&render()), linked);
}
