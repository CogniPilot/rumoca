//! Native discrete-input history uses issued runs and the existing bit predicate.

use super::codegen_test_support::builtin_template;

fn render(inputs: &serde_json::Value) -> String {
    let mut environment = super::create_environment();
    environment
        .add_template(
            "fmi-c-kernel.jinja",
            builtin_template("fmi3", "scalar_kernel.jinja"),
        )
        .unwrap();
    environment
        .add_template(
            "fmi-scalar-events.jinja",
            builtin_template("fmi3", "scalar_events.jinja"),
        )
        .unwrap();
    environment.add_template("history", "{%- from 'fmi-scalar-events.jinja' import discrete_input_history %}{{ discrete_input_history(inputs) }}").unwrap();
    environment
        .get_template("history")
        .unwrap()
        .render(minijinja::context! { inputs => inputs })
        .unwrap()
}

#[test]
fn discrete_input_c_size_follows_source_runs_not_cells() {
    let inputs = |count| {
        serde_json::json!({"scalar_count": count,
        "runs": [{"p_base": 0, "count": count, "seen_offset": 0}]})
    };
    let small = render(&inputs(6));
    let large = render(&inputs(167_497));
    assert!(
        large.len() <= small.len() + 16,
        "one declaration grew from {} to {} bytes",
        small.len(),
        large.len()
    );
    assert_eq!(large.matches("{ 0, 167497, 0 }").count(), 1);
    assert!(!large.contains("static const size_t rmc_discrete_input["));
}

fn run_c(source: &str) {
    let directory = tempfile::tempdir().unwrap();
    let path = directory.path().join("input-history.c");
    let binary = directory.path().join("input-history");
    std::fs::write(&path, source).unwrap();
    for optimization in ["-O0", "-O3"] {
        let mut command = std::process::Command::new("cc");
        command.args([
            "-std=c11",
            optimization,
            "-fno-fast-math",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
            "-Wvla",
        ]);
        if optimization == "-O0" {
            // Nix fortification requires optimization. Keep the optimized
            // control's hardening and every compiler warning unchanged.
            if let Ok(flags) = std::env::var("NIX_HARDENING_ENABLE") {
                let flags = flags
                    .split_whitespace()
                    .filter(|flag| !matches!(*flag, "fortify" | "fortify3"))
                    .collect::<Vec<_>>()
                    .join(" ");
                command.env("NIX_HARDENING_ENABLE", flags);
            }
            command.arg("-U_FORTIFY_SOURCE");
        }
        let result = command.arg(&path).arg("-o").arg(&binary).output().unwrap();
        assert!(
            result.status.success(),
            "{}\n{source}",
            String::from_utf8_lossy(&result.stderr)
        );
        let result = std::process::Command::new(&binary).output().unwrap();
        assert!(
            result.status.success(),
            "C history control {:?}: {}",
            result.status.code(),
            String::from_utf8_lossy(&result.stderr)
        );
    }
}

#[test]
fn discrete_input_c_preserves_complete_seen_bits_and_first_middle_last_changes() {
    let history = render(&serde_json::json!({"scalar_count": 5, "runs": [
        {"p_base": 6, "count": 2, "seen_offset": 0},
        {"p_base": 10, "count": 2, "seen_offset": 2},
        {"p_base": 6, "count": 1, "seen_offset": 4}
    ]}));
    let source = format!(
        r#"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
typedef struct {{ double p[12], rmc_input_seen[5]; }} ModelInstance;
{history}
int main(void) {{
    ModelInstance m = {{0}};
    const size_t indices[] = {{6, 7, 10, 11, 6}};
    const uint64_t words[] = {{UINT64_C(0x8000000000000000), 0, UINT64_C(0x7ff8000000001234), UINT64_C(0x7ff8000000005678)}};
    for (size_t k=0; k<4; ++k) memcpy(&m.p[indices[k]], &words[k], sizeof(double));
    rmc_discrete_inputs_seen(&m);
    assert(RMC_DISCRETE_INPUTS == 5 && RMC_DISCRETE_INPUT_RUNS == 3);
    assert(!rmc_discrete_inputs_changed(&m));
    for (size_t k=0; k<5; ++k) assert(memcmp(&m.rmc_input_seen[k], &m.p[indices[k]], sizeof(double)) == 0);
    ModelInstance committed = m;
    m.p[6] = 0;
    assert(rmc_discrete_inputs_changed(&m));
    m = committed;
    m.p[7] = -0.0;
    assert(rmc_discrete_inputs_changed(&m));
    m = committed;
    m.p[1] = 99;
    assert(!rmc_discrete_inputs_changed(&m));
    for (size_t k=0; k<4; ++k) {{
        m = committed;
        uint64_t changed = words[k] ^ UINT64_C(1);
        memcpy(&m.p[indices[k]], &changed, sizeof(double));
        assert(rmc_discrete_inputs_changed(&m));
        assert(memcmp(m.rmc_input_seen, committed.rmc_input_seen, sizeof m.rmc_input_seen) == 0);
        rmc_discrete_inputs_seen(&m);
        assert(!rmc_discrete_inputs_changed(&m));
        m = committed;
        assert(!rmc_discrete_inputs_changed(&m));
    }}
    return 0;
}}
"#
    );
    run_c(&source);
}

#[test]
fn discrete_input_c_empty_and_zero_length_runs_do_not_read_storage() {
    for runs in [
        serde_json::json!([]),
        serde_json::json!([{"p_base": 0, "count": 0, "seen_offset": 0}]),
    ] {
        let history = render(&serde_json::json!({"scalar_count": 0, "runs": runs}));
        let source = format!(
            r#"
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
typedef struct {{ double *p, *rmc_input_seen; }} ModelInstance;
{history}
int main(void) {{
    ModelInstance m = {{0}};
    rmc_discrete_inputs_seen(&m);
    return rmc_discrete_inputs_changed(&m) || RMC_DISCRETE_INPUTS != 0;
}}
"#
        );
        run_c(&source);
    }
}
