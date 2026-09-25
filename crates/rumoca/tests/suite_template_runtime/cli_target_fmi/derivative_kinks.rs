//! The generated C kernel's projection Jacobians follow the kink rules of
//! `rumoca_eval_solve::reverse` at the same points as the linked sites.
//!
//! Each `suite_core` kink fixture starts at rest on its kink: `atan2(x, z)` at
//! `x = z = 0`, `max(0, (x - 1)^0.5)` at `x = 0`, and `x^n` at `x = 0, n = 1`,
//! each once through a builtin (the forward programs) and once through a
//! function (the typed directional programs). A C harness compiled against the
//! generated FMI 3 `model.c` evaluates every projection block's colored
//! Jacobian at the start values and prints its entries. Each entry must be
//! finite and the block must hold exactly the partials the rule table states:
//! a NaN or infinite tangent at the kink would have poisoned the Newton
//! correction the kernel builds from it. The kernel seeds one color at a time,
//! and the colors of `x` and `n` differ, so the zero-base power pins the
//! partial values; that no rule depends on which operands carry a tangent is
//! pinned by `rumoca_phase_solve`'s `kink_rule_tests`.

use super::*;

const HARNESS: &str = r#"#include "model.c"
#include <stdio.h>

int main(void) {
    ModelInstance* m = calloc(1, sizeof(ModelInstance));
    if (!m) return 2;
    initialize_values(m);
    int status = 0;
    for (size_t k = 0; k < sizeof(rmc_blocks) / sizeof(rmc_blocks[0]); ++k) {
        const RmcBlock* b = &rmc_blocks[k];
        double values[64];
        if (b->nnz > 64) { status = 3; break; }
        if (rmc_jacobian(m, b, values) != 0) { printf("block %zu unavailable\n", k); status = 1; continue; }
        printf("block");
        for (size_t e = 0; e < b->nnz; ++e) printf(" %.17g", values[e]);
        printf("\n");
    }
    free(m);
    return status;
}
"#;

/// The Jacobian entries of every projection block of `model`, evaluated by the
/// generated C kernel at the start values.
fn c_block_jacobian_entries(source: &str, file: &str, model: &str) -> Option<Vec<f64>> {
    if !conformance_prerequisites_are_available() {
        return None;
    }
    let headers = standard_roots().1.root.join("headers");
    if !headers.join("fmi3Functions.h").is_file() {
        return None;
    }
    let compiled = match rumoca::Compiler::new()
        .model(model)
        .compile_str(source, file)
    {
        Ok(compiled) => compiled,
        Err(error) => panic!("compile {model}: {error:?}"),
    };
    let work = tempdir().expect("kink harness work directory");
    build_named_fmu(work.path(), &compiled, "fmi3", model);
    let sources = generated_sources(&work.path().join("fmi3"), model);
    let harness = work.path().join("kink_harness.c");
    fs::write(&harness, HARNESS).expect("write the kink harness source");
    let binary = work.path().join("kink_harness");
    checked_output(
        Command::new("cc")
            .arg("-std=c11")
            .arg("-Wall")
            .arg("-Wextra")
            .arg("-Werror")
            .arg(format!("-I{}", sources.display()))
            .arg(format!("-I{}", headers.display()))
            .arg(&harness)
            .args(
                [
                    "rmc_assign.c",
                    "rmc_rows.c",
                    "rmc_jacobian.c",
                    "rmc_isolators.c",
                    "rmc_functions.c",
                ]
                .map(|unit| sources.join(unit)),
            )
            .arg("-lm")
            .arg("-o")
            .arg(&binary),
        "compile the kink harness",
    );
    let output = checked_output(&mut Command::new(&binary), "run the kink harness");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut entries = Vec::new();
    for line in stdout.lines() {
        let Some(values) = line.strip_prefix("block") else {
            panic!("{model}: unexpected harness line {line:?}");
        };
        for value in values.split_whitespace() {
            match value.parse::<f64>() {
                Ok(value) => entries.push(value),
                Err(error) => panic!("{model}: entry {value:?} does not parse: {error}"),
            }
        }
    }
    Some(entries)
}

/// The `sources` directory of the one package the target wrote under `out`.
fn generated_sources(out: &Path, model: &str) -> PathBuf {
    let packages = match fs::read_dir(out) {
        Ok(entries) => entries,
        Err(error) => panic!("{model}: read {}: {error}", out.display()),
    };
    let mut sources = packages
        .filter_map(Result::ok)
        .map(|entry| entry.path().join("sources"))
        .filter(|sources| sources.join("model.c").is_file());
    match (sources.next(), sources.next()) {
        (Some(sources), None) => sources,
        other => panic!(
            "{model}: one generated package under {}: {other:?}",
            out.display()
        ),
    }
}

/// Assert every entry is finite and the magnitudes are `expected` to roundoff,
/// in any order: the matched row order and residual signs are the compiler's.
fn assert_rule_partials(model: &str, entries: &[f64], expected: &[f64]) {
    assert!(
        entries.iter().all(|value| value.is_finite()),
        "{model}: every C Jacobian entry at the kink is finite: {entries:?}"
    );
    let mut magnitudes = entries.iter().map(|value| value.abs()).collect::<Vec<_>>();
    magnitudes.sort_by(f64::total_cmp);
    assert!(
        magnitudes.len() == expected.len()
            && magnitudes
                .iter()
                .zip(expected)
                .all(|(value, expected)| (value - expected).abs() <= 1e-12),
        "{model}: the C Jacobian holds the rule partials {expected:?}: {entries:?}"
    );
}

#[test]
fn generated_projection_jacobian_takes_no_partial_at_the_atan2_origin() {
    let model = "Atan2OriginKink.Loop";
    let Some(entries) = c_block_jacobian_entries(
        include_str!("../../fixtures/sensitivity/Atan2OriginKink.mo"),
        "Atan2OriginKink.mo",
        model,
    ) else {
        return;
    };
    assert_rule_partials(model, &entries, &[0.0, 0.0, 1.0, 1.0]);
}

#[test]
fn generated_projection_jacobian_takes_no_partial_of_a_negative_base_power() {
    let model = "PowNegativeBaseKink.Loop";
    let Some(entries) = c_block_jacobian_entries(
        include_str!("../../fixtures/sensitivity/PowNegativeBaseKink.mo"),
        "PowNegativeBaseKink.mo",
        model,
    ) else {
        return;
    };
    assert_rule_partials(model, &entries, &[0.0, 0.0, 1.0, 1.0]);
}

#[test]
fn generated_projection_jacobian_keeps_the_base_partial_at_a_zero_base() {
    let model = "PowSeededExponentKink.Loop";
    let Some(entries) = c_block_jacobian_entries(
        include_str!("../../fixtures/sensitivity/PowSeededExponentKink.mo"),
        "PowSeededExponentKink.mo",
        model,
    ) else {
        return;
    };
    assert_rule_partials(model, &entries, &[0.0, 0.1, 1.0, 1.1]);
}
