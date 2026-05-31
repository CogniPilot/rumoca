//! Rumoca-vs-OMC compile-speed scalability comparison.
//!
//! Joins per-model rumoca compile seconds (from the rumoca results JSON) with
//! OMC's self-reported compile time (`timeTotal - timeSimulation`) and produces
//! a two-series scalability curve: for each model-size bin, the average and
//! median compile time for rumoca and for OMC, so the two can be plotted
//! against each other (the slower-growing curve scales better).
//!
//! The scaling axis is `scalar_equations` (flattened square system size), the
//! workload both compilers actually process. Continuous states are *not* the
//! axis: most MSL example models are algebraic (0 states), so equation count is
//! the meaningful size; states/algebraics are reported as secondary columns.

use std::collections::{BTreeSet, HashMap};

use anyhow::Result;
use rumoca_sim::viz_web::{UPLOT_CSS, UPLOT_JS};
use serde_json::json;

use super::super::common::{MslPaths, round3, write_pretty_json};
use super::{RumocaRuntime, SimModelResult, SimRunState};

/// One model with both compilers' compile times and its size descriptors.
struct ModelSpeedRecord {
    scalar_equations: usize,
    num_states: usize,
    rumoca_compile: f64,
    omc_compile: f64,
}

impl ModelSpeedRecord {
    /// Rumoca's compile speedup over OMC for this model:
    /// `omc_compile_seconds / rumoca_compile_seconds`. Equivalently the ratio of
    /// speeds (rumoca_speed / omc_speed). A value of `N` means rumoca compiled
    /// this model `N` times faster than OMC; `< 1` means OMC was faster.
    fn speedup(&self) -> f64 {
        self.omc_compile / self.rumoca_compile
    }
}

/// Inclusive `[low, high]` bounds defining the scaling bins.
const SIZE_BINS: &[(usize, usize)] = &[
    (1, 9),
    (10, 24),
    (25, 49),
    (50, 99),
    (100, 249),
    (250, usize::MAX),
];

const STATE_BINS: &[(usize, usize)] = &[(0, 0), (1, 2), (3, 5), (6, 10), (11, usize::MAX)];

pub(super) fn write_and_print_speed_comparison(
    paths: &MslPaths,
    rumoca_runtimes: &HashMap<String, RumocaRuntime>,
    state: &SimRunState,
    agreeing_models: &BTreeSet<String>,
) -> Result<()> {
    let records = collect_speed_records(rumoca_runtimes, state, agreeing_models);
    let size_curve = scaling_curve(&records, SIZE_BINS, |record| record.scalar_equations);
    let state_curve = scaling_curve(&records, STATE_BINS, |record| record.num_states);

    // Timing artifacts:
    //  * `msl_speed_comparison.json` — the machine-readable data contract, with
    //    an `_about` block defining every metric (see [`metric_definitions`]).
    //  * `msl_speed_scaling.html` — a self-contained local scalability plot
    //    using the same embedded uPlot backend as `plot-compare`.
    // The PR-comment table + mermaid plot are rendered by `cargo xtask repo msl
    // pr-comment` from the JSON, so the PR plot is a deliberate step, not a
    // side effect of every run. Rich per-stage IR lives in `debug-model`.
    write_pretty_json(
        &paths.results_dir.join("msl_speed_comparison.json"),
        &build_payload(&records, &size_curve, &state_curve),
    )?;
    std::fs::write(
        paths.results_dir.join("msl_speed_scaling.html"),
        generate_scaling_html(&records),
    )?;
    print_speed_comparison(&records, &size_curve);
    Ok(())
}

/// Self-contained local scalability plot using the same embedded uPlot backend
/// as `plot-compare` (`rumoca_sim::viz_web`). One scatter point per model:
/// x = flattened scalar equations, y = compile seconds, two series (rumoca, OMC).
fn generate_scaling_html(records: &[ModelSpeedRecord]) -> String {
    let mut sorted: Vec<&ModelSpeedRecord> = records.iter().collect();
    sorted.sort_by_key(|record| record.scalar_equations);
    let xs: Vec<f64> = sorted.iter().map(|r| r.scalar_equations as f64).collect();
    let rumoca_ys: Vec<f64> = sorted.iter().map(|r| r.rumoca_compile).collect();
    let omc_ys: Vec<f64> = sorted.iter().map(|r| r.omc_compile).collect();
    let data_json = serde_json::to_string(&[xs, rumoca_ys, omc_ys])
        .unwrap_or_else(|_| "[[],[],[]]".to_string());
    let app_js = format!(
        "const data = {data_json};\n\
         const opts = {{\n\
           title: 'Compile time vs system size ({n} agreeing models) — rumoca vs OMC',\n\
           width: Math.max(640, window.innerWidth - 48), height: Math.max(420, window.innerHeight - 96),\n\
           scales: {{ x: {{ time: false }} }},\n\
           axes: [ {{ label: 'flattened scalar equations' }}, {{ label: 'compile seconds' }} ],\n\
           series: [\n\
             {{}},\n\
             {{ label: 'rumoca', stroke: '#4ec9b0', paths: () => null, points: {{ show: true, size: 6 }} }},\n\
             {{ label: 'OMC', stroke: '#d16969', paths: () => null, points: {{ show: true, size: 6 }} }},\n\
           ],\n\
         }};\n\
         new uPlot(opts, data, document.getElementById('plot'));\n",
        n = records.len()
    );
    format!(
        "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n\
         <meta charset=\"utf-8\">\n<title>Rumoca vs OMC compile scalability</title>\n\
         <style>{UPLOT_CSS}</style>\n\
         <style>body{{margin:0;padding:12px;background:#1e1e1e;color:#d4d4d4;font-family:monospace}}</style>\n\
         </head>\n<body>\n<div id=\"plot\"></div>\n\
         <script>{UPLOT_JS}</script>\n<script>{app_js}</script>\n\
         </body>\n</html>"
    )
}

fn collect_speed_records(
    rumoca_runtimes: &HashMap<String, RumocaRuntime>,
    state: &SimRunState,
    agreeing_models: &BTreeSet<String>,
) -> Vec<ModelSpeedRecord> {
    let mut records = Vec::new();
    for (model_name, result) in &state.all_results {
        if result.status != "success" {
            continue;
        }
        // Only compare compile speed where both tools produced a trace AND the
        // traces agree (high/near band); otherwise we would be timing rumoca
        // compiling a result that does not match OMC.
        if !agreeing_models.contains(model_name) {
            continue;
        }
        let Some(runtime) = rumoca_runtimes.get(model_name) else {
            continue;
        };
        let Some(rumoca_compile) = runtime.compile_seconds.filter(|value| *value > 0.0) else {
            continue;
        };
        let Some(scalar_equations) = runtime.scalar_equations else {
            continue;
        };
        let Some(omc_compile) = omc_compile_seconds(result) else {
            continue;
        };
        records.push(ModelSpeedRecord {
            scalar_equations,
            num_states: runtime.num_states.unwrap_or(0),
            rumoca_compile,
            omc_compile,
        });
    }
    records
}

/// OMC compile time = total time minus the integration time it self-reports.
fn omc_compile_seconds(result: &SimModelResult) -> Option<f64> {
    let total = result.total_system_seconds?;
    let sim = result.sim_system_seconds.unwrap_or(0.0);
    let compile = total - sim;
    (compile > 0.0).then_some(compile)
}

/// One bin of a two-series scaling curve.
struct ScalingBin {
    low: usize,
    high: usize,
    models: usize,
    rumoca_mean: f64,
    rumoca_median: f64,
    omc_mean: f64,
    omc_median: f64,
    ratio_median: f64,
}

impl ScalingBin {
    fn label(&self) -> String {
        if self.high == usize::MAX {
            format!("{}+", self.low)
        } else {
            format!("{}-{}", self.low, self.high)
        }
    }
}

/// Build a scaling curve over `bins`, keyed by `axis(record)`.
fn scaling_curve(
    records: &[ModelSpeedRecord],
    bins: &[(usize, usize)],
    axis: impl Fn(&ModelSpeedRecord) -> usize,
) -> Vec<ScalingBin> {
    bins.iter()
        .filter_map(|&(low, high)| {
            let in_bin: Vec<&ModelSpeedRecord> = records
                .iter()
                .filter(|record| {
                    let key = axis(record);
                    key >= low && key <= high
                })
                .collect();
            if in_bin.is_empty() {
                return None;
            }
            Some(ScalingBin {
                low,
                high,
                models: in_bin.len(),
                rumoca_mean: mean(in_bin.iter().map(|record| record.rumoca_compile)),
                rumoca_median: median(in_bin.iter().map(|record| record.rumoca_compile))
                    .unwrap_or(0.0),
                omc_mean: mean(in_bin.iter().map(|record| record.omc_compile)),
                omc_median: median(in_bin.iter().map(|record| record.omc_compile)).unwrap_or(0.0),
                ratio_median: median(in_bin.iter().map(|record| record.speedup())).unwrap_or(0.0),
            })
        })
        .collect()
}

fn build_payload(
    records: &[ModelSpeedRecord],
    size_curve: &[ScalingBin],
    state_curve: &[ScalingBin],
) -> serde_json::Value {
    let speedups: Vec<f64> = records.iter().map(ModelSpeedRecord::speedup).collect();
    json!({
        "_about": metric_definitions(),
        "models_compared": records.len(),
        "speedup": ratio_stats_json(&speedups),
        "scaling_by_scalar_equations": curve_json(size_curve),
        "scaling_by_continuous_states": curve_json(state_curve),
    })
}

/// Embedded, authoritative definition of every metric in this artifact: how it
/// is computed, its units, and its direction. Kept in the JSON so the file is
/// self-describing and the table/plot consumers never have to guess.
fn metric_definitions() -> serde_json::Value {
    json!({
        "purpose": "Rumoca-vs-OMC compile-time comparison over MSL example models that BOTH tools compiled successfully. Drives the compile-speed table and the scalability plots.",
        "rumoca_compile_seconds": "Rumoca front-end-through-DAE compile time (`compile_seconds` from the rumoca results JSON). Cranelift JIT; no external C compiler.",
        "omc_compile_seconds": "OMC compile time = `timeTotal - timeSimulation` from OMC's self-reported SimulationResult record (frontend+backend+simcode+templates+C-compile; excludes the integration run).",
        "speedup": "Per model: omc_compile_seconds / rumoca_compile_seconds (equivalently rumoca_speed / omc_speed). Value N means rumoca compiled N times faster; <1 means OMC was faster. The top-level `speedup` reports min/median/max of these per-model values.",
        "scaling_axis": "Models are binned by `scalar_equations` (the flattened square system size = scalar_unknowns), the workload both compilers process. `scaling_by_continuous_states` is a SECONDARY view only; most MSL examples have 0 continuous states, so it collapses.",
        "scaling.<tool>_compile_seconds.median": "Robust typical compile time for the bin (median of per-model seconds).",
        "scaling.<tool>_compile_seconds.mean": "Mean compile time for the bin; compared with the median it exposes large-model/outlier influence.",
        "scaling.speedup_median": "Median of per-model speedup within the bin. NOTE: median-of-ratios != ratio-of-medians, so this is reported directly, not derived from the median seconds.",
        "bin_low / bin_high_inclusive": "Inclusive bin bounds on the axis named by the containing key; bin_high_inclusive is null for the open top bin.",
        "agreement_bands": {
            "channel": "Per output channel, OMC vs rumoca traces are scored by a bounded-normalized L1 error in [0,1]: high if error <= 0.05, near (minor) if <= 0.20, else deviation.",
            "high": "Model band: >=80% of channels are `high` AND <=1% are `deviation`.",
            "near": "Model band (a.k.a. minor): >=90% of channels are `high`+`near` AND <=10% are `deviation`. (`near` here is the same band counted as `minor_agreement` elsewhere.)",
            "deviation": "Model band: anything not meeting the `high` or `near` thresholds.",
        },
        "inclusion": "A model is included ONLY if (1) both OMC and rumoca produced a trace and those traces are in the `high` or `near` agreement band, AND (2) it has a positive rumoca compile time, a known scalar_equations, and an OMC success with positive (timeTotal - timeSimulation). Restricting to agreeing models ensures we compare compile time for results that actually match, not rumoca compiling a wrong answer quickly. `models_compared` is that count and the denominator for every statistic here.",
    })
}

fn curve_json(curve: &[ScalingBin]) -> serde_json::Value {
    let bins: Vec<_> = curve
        .iter()
        .map(|bin| {
            json!({
                "bin_low": bin.low,
                "bin_high_inclusive": (bin.high != usize::MAX).then_some(bin.high),
                "models": bin.models,
                "rumoca_compile_seconds": {
                    "median": round3(bin.rumoca_median),
                    "mean": round3(bin.rumoca_mean),
                },
                "omc_compile_seconds": {
                    "median": round3(bin.omc_median),
                    "mean": round3(bin.omc_mean),
                },
                "speedup_median": round4(bin.ratio_median),
            })
        })
        .collect();
    serde_json::Value::Array(bins)
}

fn ratio_stats_json(ratios: &[f64]) -> serde_json::Value {
    match ratio_stats(ratios) {
        Some((min, median, max)) => json!({
            "samples": ratios.len(),
            "min": round4(min),
            "median": round4(median),
            "max": round4(max),
        }),
        None => json!({ "samples": 0 }),
    }
}

fn print_speed_comparison(records: &[ModelSpeedRecord], size_curve: &[ScalingBin]) {
    println!();
    if records.is_empty() {
        println!("=== Rumoca vs OMC Compile Speed ===");
        println!("  no models with both rumoca and OMC compile timing available");
        return;
    }
    let speedups: Vec<f64> = records.iter().map(ModelSpeedRecord::speedup).collect();
    println!(
        "=== Rumoca vs OMC Compile Speed ({} models) ===",
        records.len()
    );
    println!("speedup = OMC_compile_s / Rumoca_compile_s  (N = rumoca compiled N x faster)");
    if let Some((min, median, max)) = ratio_stats(&speedups) {
        println!("  speedup: min={min:.3} median={median:.3} max={max:.3}");
    }
    println!("Scalability by flattened scalar equations (median compile seconds per bin):");
    println!(
        "  {:>12}  {:>6}  {:>11}  {:>11}  {:>10}",
        "scalar_eqs", "models", "rumoca_s", "omc_s", "speedup"
    );
    for bin in size_curve {
        println!(
            "  {:>12}  {:>6}  {:>11.4}  {:>11.4}  {:>10.3}",
            bin.label(),
            bin.models,
            bin.rumoca_median,
            bin.omc_median,
            bin.ratio_median
        );
    }
}

fn ratio_stats(ratios: &[f64]) -> Option<(f64, f64, f64)> {
    if ratios.is_empty() {
        return None;
    }
    let min = ratios.iter().copied().fold(f64::INFINITY, f64::min);
    let max = ratios.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    let median = median(ratios.iter().copied())?;
    Some((min, median, max))
}

fn mean(values: impl Iterator<Item = f64>) -> f64 {
    let values: Vec<f64> = values.filter(|value| value.is_finite()).collect();
    if values.is_empty() {
        return 0.0;
    }
    values.iter().sum::<f64>() / values.len() as f64
}

fn median(values: impl Iterator<Item = f64>) -> Option<f64> {
    let mut values: Vec<f64> = values.filter(|value| value.is_finite()).collect();
    if values.is_empty() {
        return None;
    }
    values.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let len = values.len();
    let median = if len.is_multiple_of(2) {
        (values[len / 2 - 1] + values[len / 2]) / 2.0
    } else {
        values[len / 2]
    };
    Some(median)
}

fn round4(value: f64) -> f64 {
    (value * 10_000.0).round() / 10_000.0
}

#[cfg(test)]
mod tests {
    use super::*;

    fn record(scalar_equations: usize, states: usize, rumoca: f64, omc: f64) -> ModelSpeedRecord {
        ModelSpeedRecord {
            scalar_equations,
            num_states: states,
            rumoca_compile: rumoca,
            omc_compile: omc,
        }
    }

    #[test]
    fn speedup_is_omc_compile_over_rumoca_compile() {
        let r = record(10, 0, 0.5, 1.5);
        assert!((r.speedup() - 3.0).abs() < 1e-9);
    }

    #[test]
    fn ratio_stats_min_median_max() {
        let stats = ratio_stats(&[3.0, 1.0, 2.0]).expect("stats");
        assert_eq!(stats, (1.0, 2.0, 3.0));
    }

    #[test]
    fn scaling_curve_bins_two_series_by_size() {
        let records = vec![
            record(5, 0, 0.1, 0.2),
            record(8, 0, 0.2, 0.4),
            record(30, 2, 1.0, 0.5),
            record(300, 8, 5.0, 2.0),
        ];
        let curve = scaling_curve(&records, SIZE_BINS, |r| r.scalar_equations);
        let small = curve.iter().find(|bin| bin.low == 1).expect("1-9 bin");
        assert_eq!(small.models, 2);
        assert!((small.rumoca_median - 0.15).abs() < 1e-9);
        assert!((small.omc_median - 0.30).abs() < 1e-9);
        assert!((small.rumoca_mean - 0.15).abs() < 1e-9);
        let big = curve.iter().find(|bin| bin.low == 250).expect("250+ bin");
        assert_eq!(big.models, 1);
        assert!((big.ratio_median - 0.4).abs() < 1e-9);
    }

    #[test]
    fn scaling_curve_bins_by_states_too() {
        let records = vec![
            record(5, 0, 0.1, 0.2),
            record(8, 0, 0.2, 0.4),
            record(30, 2, 1.0, 0.5),
        ];
        let curve = scaling_curve(&records, STATE_BINS, |r| r.num_states);
        let zero = curve.iter().find(|bin| bin.high == 0).expect("0 bin");
        assert_eq!(zero.models, 2);
        let small = curve.iter().find(|bin| bin.low == 1).expect("1-2 bin");
        assert_eq!(small.models, 1);
    }
}
