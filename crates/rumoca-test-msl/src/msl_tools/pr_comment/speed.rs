//! The "Speed vs OMC" section of the MSL PR comment (SPEC_0025 §4a speed
//! report).
//!
//! Every published aggregate is taken over the trace-agreeing models (both
//! tools produced traces in the `high` or `near` band), with a high-band-only
//! line beside it. Compilation is compared two ways so each number compares like
//! with like: the time until the model can run (rumoca's JIT against OMC's C
//! toolchain) and the FMU path. A methodology block states the recorded
//! conditions of both tools' runs.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use anyhow::{Context, Result};
use serde_json::Value;

use super::{json_f64, json_str, json_u64, nonnegative_json_f64, positive_json_f64};

const SIZE_BINS: &[(u64, Option<u64>)] = &[
    (1, Some(9)),
    (10, Some(24)),
    (25, Some(49)),
    (50, Some(99)),
    (100, Some(249)),
    (250, None),
];
const SLOWER_MODEL_LIMIT: usize = 10;

/// Rumoca's seconds for one model, from its results row.
#[derive(Clone, Default)]
struct RumocaSeconds {
    instantiate: f64,
    typecheck: f64,
    flatten: f64,
    dae: f64,
    /// Front end through the checked DAE (`compile_seconds`).
    front_end: f64,
    /// Solve lowering (`ir_solve_seconds`).
    solve: f64,
    /// Native code generation (`sim_backend_build_seconds`).
    jit: f64,
    /// Solve lowering, JIT, and preparation (`sim_build_seconds`).
    sim_build: f64,
    /// Initialization (`ic_seconds`), timed apart from the integration.
    initialization: f64,
    run: f64,
}

/// OMC's seconds for one model, from its self-reported `SimulationResult`.
#[derive(Clone, Default)]
struct OmcSeconds {
    /// `timeTotal - timeSimulation`: OMC's compiler plus its C toolchain.
    runnable: f64,
    simulation: f64,
    total: f64,
}

#[derive(Clone)]
struct SpeedRecord {
    model_name: String,
    scalar_equations: u64,
    high: bool,
    rumoca: RumocaSeconds,
    omc: OmcSeconds,
    rumoca_settings: Option<Value>,
    omc_settings: Option<Value>,
    /// Whether the compile started from the worker's prepared resolution
    /// plan (`strict_plan_warm`), `None` when the row does not record it.
    plan_warm: Option<bool>,
}

/// One compared quantity.
#[derive(Clone, Copy)]
enum Comparison {
    Total,
    Runnable,
    Simulation,
}

const COMPARISONS: [Comparison; 3] = [
    Comparison::Total,
    Comparison::Runnable,
    Comparison::Simulation,
];

impl Comparison {
    fn label(self) -> &'static str {
        match self {
            Self::Total => "Total (model to results)",
            Self::Runnable => "Time to runnable (JIT vs C toolchain)",
            Self::Simulation => "Simulation",
        }
    }

    fn definition(self) -> &'static str {
        match self {
            Self::Total => {
                "Rumoca = front end + Solve lowering + JIT + initialization + integration; OMC = `timeTotal`."
            }
            Self::Runnable => {
                "Rumoca = front end + Solve lowering + Cranelift JIT (`compile_seconds + \
                 sim_build_seconds`); OMC = `timeTotal - timeSimulation`, which includes \
                 `timeCompile`, the C compiler and linker."
            }
            Self::Simulation => {
                "Rumoca = initialization + integration (`ic_seconds + sim_run_seconds`); OMC = \
                 `timeSimulation`, which includes its initialization. Both include writing the \
                 output: N grid intervals (N + 1 points) by the same rule, plus event points."
            }
        }
    }

    fn rumoca(self, record: &SpeedRecord) -> f64 {
        let r = &record.rumoca;
        match self {
            Self::Total => r.front_end + r.sim_build + r.initialization + r.run,
            Self::Runnable => r.front_end + r.sim_build,
            Self::Simulation => r.initialization + r.run,
        }
    }

    fn omc(self, record: &SpeedRecord) -> f64 {
        let o = &record.omc;
        match self {
            Self::Total => o.total,
            Self::Runnable => o.runnable,
            Self::Simulation => o.simulation,
        }
    }

    /// `(rumoca, omc)` seconds when both are known and positive.
    fn pair(self, record: &SpeedRecord) -> Option<(f64, f64)> {
        let (rumoca, omc) = (self.rumoca(record), self.omc(record));
        (rumoca > 0.0 && omc > 0.0).then_some((rumoca, omc))
    }

    /// The rumoca phase that took longest for `record` within this comparison.
    fn dominant_cost(self, record: &SpeedRecord) -> String {
        let r = &record.rumoca;
        let named = r.instantiate + r.typecheck + r.flatten + r.dae;
        let front_end = [
            (
                "resolve and other front end",
                (r.front_end - named).max(0.0),
            ),
            ("instantiate", r.instantiate),
            ("typecheck", r.typecheck),
            ("flatten", r.flatten),
            ("DAE", r.dae),
        ];
        let phases: Vec<(&str, f64)> = match self {
            Self::Runnable | Self::Total => front_end
                .into_iter()
                .chain([("Solve lowering", r.solve), ("JIT", r.jit)])
                .chain(matches!(self, Self::Total).then_some(("initialization", r.initialization)))
                .chain(matches!(self, Self::Total).then_some(("integration", r.run)))
                .collect(),
            Self::Simulation => return simulation_work(record.rumoca_settings.as_ref()),
        };
        phases
            .into_iter()
            .max_by(|left, right| left.1.total_cmp(&right.1))
            .map_or_else(String::new, |(name, seconds)| {
                format!("{name} {seconds:.3} s")
            })
    }
}

/// The recorded integration work of a rumoca simulation.
fn simulation_work(settings: Option<&Value>) -> String {
    let Some(settings) = settings else {
        return "work not recorded".to_string();
    };
    format!(
        "{} steps, {} events, {} output points",
        json_u64(settings, "steps").unwrap_or(0),
        json_u64(settings, "events").unwrap_or(0),
        json_u64(settings, "output_points").unwrap_or(0),
    )
}

/// Render the speed section from the joined MSL timing artifacts.
pub(super) fn render_speed_section(results_dir: &Path) -> Result<String> {
    let Some(inputs) = SpeedInputs::read(results_dir)? else {
        return Ok(
            "_Speed vs OMC not measured: the run published no joined timing inputs._\n".to_string(),
        );
    };
    let (records, other_context) = inputs.records();
    if records.is_empty() {
        return Ok("_No agreeing models had valid timings on both tools._\n".to_string());
    }
    let mut out = String::new();
    let high = records.iter().filter(|record| record.high).count();
    out.push_str(&format!(
        "Timed over the **{} trace-agreeing models** (both traces in the `high` or `near` \
         band: {high} high, {} near). `Speedup = OMC / Rumoca` (>1 means rumoca faster).\n\n",
        records.len(),
        records.len() - high,
    ));
    out.push_str("##### Aggregate\n\n");
    out.push_str(&render_aggregate_table(&records));
    out.push_str(&render_high_only_line(&records));
    out.push_str(FMU_PATH_LINE);
    out.push('\n');
    out.push_str(&render_methodology(&inputs, &records, other_context));
    out.push_str("\n<details>\n<summary><strong>Speed by system size</strong></summary>\n");
    for comparison in COMPARISONS {
        out.push('\n');
        out.push_str(&render_size_table(&records, comparison));
    }
    out.push_str("\n</details>\n\n");
    out.push_str(&render_slower_than_omc(&records));
    Ok(out)
}

const FMU_PATH_LINE: &str = "\n**FMU path:** not measured in CI; no CI job exports and builds FMUs \
     for timing. Local recipe: `rumoca compile <file> -m <Model> --target fmi3 --output <dir>` \
     plus the C build of the emitted sources, against OMC's `buildModelFMU(<Model>, version=\"3.0\")`.\n";

/// The three result artifacts the section joins: rumoca's rows, OMC's
/// reference, and the comparator's band table, whose `band` is the one owner
/// of agreement (SPEC_0033 §6a).
struct SpeedInputs {
    rumoca: Vec<Value>,
    omc: serde_json::Map<String, Value>,
    omc_timing: Value,
    bands: Vec<Value>,
}

impl SpeedInputs {
    fn read(results_dir: &Path) -> Result<Option<Self>> {
        let paths = [
            "msl_results.json",
            "omc_simulation_reference.json",
            "msl_band_table.json",
        ]
        .map(|name| results_dir.join(name));
        if !paths.iter().all(|path| path.is_file()) {
            return Ok(None);
        }
        let msl = read_json_file(&paths[0])?;
        let omc = read_json_file(&paths[1])?;
        let table = read_json_file(&paths[2])?;
        let (Some(rumoca), Some(omc_models), Some(bands)) = (
            msl.get("model_results").and_then(Value::as_array),
            omc.get("models").and_then(Value::as_object),
            table.get("rows").and_then(Value::as_array),
        ) else {
            return Ok(None);
        };
        Ok(Some(Self {
            rumoca: rumoca.clone(),
            omc: omc_models.clone(),
            omc_timing: omc.get("timing").cloned().unwrap_or(Value::Null),
            bands: bands.clone(),
        }))
    }

    /// The timed models: in the comparator's `high` or `near` band, with
    /// valid timings on both tools, and with an OMC timing taken in this run's
    /// context; the second value counts agreeing models excluded because their
    /// OMC timing came from another context (a cache of another run).
    fn records(&self) -> (Vec<SpeedRecord>, usize) {
        let rumoca_by_name: BTreeMap<&str, &Value> = self
            .rumoca
            .iter()
            .filter_map(|model| Some((json_str(model, "model_name")?, model)))
            .collect();
        let mut records = Vec::new();
        let mut other_context = 0;
        for row in &self.bands {
            let (Some(name), Some(band)) = (json_str(row, "model_name"), json_str(row, "band"))
            else {
                continue;
            };
            if band != "high" && band != "near" {
                continue;
            }
            let (Some(rumoca), Some(omc)) = (rumoca_by_name.get(name), self.omc.get(name)) else {
                continue;
            };
            if !self.timed_in_this_context(omc) {
                other_context += 1;
                continue;
            }
            records.extend(speed_record(name, band == "high", rumoca, omc));
        }
        (records, other_context)
    }

    /// Whether `omc`'s timing was taken under this run's worker count, OMC
    /// threads, and host.
    fn timed_in_this_context(&self, omc: &Value) -> bool {
        let Some(context) = omc.get("omc_timing_context") else {
            return false;
        };
        let timing = &self.omc_timing;
        let host = |value: &Value| {
            value.get("host").map(|host| {
                (
                    host.get("image").cloned(),
                    host.get("logical_cpus").cloned(),
                )
            })
        };
        context.get("workers") == timing.get("workers_used")
            && context.get("omc_threads") == timing.get("omc_threads")
            && host(context) == host(timing)
    }
}

fn speed_record(name: &str, high: bool, rumoca: &Value, omc: &Value) -> Option<SpeedRecord> {
    if json_str(omc, "status")? != "success" {
        return None;
    }
    let seconds = |key| json_f64(rumoca, key).unwrap_or(0.0);
    let rumoca_seconds = RumocaSeconds {
        instantiate: seconds("instantiate_seconds"),
        typecheck: seconds("typecheck_seconds"),
        flatten: seconds("flatten_seconds"),
        dae: seconds("dae_seconds"),
        front_end: positive_json_f64(rumoca, "compile_seconds")?,
        solve: seconds("ir_solve_seconds"),
        jit: seconds("sim_backend_build_seconds"),
        sim_build: nonnegative_json_f64(rumoca, "sim_build_seconds")?,
        initialization: seconds("ic_seconds"),
        run: positive_json_f64(rumoca, "sim_run_seconds")?,
    };
    let total = positive_json_f64(omc, "total_system_seconds")?;
    let simulation = positive_json_f64(omc, "sim_system_seconds")?;
    let omc_seconds = OmcSeconds {
        runnable: Some(total - simulation).filter(|seconds| *seconds > 0.0)?,
        simulation,
        total,
    };
    Some(SpeedRecord {
        model_name: name.to_string(),
        scalar_equations: json_u64(rumoca, "scalar_equations")?,
        high,
        rumoca: rumoca_seconds,
        omc: omc_seconds,
        rumoca_settings: rumoca.get("sim_settings").cloned(),
        omc_settings: omc.get("omc_settings").cloned(),
        plan_warm: rumoca.get("strict_plan_warm").and_then(Value::as_bool),
    })
}

/// Throughput and median per-model speedup of `comparison` over `records`.
fn speedups(
    records: &[SpeedRecord],
    comparison: Comparison,
) -> Option<(usize, f64, f64, f64, f64)> {
    let pairs: Vec<(f64, f64)> = records.iter().filter_map(|r| comparison.pair(r)).collect();
    if pairs.is_empty() {
        return None;
    }
    let rumoca: f64 = pairs.iter().map(|pair| pair.0).sum();
    let omc: f64 = pairs.iter().map(|pair| pair.1).sum();
    let median = median_value(pairs.iter().map(|(rumoca, omc)| omc / rumoca));
    Some((pairs.len(), rumoca, omc, omc / rumoca, median))
}

fn render_aggregate_table(records: &[SpeedRecord]) -> String {
    let mut out = String::from(
        "| Comparison | Models | Rumoca total (s) | OMC total (s) | Throughput speedup (×) | Median per-model speedup (×) |\n\
         |---|--:|--:|--:|--:|--:|\n",
    );
    for comparison in COMPARISONS {
        let row = match speedups(records, comparison) {
            Some((models, rumoca, omc, throughput, median)) => format!(
                "| {} | {models} | {rumoca:.1} | {omc:.1} | **{throughput:.2}** | **{median:.2}** |\n",
                comparison.label()
            ),
            None => format!(
                "| {} | 0 | - | - | not measured | not measured |\n",
                comparison.label()
            ),
        };
        out.push_str(&row);
    }
    out.push('\n');
    for comparison in COMPARISONS {
        out.push_str(&format!(
            "- **{}**: {}\n",
            comparison.label(),
            comparison.definition()
        ));
    }
    out
}

fn render_high_only_line(records: &[SpeedRecord]) -> String {
    let high: Vec<SpeedRecord> = records.iter().filter(|r| r.high).cloned().collect();
    let cells: Vec<String> = COMPARISONS
        .into_iter()
        .map(|comparison| match speedups(&high, comparison) {
            Some((_, _, _, _, median)) => format!("{} {median:.2}×", comparison.label()),
            None => format!("{} not measured", comparison.label()),
        })
        .collect();
    format!(
        "\nHigh band only ({} models), median per-model speedup: {}.\n",
        high.len(),
        cells.join("; ")
    )
}

fn render_size_table(records: &[SpeedRecord], comparison: Comparison) -> String {
    let mut out = format!(
        "##### {}\n\n{}\n\n| Scalar eqns | Models | Rumoca median (s) | OMC median (s) | Median speedup (×) |\n|---|--:|--:|--:|--:|\n",
        comparison.label(),
        comparison.definition()
    );
    for (label, bin) in speed_bins(records) {
        let pairs: Vec<(f64, f64)> = bin.iter().filter_map(|r| comparison.pair(r)).collect();
        if pairs.is_empty() {
            continue;
        }
        out.push_str(&format!(
            "| {label} | {} | {:.4} | {:.4} | **{:.2}** |\n",
            pairs.len(),
            median_value(pairs.iter().map(|pair| pair.0)),
            median_value(pairs.iter().map(|pair| pair.1)),
            median_value(pairs.iter().map(|(rumoca, omc)| omc / rumoca)),
        ));
    }
    out
}

fn render_slower_than_omc(records: &[SpeedRecord]) -> String {
    let mut out = String::from(
        "<details>\n<summary><strong>Slower than OMC, per comparison</strong></summary>\n",
    );
    for comparison in COMPARISONS {
        let mut slower: Vec<(&SpeedRecord, f64)> = records
            .iter()
            .filter_map(|record| {
                let (rumoca, omc) = comparison.pair(record)?;
                (omc < rumoca).then_some((record, omc / rumoca))
            })
            .collect();
        slower.sort_by(|left, right| left.1.total_cmp(&right.1));
        out.push_str(&format!(
            "\n##### {} ({} slower)\n\n",
            comparison.label(),
            slower.len()
        ));
        if slower.is_empty() {
            out.push_str("_None._\n");
            continue;
        }
        out.push_str(
            "| Model | Scalar eqns | Speedup (×) | Dominant rumoca cost |\n|---|--:|--:|---|\n",
        );
        for (record, speedup) in slower.into_iter().take(SLOWER_MODEL_LIMIT) {
            out.push_str(&format!(
                "| `{}` | {} | {speedup:.2} | {} |\n",
                record.model_name,
                record.scalar_equations,
                comparison.dominant_cost(record)
            ));
        }
    }
    out.push_str("\n</details>\n");
    out
}

/// The recorded conditions of both tools' runs.
fn render_methodology(
    inputs: &SpeedInputs,
    records: &[SpeedRecord],
    other_context: usize,
) -> String {
    let timing = &inputs.omc_timing;
    let count = |key| json_u64(timing, key).map_or("not recorded".to_string(), |v| v.to_string());
    let host = timing.get("host");
    let host_field = |key| {
        host.and_then(|host| host.get(key))
            .filter(|value| !value.is_null())
            .map_or("not recorded".to_string(), |value| {
                value
                    .as_str()
                    .map_or_else(|| value.to_string(), str::to_string)
            })
    };
    let omc_workers = json_u64(timing, "workers_used");
    let ran = json_u64(timing, "batches_ran").unwrap_or(0);
    let total = json_u64(timing, "batches_total").unwrap_or(0);
    format!(
        "\n<details>\n<summary><strong>Methodology</strong></summary>\n\n\
         - Runner: image {}, environment {}, {} logical CPUs, {} physical cores; {} shard(s).\n\
         - Parallelism per shard: rumoca compile stage {}, rumoca simulations {}, OMC reference \
         compile and simulation {} (OMC threads {}); compile contention {}; simulation contention {}.\n\
         - Solver and tolerance: rumoca {}; OMC {}.\n\
         - Output density: {}\n\
         - Initialization: {:.0}% of rumoca's simulation seconds; OMC's `timeSimulation` \
         includes its initialization.\n\
         - Cache: OMC references ran for {ran} of {total} models this run ({} reused from cache); \
         {other_context} agreeing models are excluded because their OMC timing was taken under \
         another worker count, thread count, or host. Rumoca compiled and simulated every model \
         this run.\n\
         - Front end scope: rumoca loads the library and builds its resolution plan once per \
         worker (`worker_prepare_seconds`, recorded on each row with `strict_plan_warm`), and \
         `compile_seconds` covers resolving each model's reachable library classes for that \
         model; OMC loads the library once per session with `loadModel`, outside every \
         per-model timer. {}\n\
         - Parity gating: only models in the comparator's high or near band are timed; {} models.\
         \n\n</details>\n",
        host_field("image"),
        host_field("runner_environment"),
        host_field("logical_cpus"),
        host_field("physical_cores"),
        count("shards"),
        count("rumoca_stage_workers"),
        count("rumoca_sim_workers"),
        count("workers_used"),
        count("omc_threads"),
        contention_flag(
            "compile",
            omc_workers,
            json_u64(timing, "rumoca_stage_workers")
        ),
        contention_flag(
            "simulation",
            omc_workers,
            json_u64(timing, "rumoca_sim_workers")
        ),
        setting_summary(records, true, &["integrator", "requested_solver", "rtol"]),
        setting_summary(records, false, &["method", "tolerance"]),
        density_flag(records),
        initialization_share(records),
        total.saturating_sub(ran),
        plan_scope(records),
        records.len(),
    )
}

/// Whether `stage` ran as many tasks at a time on both tools.
fn contention_flag(stage: &str, omc: Option<u64>, rumoca: Option<u64>) -> String {
    match (omc, rumoca) {
        (Some(omc), Some(rumoca)) if omc == rumoca => "equal on both tools".to_string(),
        (Some(omc), Some(rumoca)) if omc > rumoca => format!(
            "**unequal**: OMC ran {omc} {stage} tasks at a time against rumoca's {rumoca}, which favours rumoca"
        ),
        (Some(omc), Some(rumoca)) => format!(
            "**unequal**: rumoca ran {rumoca} {stage} tasks at a time against OMC's {omc}, which favours OMC"
        ),
        _ => format!("**not comparable**: a {stage} worker count was not recorded"),
    }
}

/// Whether each timed model's output grid had as many intervals on both
/// tools (N intervals are N + 1 points; event points come on top).
fn density_flag(records: &[SpeedRecord]) -> String {
    let intervals = |settings: Option<&Value>, key| settings.and_then(|s| json_u64(s, key));
    let (mut equal, mut unequal, mut unrecorded) = (0, 0, 0);
    for record in records {
        let rumoca = intervals(record.rumoca_settings.as_ref(), "output_intervals");
        let omc = intervals(record.omc_settings.as_ref(), "number_of_intervals");
        match (rumoca, omc) {
            (Some(rumoca), Some(omc)) if rumoca == omc => equal += 1,
            (Some(_), Some(_)) => unequal += 1,
            _ => unrecorded += 1,
        }
    }
    let flag = if unequal == 0 && unrecorded == 0 {
        "equal on every model".to_string()
    } else {
        format!(
            "**unequal or unrecorded on {} models**",
            unequal + unrecorded
        )
    };
    format!(
        "{flag}: {equal} models with the same number of grid intervals on both tools, \
         {unequal} different, {unrecorded} not recorded. Both apply OMC's rule (the \
         experiment `Interval`, else 500 intervals); rumoca's {}, OMC's {}.",
        setting_summary(records, true, &["output_intervals"]),
        setting_summary(records, false, &["number_of_intervals"]),
    )
}

/// Rumoca's initialization seconds as a share of its simulation seconds.
fn initialization_share(records: &[SpeedRecord]) -> f64 {
    let initialization: f64 = records.iter().map(|r| r.rumoca.initialization).sum();
    let simulation: f64 = records
        .iter()
        .map(|r| r.rumoca.initialization + r.rumoca.run)
        .sum();
    if simulation > 0.0 {
        100.0 * initialization / simulation
    } else {
        0.0
    }
}

/// The most common value of each `keys` setting across `records`, with its
/// share, for rumoca's rows (`rumoca`) or OMC's.
fn setting_summary(records: &[SpeedRecord], rumoca: bool, keys: &[&str]) -> String {
    keys.iter()
        .map(|key| {
            let mut counts: BTreeMap<String, usize> = BTreeMap::new();
            for record in records {
                let settings = if rumoca {
                    record.rumoca_settings.as_ref()
                } else {
                    record.omc_settings.as_ref()
                };
                let value = settings
                    .and_then(|settings| settings.get(*key))
                    .filter(|value| !value.is_null())
                    .map_or("not recorded".to_string(), |value| {
                        value
                            .as_str()
                            .map_or_else(|| value.to_string(), str::to_string)
                    });
                *counts.entry(value).or_default() += 1;
            }
            let (value, count) = counts
                .into_iter()
                .max_by_key(|(_, count)| *count)
                .unwrap_or_else(|| ("not recorded".to_string(), 0));
            format!("{key} {value} ({count}/{})", records.len())
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn speed_bins(records: &[SpeedRecord]) -> Vec<(String, Vec<&SpeedRecord>)> {
    SIZE_BINS
        .iter()
        .filter_map(|(low, high)| {
            let in_bin = records
                .iter()
                .filter(|record| {
                    record.scalar_equations >= *low
                        && high.is_none_or(|high| record.scalar_equations <= high)
                })
                .collect::<Vec<_>>();
            let label = high.map_or_else(|| format!("{low}+"), |high| format!("{low}–{high}"));
            (!in_bin.is_empty()).then_some((label, in_bin))
        })
        .collect()
}

fn read_json_file(path: &Path) -> Result<Value> {
    serde_json::from_str(
        &fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?,
    )
    .with_context(|| format!("failed to parse {}", path.display()))
}

fn median_value(values: impl IntoIterator<Item = f64>) -> f64 {
    let mut values: Vec<f64> = values.into_iter().filter(|v| v.is_finite()).collect();
    if values.is_empty() {
        return 0.0;
    }
    values.sort_by(f64::total_cmp);
    let middle = values.len() / 2;
    if values.len().is_multiple_of(2) {
        (values[middle - 1] + values[middle]) / 2.0
    } else {
        values[middle]
    }
}

#[cfg(test)]
mod tests;

/// What the rows record about the one-time resolution plan: a compile that
/// built the plan itself carries that one-time cost in its `compile_seconds`.
fn plan_scope(records: &[SpeedRecord]) -> String {
    let cold = records
        .iter()
        .filter(|r| r.plan_warm == Some(false))
        .count();
    let unrecorded = records.iter().filter(|r| r.plan_warm.is_none()).count();
    if cold == 0 && unrecorded == 0 {
        return "Every timed rumoca compile started from the prepared plan, so neither one-time \
                load is in a per-model timer."
            .to_string();
    }
    format!(
        "{cold} timed rumoca compile(s) built the plan themselves and {unrecorded} row(s) do not \
         record it; a plan built in the compile is in that model's `compile_seconds`."
    )
}
