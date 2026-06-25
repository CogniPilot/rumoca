//! `rumoca sim --inspect` debug dumps: structural analysis, point evaluation
//! (the scriptable NaN trace), and the dense state-Jacobian. Split out of
//! `main.rs` to keep it under the SPEC_0021 file-size limit.

use anyhow::{Result, bail};

use rumoca_compile::compile::Dae;
use rumoca_sim::{SimOptions, SimSolverMode};

/// Parse an `--at` spec of the form `<name=value,...@t>`: comma-separated
/// `state=value` assignments, with the time after `@`. States are addressed by
/// name (never position); unlisted states keep their model initial value, so an
/// empty assignment list (e.g. `@0.5`, or just running it to discover the state
/// names) is valid. The time defaults to 0.
pub(crate) fn parse_eval_at_spec(spec: &str) -> Result<(Vec<(String, f64)>, f64)> {
    let (state_part, time_part) = match spec.split_once('@') {
        Some((state, time)) => (state, Some(time.trim())),
        None => (spec, None),
    };
    let overrides = state_part
        .split(',')
        .map(str::trim)
        .filter(|token| !token.is_empty())
        .map(|token| {
            let (name, value) = token.split_once('=').ok_or_else(|| {
                anyhow::anyhow!(
                    "invalid --at assignment `{token}`: states are set by name, \
                     e.g. `--at \"inductor.i=0.0@0.5\"` (run with no assignments \
                     to list the state names)"
                )
            })?;
            let name = name.trim();
            if name.is_empty() {
                bail!("invalid --at assignment `{token}`: missing state name");
            }
            let value = value.trim().parse::<f64>().map_err(|_| {
                anyhow::anyhow!(
                    "invalid value `{}` for state `{name}` in --at",
                    value.trim()
                )
            })?;
            Ok((name.to_string(), value))
        })
        .collect::<Result<Vec<(String, f64)>>>()?;
    let t = match time_part {
        Some(text) if !text.is_empty() => text
            .parse::<f64>()
            .map_err(|_| anyhow::anyhow!("invalid time `{text}` in --at"))?,
        _ => 0.0,
    };
    Ok((overrides, t))
}

/// Structural-analysis debug dump: lower the model the way the simulator does
/// and print the matching, BLT blocks, coupled SCCs, and tearing by name.
pub(crate) fn run_structure_dump(dae: &Dae, model: &str, solver_mode: SimSolverMode) -> Result<()> {
    let opts = SimOptions {
        solver_mode,
        ..SimOptions::default()
    };
    let report = match rumoca_sim::structural_report_for_dae(dae, &opts) {
        Ok(report) => report,
        Err(error) => {
            // Automated triage: instead of just "structurally singular", explain
            // each unmatched unknown (likely cause + the f_x rows referencing it)
            // so the failure is actionable without hand-reading the DAE.
            println!("structure: model `{model}`");
            if let Ok(Some(diagnosis)) = rumoca_sim::diagnose_structural_singularity(dae, &opts) {
                print_singularity_diagnosis(&diagnosis);
            }
            return Err(anyhow::Error::msg(error));
        }
    };

    println!("structure: model `{model}`");
    print!("{report}");

    println!("\nmatching ({}):", report.matching.len());
    for (equation, unknown) in &report.matching {
        println!("  {unknown:<44} <- {equation}");
    }
    Ok(())
}

/// Print an automated structural-singularity diagnosis: for each unmatched
/// unknown, its likely root-cause category and the `f_x` rows referencing it,
/// plus a category tally that highlights the dominant failure mode.
fn print_singularity_diagnosis(diagnosis: &rumoca_sim::SingularityDiagnosis) {
    use std::collections::BTreeMap;

    println!(
        "structurally singular: {} of {} equations matched ({} unknowns); {} unmatched:",
        diagnosis.n_matched,
        diagnosis.n_equations,
        diagnosis.n_unknowns,
        diagnosis.unknowns.len()
    );
    let mut by_category: BTreeMap<&str, usize> = BTreeMap::new();
    for unknown in &diagnosis.unknowns {
        *by_category.entry(unknown.category.as_str()).or_default() += 1;
        let rows = if unknown.referencing_rows.is_empty() {
            "(none)".to_string()
        } else {
            unknown
                .referencing_rows
                .iter()
                .map(|index| format!("f_x[{index}]"))
                .collect::<Vec<_>>()
                .join(", ")
        };
        println!("  {:<40} {}", unknown.name, unknown.category);
        println!("  {:<40}   referenced by: {rows}", "");
    }
    if !diagnosis.equations.is_empty() {
        println!("\nunmatched equations:");
        for equation in &diagnosis.equations {
            println!(
                "  {:<10} origin='{}'\n             {}",
                equation.name, equation.origin, equation.summary
            );
        }
    }
    println!("\ncategory tally:");
    for (category, count) in &by_category {
        println!("  {count:>3}  {category}");
    }
}

/// State-Jacobian inspector: assemble and print the dense `d(der(state))/d(state)`
/// at named state overrides and a chosen time, naming each row/column by its
/// qualified state name and flagging structurally-singular columns and zero pivots.
pub(crate) fn run_jacobian(
    dae: &Dae,
    model: &str,
    spec: &str,
    solver_mode: SimSolverMode,
    json: bool,
) -> Result<()> {
    let (overrides, t) = parse_eval_at_spec(spec)?;
    let opts = SimOptions {
        solver_mode,
        ..SimOptions::default()
    };
    // One lowering for both Jacobians (lowering can dominate on large models).
    let probe = rumoca_sim::state_and_parameter_jacobian_for_dae(dae, &opts, &overrides, t)
        .map_err(anyhow::Error::msg)?;
    let report = &probe.state;
    let param = &probe.parameter;

    if json {
        print_jacobian_json(model, t, &probe)?;
        return Ok(());
    }

    println!(
        "jacobian: model `{model}` at t={t}  ({0}x{0}, d(der(state))/d(state))",
        report.dim()
    );
    println!("states ({}):", probe.state_names.len());
    for (name, value) in probe.state_names.iter().zip(probe.state_used.iter()) {
        println!("  {name:<44} = {value}");
    }

    let singular = report.singular_columns();
    if singular.is_empty() {
        println!("\nstructurally-singular columns: none");
    } else {
        println!(
            "\nstructurally-singular columns ({}) — no state derivative depends on these:",
            singular.len()
        );
        for col in &singular {
            println!("  {}", report.state_labels[*col]);
        }
    }

    let zero_pivots = report.zero_pivots();
    if zero_pivots.is_empty() {
        println!("zero pivots: none");
    } else {
        println!("zero pivots ({}) — d(der(x))/d(x) = 0:", zero_pivots.len());
        for pivot in &zero_pivots {
            println!("  {}", report.state_labels[*pivot]);
        }
    }

    let entries: Vec<_> = report.nonzero_entries().collect();
    println!("\nnonzero entries ({}):", entries.len());
    for (row, col, value) in entries {
        println!(
            "  d(der({}))/d({}) = {value}",
            report.state_labels[row], report.state_labels[col]
        );
    }

    if let Some(error) = &report.error {
        println!("\njacobian error: {error}");
    }

    // Parameter-sensitivity block (roadmap Track 0.3): d(der(state))/d(p), the
    // forward parameter gradient building block.
    println!(
        "\nparameter jacobian: ({}x{}, d(der(state))/d(p))",
        param.rows(),
        param.cols()
    );
    let param_entries: Vec<_> = param.nonzero_entries().collect();
    println!("nonzero entries ({}):", param_entries.len());
    for (row, col, value) in param_entries {
        println!(
            "  d(der({}))/d({}) = {value}",
            param.row_labels[row], param.param_labels[col]
        );
    }
    if let Some(error) = &param.error {
        println!("parameter jacobian error: {error}");
    }
    Ok(())
}

/// Machine-readable JSON dump of the state Jacobian `∂(der)/∂(state)` and the
/// parameter Jacobian `∂(der)/∂p`, with named rows/columns and dense matrices.
fn print_jacobian_json(
    model: &str,
    t: f64,
    probe: &rumoca_sim::StateAndParameterJacobianProbe,
) -> Result<()> {
    let report = &probe.state;
    let param = &probe.parameter;
    let states: Vec<_> = probe
        .state_names
        .iter()
        .zip(probe.state_used.iter())
        .map(|(name, value)| serde_json::json!({ "name": name, "value": value }))
        .collect();
    let value = serde_json::json!({
        "model": model,
        "t": t,
        "states": states,
        "state_jacobian": {
            "labels": report.state_labels,
            "matrix": report.matrix,
            "error": report.error,
        },
        "parameter_jacobian": {
            "row_labels": param.row_labels,
            "param_labels": param.param_labels,
            "param_slots": param.param_slots,
            "matrix": param.matrix,
            "error": param.error,
        },
    });
    println!("{}", serde_json::to_string_pretty(&value)?);
    Ok(())
}

/// Steady-state objective gradient `d(objective)/dp` for a chosen model variable
/// (a state or solver algebraic), at the `--at` point. `adjoint` selects the
/// reverse-mode adjoint (matrix-free, one solve for all parameters) over the
/// default forward sensitivity. Both produce the same gradient; `--format json`
/// emits the parameter-named result. Exits non-zero if the gradient is undefined.
pub(crate) fn run_objective_gradient(
    dae: &Dae,
    model: &str,
    spec: &str,
    objective: Option<&str>,
    adjoint: bool,
    json: bool,
) -> Result<()> {
    let Some(objective) = objective else {
        bail!("`--inspect objective-gradient` requires `--objective <NAME>`");
    };
    let (overrides, t) = parse_eval_at_spec(spec)?;
    let opts = SimOptions::default();
    let probe = if adjoint {
        rumoca_sim::steady_state_adjoint_objective_gradient_for_dae(
            dae, &opts, &overrides, objective, t,
        )
    } else {
        rumoca_sim::steady_state_objective_gradient_for_dae(dae, &opts, &overrides, objective, t)
    }
    .map_err(anyhow::Error::msg)?;
    let report = &probe.report;
    let mode = if adjoint { "adjoint" } else { "forward" };

    if json {
        let params: Vec<_> = report
            .param_labels
            .iter()
            .zip(&report.param_slots)
            .zip(&report.gradient)
            .map(|((name, slot), value)| {
                serde_json::json!({ "name": name, "slot": slot, "gradient": value })
            })
            .collect();
        let value = serde_json::json!({
            "model": model,
            "objective": objective,
            "mode": mode,
            "t": t,
            "parameters": params,
            "error": report.error,
        });
        println!("{}", serde_json::to_string_pretty(&value)?);
        return match &report.error {
            Some(error) => bail!("objective gradient failed: {error}"),
            None => Ok(()),
        };
    }

    println!("objective gradient: model `{model}`  d({objective})/dp  via {mode}  at t={t}");
    println!("state (used):");
    for (name, value) in probe.state_names.iter().zip(&probe.state_used) {
        println!("  {name:<44} = {value}");
    }
    if let Some(error) = &report.error {
        println!("\nerror: {error}");
        bail!("objective gradient failed: {error}");
    }
    println!(
        "\ngradient d({objective})/dp ({}):",
        report.param_labels.len()
    );
    for (name, value) in report.param_labels.iter().zip(&report.gradient) {
        println!("  d({objective})/d({name}) = {value}");
    }
    Ok(())
}

/// One-shot model evaluation probe: evaluate the compiled model at named state
/// overrides and a chosen time, then dump every solver value and state
/// derivative by name, flagging non-finite (NaN/inf) entries. Exits non-zero if
/// any are found.
pub(crate) fn run_eval_at(
    dae: &Dae,
    model: &str,
    spec: &str,
    solver_mode: SimSolverMode,
) -> Result<()> {
    use rumoca_sim::{eval_dae_at, nan_trace};

    let (overrides, t) = parse_eval_at_spec(spec)?;
    let opts = SimOptions {
        solver_mode,
        ..SimOptions::default()
    };

    // Enable NaN tracing so the runtime also names offending variables (with
    // source spans) during the eval; the report below names them as well.
    nan_trace::set_nan_trace(true);
    let probe = eval_dae_at(dae, &opts, &overrides, t).map_err(anyhow::Error::msg);
    nan_trace::set_nan_trace(false);
    let probe = probe?;
    let report = &probe.report;

    println!("eval: model `{model}` at t={t}");
    println!("states ({}):", probe.state_names.len());
    for (name, value) in probe.state_names.iter().zip(probe.state_used.iter()) {
        println!("  {name:<44} = {value}");
    }

    println!("\nsolver_y ({}):", report.solver_y.len());
    for (index, slot) in report.solver_y.iter().enumerate() {
        let role = if index < report.state_count {
            "state"
        } else {
            "alg"
        };
        let flag = nonfinite_flag_suffix(slot.nonfinite_kind());
        println!(
            "  [{index:>4}] {:<40} = {:<24} [{role}]{flag}",
            slot.name, slot.value
        );
    }

    println!("\nderivatives ({}):", report.derivatives.len());
    for slot in &report.derivatives {
        let flag = nonfinite_flag_suffix(slot.nonfinite_kind());
        println!("  {:<46} = {:<24}{flag}", slot.name, slot.value);
    }

    if let Some(error) = &report.error {
        println!("\neval error: {error}");
    }

    let nonfinite: Vec<_> = report.nonfinite().collect();
    if nonfinite.is_empty() {
        println!(
            "\nall {} solver values and {} derivatives are finite",
            report.solver_y.len(),
            report.derivatives.len()
        );
        return Ok(());
    }

    println!("\nNON-FINITE ({}):", nonfinite.len());
    for (section, slot) in &nonfinite {
        let kind = slot.nonfinite_kind().ok_or_else(|| {
            anyhow::anyhow!("non-finite slot `{}` did not report a kind", slot.name)
        })?;
        println!("  {section}: `{}` = {}", slot.name, kind);
    }
    bail!("eval found {} non-finite value(s)", nonfinite.len());
}

fn nonfinite_flag_suffix(kind: Option<&str>) -> String {
    match kind {
        Some(kind) => format!("  <-- {kind}"),
        None => String::new(),
    }
}
