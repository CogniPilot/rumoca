//! `rumoca sim --inspect trajectory-sensitivity|objective-gradient|linearize`:
//! sensitivities over a whole run, trajectory objective gradients, and the
//! state-space linearization at an operating point.

use std::path::Path;

use anyhow::{Result, bail};

use rumoca_compile::compile::Dae;
use rumoca_core::{SourceMap, Span};
use rumoca_sim::{
    DataSeries, RunningKind, RunningTerm, SimOptions, SimResult, SimulationRequestSummary,
    SimulationRunMetrics, TerminalTerm, TrajectoryObjective, TrajectorySession,
};

/// The run a trajectory inspection integrates: window and solver tolerances.
pub(crate) struct TrajectoryRun<'a> {
    pub dae: &'a Dae,
    pub model: &'a str,
    pub window: (f64, f64),
    pub dt: Option<f64>,
    pub atol: Option<f64>,
    pub rtol: Option<f64>,
    /// Parameters to differentiate with respect to; empty selects every
    /// independent tunable parameter the lowered model reads.
    pub wrt: &'a [String],
    /// Bytes the adjoint may spend storing the forward path.
    pub checkpoint_budget: Option<u64>,
    /// The workspace whose plot views the HTML report may load.
    pub workspace_root: Option<&'a Path>,
    /// Source text, to show the relation a refusal names.
    pub source_map: Option<&'a SourceMap>,
}

impl TrajectoryRun<'_> {
    /// Lower once and prove the construction, reporting which parameters a
    /// default request leaves out and why.
    fn session(&self) -> Result<TrajectorySession> {
        let session = TrajectorySession::new(self.dae, &self.options(), self.wrt)
            .map_err(|error| self.failure(&error))?;
        if self.wrt.is_empty() {
            eprintln!(
                "differentiating {} parameter(s): {}",
                session.parameter_names().len(),
                session.parameter_names().join(", ")
            );
            for excluded in session.excluded() {
                eprintln!(
                    "  excluded `{}`: {}",
                    excluded.name,
                    excluded.reason.describe()
                );
            }
        }
        self.report_switching_values(&session);
        Ok(session)
    }

    /// Name each requested parameter that sits exactly on the switching value of
    /// an admitted relation: the sensitivity there is that of one side.
    fn report_switching_values(&self, session: &TrajectorySession) {
        for note in session.switching_value_notes() {
            eprintln!(
                "{}",
                switching_warning(&note.parameter, note.value, note.span, self.source_map)
            );
        }
    }

    /// The failure with the source text of the relation it names.
    fn failure(&self, error: &rumoca_sim::SimulationDiagnosticError) -> anyhow::Error {
        let base = crate::cli::simulation_failure_error(error);
        let located = error
            .source_span()
            .zip(self.source_map)
            .and_then(|(span, map)| describe_span(map, span));
        match located {
            Some(text) => anyhow::anyhow!("{base}\n  at {text}"),
            None => base,
        }
    }

    fn options(&self) -> SimOptions {
        let mut opts = SimOptions {
            t_start: self.window.0,
            t_end: self.window.1,
            dt: self.dt,
            ..SimOptions::default()
        };
        if let Some(atol) = self.atol {
            opts.atol = atol;
        }
        if let Some(rtol) = self.rtol {
            opts.rtol = rtol;
        }
        if let Some(bytes) = self.checkpoint_budget {
            opts.checkpoint_budget_bytes = bytes;
        }
        opts
    }
}

/// The objective terms a trajectory gradient is requested for.
pub(crate) struct ObjectiveRequest<'a> {
    pub integral: &'a [String],
    pub terminal: &'a [String],
    pub fit_data: Option<&'a Path>,
}

impl ObjectiveRequest<'_> {
    pub(crate) fn is_requested(&self) -> bool {
        !self.integral.is_empty() || !self.terminal.is_empty() || self.fit_data.is_some()
    }

    fn objective(&self) -> Result<TrajectoryObjective> {
        let mut running: Vec<RunningTerm> = self
            .integral
            .iter()
            .map(|variable| RunningTerm {
                variable: variable.clone(),
                weight: 1.0,
                kind: RunningKind::Value,
            })
            .collect();
        if let Some(path) = self.fit_data {
            for (variable, series) in read_fit_data(path)? {
                running.push(RunningTerm {
                    variable,
                    weight: 1.0,
                    kind: RunningKind::SquaredError(series),
                });
            }
        }
        let terminal = self
            .terminal
            .iter()
            .map(|variable| TerminalTerm {
                variable: variable.clone(),
                weight: 1.0,
            })
            .collect();
        Ok(TrajectoryObjective { running, terminal })
    }
}

/// The warning for a requested parameter sitting on a relation's switching value.
fn switching_warning(
    parameter: &str,
    value: f64,
    span: Option<Span>,
    map: Option<&SourceMap>,
) -> String {
    let at = span
        .zip(map)
        .and_then(|(span, map)| describe_span(map, span))
        .map_or_else(String::new, |text| format!(" at {text}"));
    format!(
        "warning: parameter `{}` = {} sits exactly on the switching value of a relation{at}; \
         its sensitivity is that of one side of the switch",
        parameter, value
    )
}

/// `file:line: `text`` of the source a span covers.
fn describe_span(map: &SourceMap, span: Span) -> Option<String> {
    let (name, content) = map.get_source(span.source)?;
    let text = content.get(span.start.0..span.end.0)?;
    let line = content.get(..span.start.0)?.matches('\n').count() + 1;
    Some(format!("{name}:{line}: `{}`", text.trim()))
}

/// Read measurements from a CSV: a header `time,<variable>,...` and one row per
/// sample. Each variable column becomes one interpolated series.
pub(crate) fn read_fit_data(path: &Path) -> Result<Vec<(String, DataSeries)>> {
    let text = std::fs::read_to_string(path)
        .map_err(|error| anyhow::anyhow!("cannot read `{}`: {error}", path.display()))?;
    parse_fit_data(&text, &path.display().to_string())
}

fn parse_fit_data(text: &str, label: &str) -> Result<Vec<(String, DataSeries)>> {
    let mut lines = text
        .lines()
        .enumerate()
        .filter(|(_, line)| !line.trim().is_empty());
    let Some((_, header)) = lines.next() else {
        bail!("`{label}` is empty");
    };
    let names: Vec<&str> = header.split(',').map(str::trim).collect();
    if names.len() < 2 || !names[0].eq_ignore_ascii_case("time") {
        bail!("`{label}` needs a header `time,<variable>,...`");
    }
    let mut times = Vec::new();
    let mut columns: Vec<Vec<f64>> = vec![Vec::new(); names.len() - 1];
    for (index, line) in lines {
        let cells: Vec<&str> = line.split(',').map(str::trim).collect();
        if cells.len() != names.len() {
            bail!(
                "`{label}` line {}: {} cells for {} columns",
                index + 1,
                cells.len(),
                names.len()
            );
        }
        let mut values = Vec::with_capacity(cells.len());
        for cell in &cells {
            values.push(cell.parse::<f64>().map_err(|_| {
                anyhow::anyhow!("`{label}` line {}: `{cell}` is not a number", index + 1)
            })?);
        }
        times.push(values[0]);
        for (column, value) in columns.iter_mut().zip(&values[1..]) {
            column.push(*value);
        }
    }
    names[1..]
        .iter()
        .zip(columns)
        .map(|(name, values)| {
            let series = DataSeries::new(times.clone(), values)
                .map_err(|error| anyhow::anyhow!("`{label}` column `{name}`: {error}"))?;
            Ok(((*name).to_string(), series))
        })
        .collect()
}

/// `d(variable)/d(parameter)` over the run: written as result columns to
/// `output` (`.csv`) or summarized at the end of the run.
pub(crate) fn run_trajectory_sensitivity(
    run: &TrajectoryRun<'_>,
    output: Option<&Path>,
    json: bool,
) -> Result<()> {
    let result = run
        .session()?
        .sensitivity()
        .map_err(|error| run.failure(&error))?;
    if let Some(path) = output {
        return write_result(run, &result, path);
    }
    let Some(end) = result.times.len().checked_sub(1) else {
        bail!("the run produced no sample");
    };
    if json {
        let columns: Vec<_> = result
            .names
            .iter()
            .zip(&result.data)
            .map(|(name, series)| serde_json::json!({ "name": name, "value": series[end] }))
            .collect();
        let value = serde_json::json!({
            "model": run.model,
            "t": result.times[end],
            "columns": columns,
        });
        println!("{}", serde_json::to_string_pretty(&value)?);
        return Ok(());
    }
    print_sensitivity_summary(run.model, &result, end);
    Ok(())
}

/// Write the trace and its sensitivity columns as `sim` writes a run: raw `.csv`
/// results or the `.html` report.
fn write_result(run: &TrajectoryRun<'_>, result: &SimResult, path: &Path) -> Result<()> {
    let extension = path
        .extension()
        .map(|ext| ext.to_string_lossy().to_ascii_lowercase());
    match extension.as_deref() {
        Some("csv") => rumoca_sim::report::write_csv_results(result, path)?,
        Some("html") => {
            let opts = run.options();
            let summary = SimulationRequestSummary {
                solver: "rk-like".to_string(),
                t_start: opts.t_start,
                t_end: opts.t_end,
                dt: opts.dt,
                rtol: opts.rtol,
                atol: opts.atol,
            };
            rumoca_sim::report::write_html_report(
                result,
                run.model,
                path,
                &summary,
                &SimulationRunMetrics::default(),
                run.workspace_root,
            )?;
        }
        _ => bail!(
            "unsupported output extension for `{}`: use `.csv` for raw results or `.html` for the \
             report",
            path.display()
        ),
    }
    println!("{}", path.display());
    Ok(())
}

fn print_sensitivity_summary(model: &str, result: &SimResult, end: usize) {
    println!(
        "trajectory sensitivity: model `{model}` at t={} ({} samples)",
        result.times[end],
        result.times.len()
    );
    let mut omitted = 0usize;
    for ((name, series), meta) in result
        .names
        .iter()
        .zip(&result.data)
        .zip(&result.variable_meta)
    {
        // A sensitivity that is zero over the whole run is structural: it is
        // counted, not listed.
        if meta.role == "sensitivity" && series.iter().all(|value| *value == 0.0) {
            omitted += 1;
            continue;
        }
        println!("  {name:<48} = {}", series[end]);
    }
    if omitted > 0 {
        println!("  ({omitted} sensitivities are zero over the whole run)");
    }
}

/// Gradient `dJ/dp` of a trajectory objective, forward or adjoint.
pub(crate) fn run_trajectory_objective_gradient(
    run: &TrajectoryRun<'_>,
    request: &ObjectiveRequest<'_>,
    adjoint: bool,
    json: bool,
) -> Result<()> {
    let objective = request.objective()?;
    let gradient = run
        .session()?
        .gradient(&objective, adjoint)
        .map_err(|error| run.failure(&error))?;
    let mode = if adjoint { "adjoint" } else { "forward" };
    if json {
        let parameters: Vec<_> = gradient
            .parameters
            .iter()
            .zip(&gradient.gradient)
            .map(|(name, value)| serde_json::json!({ "name": name, "gradient": value }))
            .collect();
        let value = serde_json::json!({
            "model": run.model,
            "mode": mode,
            "window": [run.window.0, run.window.1],
            "objective": gradient.value,
            "parameters": parameters,
        });
        println!("{}", serde_json::to_string_pretty(&value)?);
        return Ok(());
    }
    println!(
        "trajectory objective gradient: model `{}` over [{}, {}] via {mode}",
        run.model, run.window.0, run.window.1
    );
    println!("J = {}", gradient.value);
    for (name, value) in gradient.parameters.iter().zip(&gradient.gradient) {
        println!("  dJ/d({name}) = {value}");
    }
    Ok(())
}

/// `A`, `B`, `C`, `D` at the operating point `(overrides, t)`.
pub(crate) fn run_linearize(
    dae: &Dae,
    model: &str,
    overrides: &[(String, f64)],
    t: f64,
    json: bool,
) -> Result<()> {
    let linearization =
        rumoca_sim::linearization_for_dae(dae, &SimOptions::default(), overrides, t)
            .map_err(|error| crate::cli::simulation_failure_error(&error))?;
    if json {
        let value = serde_json::json!({
            "model": model,
            "t": linearization.t,
            "states": linearization.states,
            "inputs": linearization.inputs,
            "outputs": linearization.outputs,
            "state_values": linearization.state_values,
            "input_values": linearization.input_values,
            "A": linearization.a,
            "B": linearization.b,
            "C": linearization.c,
            "D": linearization.d,
        });
        println!("{}", serde_json::to_string_pretty(&value)?);
        return Ok(());
    }
    println!(
        "linearize: model `{model}` at t={}  ({} states, {} inputs, {} outputs)",
        linearization.t,
        linearization.states.len(),
        linearization.inputs.len(),
        linearization.outputs.len()
    );
    print_block(
        "A",
        &linearization.a,
        &linearization.states,
        &linearization.states,
    );
    print_block(
        "B",
        &linearization.b,
        &linearization.states,
        &linearization.inputs,
    );
    print_block(
        "C",
        &linearization.c,
        &linearization.outputs,
        &linearization.states,
    );
    print_block(
        "D",
        &linearization.d,
        &linearization.outputs,
        &linearization.inputs,
    );
    Ok(())
}

fn print_block(label: &str, matrix: &[Vec<f64>], rows: &[String], columns: &[String]) {
    println!("\n{label} ({}x{}):", rows.len(), columns.len());
    for (row, values) in matrix.iter().enumerate() {
        for (col, value) in values.iter().enumerate() {
            if *value != 0.0 {
                println!("  {label}[{}][{}] = {value}", rows[row], columns[col]);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fit_data_columns_become_named_series() {
        let series = parse_fit_data("time,x,y\n0,1,10\n1,2,20\n", "data.csv").expect("valid csv");
        assert_eq!(series.len(), 2);
        assert_eq!(series[0].0, "x");
        assert!((series[1].1.value_at(0.5) - 15.0).abs() < 1.0e-12);
    }

    #[test]
    fn fit_data_rejects_malformed_input() {
        for (text, needle) in [
            ("", "is empty"),
            ("t,x\n0,1\n1,2\n", "needs a header"),
            ("time,x\n0,1,2\n", "cells"),
            ("time,x\n0,a\n", "not a number"),
            ("time,x\n0,1\n", "at least two samples"),
        ] {
            let error = parse_fit_data(text, "d.csv").expect_err(text);
            assert!(error.to_string().contains(needle), "{text:?}: {error}");
        }
    }

    #[test]
    fn a_switching_warning_names_the_parameter_and_the_relation_source() {
        let mut map = SourceMap::new();
        let id = map.add("M.mo", "model M\n  Real x;\nend M;");
        let span = map.try_span(id, 10, 16);
        assert!(span.is_some());
        let located = switching_warning("th", 0.5, span, Some(&map));
        assert!(located.contains("`th` = 0.5"), "{located}");
        assert!(located.contains("at M.mo:2: `Real x`"), "{located}");
        let bare = switching_warning("th", 0.5, None, Some(&map));
        assert!(!bare.contains(" at "), "{bare}");
        assert_eq!(
            describe_span(&map, span.expect("span")).as_deref(),
            Some("M.mo:2: `Real x`")
        );
    }

    #[test]
    fn an_objective_request_reports_whether_any_term_is_present() {
        let none = ObjectiveRequest {
            integral: &[],
            terminal: &[],
            fit_data: None,
        };
        assert!(!none.is_requested());
        let terminal = ["x".to_string()];
        let some = ObjectiveRequest {
            integral: &[],
            terminal: &terminal,
            fit_data: None,
        };
        assert!(some.is_requested());
        let objective = some.objective().expect("terminal term");
        assert_eq!(objective.terminal.len(), 1);
        assert!(objective.running.is_empty());
    }
}
