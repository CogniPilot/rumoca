//! Reduced-chart switching view of one generated component (SPEC_0040
//! STRUCT-T07 constraint-fold chart rows).
//!
//! A component whose reduced chart set carries executable alternates runs each
//! chart's refresh plans, projection blocks, and derivative kernel from the one
//! shared program table, selected by the active chart index. Chart zero is the
//! primary basis; the alternates follow in chart index order, skipping any
//! chart without a lowered plan, which is the order the linked kernel builds
//! its chart runtimes in (`dynamic_chart::build_reduced_charts`). Per chart,
//! this module records what the linked kernel reads at an accepted step and at
//! a basis change: the conditioning matrix over the chart's slope blocks (its
//! rows, columns, dependent positions, and every structurally nonzero entry
//! with the forward-Jacobian program that evaluates it) and the state-binding
//! residual rows the transfer evaluates.

use std::borrow::Cow;

use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext};
use rumoca_ir_solve as solve;
use serde::Serialize;

use super::block::check_seed_loads;
use super::table::ProgramTable;
use crate::errors::CodegenError;

fn refuse(reason: &str) -> CodegenError {
    CodegenError::dae_preparation_failed(
        format!(
            "unsupported-feature:algebraic_projection: {reason}; the generated component \
             cannot reproduce the linked reduced-chart switch"
        ),
        None,
    )
}

/// One executable chart of a component, as the continuous system and
/// artifacts the linked kernel runs it with.
pub(super) struct ChartSystem<'a> {
    /// Index of the chart in the reduced chart set.
    pub(super) index: usize,
    pub(super) problem: Cow<'a, solve::SolveProblem>,
    pub(super) artifacts: Cow<'a, solve::SolveArtifacts>,
}

/// Whether the reduced chart set carries an executable alternate, so the
/// component switches charts.
pub(super) fn switches_charts(problem: &solve::SolveProblem) -> bool {
    problem
        .continuous
        .reduced_chart_set
        .charts
        .iter()
        .any(|chart| chart.plan.is_some())
}

/// The problem of one alternate chart: its carried plan spliced into the shared
/// problem skeleton (`dynamic_chart::alternate_chart_model`).
pub(super) fn alternate_problem(
    problem: &solve::SolveProblem,
    plan: &solve::ReducedChartPlan,
) -> solve::SolveProblem {
    let mut alternate = problem.clone();
    alternate.continuous = solve::ContinuousSolveSystem {
        implicit_rhs: plan.implicit_rhs.clone(),
        implicit_row_targets: plan.implicit_row_targets.clone(),
        algebraic_projection_plan: plan.algebraic_projection_plan.clone(),
        residual: plan.residual.clone(),
        manifold_residual: solve::ComputeBlock::default(),
        manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
        derivative_rhs: plan.derivative_rhs.clone(),
        refresh_owners: plan.refresh_owners.clone(),
        reduced_chart_set: solve::ReducedChartSet::default(),
    };
    alternate
}

/// Every executable chart of a component, primary first.
pub(super) fn chart_systems<'a>(
    problem: &'a solve::SolveProblem,
    artifacts: &'a solve::SolveArtifacts,
) -> Vec<ChartSystem<'a>> {
    let mut systems = vec![ChartSystem {
        index: 0,
        problem: Cow::Borrowed(problem),
        artifacts: Cow::Borrowed(artifacts),
    }];
    if !switches_charts(problem) {
        return systems;
    }
    let charts = &problem.continuous.reduced_chart_set.charts;
    for (index, chart) in charts.iter().enumerate().skip(1) {
        let Some(plan) = &chart.plan else {
            continue;
        };
        let mut chart_artifacts = artifacts.clone();
        chart_artifacts.continuous = plan.artifacts.clone();
        systems.push(ChartSystem {
            index,
            problem: Cow::Owned(alternate_problem(problem, plan)),
            artifacts: Cow::Owned(chart_artifacts),
        });
    }
    systems
}

/// The sorted union of a chart's independent and dependent columns.
fn chart_columns(chart: &solve::ReducedChart) -> Vec<usize> {
    let mut columns = chart
        .independent_y_indices
        .iter()
        .chain(&chart.dependent_y_indices)
        .copied()
        .collect::<Vec<_>>();
    columns.sort_unstable();
    columns.dedup();
    columns
}

/// The point at which the linked kernel resolves each chart's state-binding
/// rows: the retained kernel's initial solver vector and parameters at time
/// zero.
pub(super) struct BindingPoint<'a> {
    pub(super) y: &'a [f64],
    pub(super) p: &'a [f64],
    pub(super) pure_calls: &'a solve::SolvePureCallTable,
    pub(super) state_count: usize,
    pub(super) seed_len: usize,
}

/// One chart's switching descriptor, in the field order of the C `RmcChart`.
#[derive(Serialize, Default)]
pub(super) struct ChartRecord {
    /// Conditioning matrix shape: slope rows by slope columns.
    nrows: usize,
    ncols: usize,
    /// Positions within the columns of the slope blocks' unknowns.
    ndep: usize,
    dep: usize,
    /// Structurally nonzero entries: (row, column, solver-Y seed column,
    /// forward-Jacobian function, output offset) per entry.
    nentries: usize,
    entries: usize,
    /// Per state coordinate, the (residual function, output offset) of its
    /// identity row `state - source`.
    binding: usize,
    /// The chart's conditioning at construction: the primary's keep reference.
    trial_rcond: String,
    /// The nominal of each state while this chart is active.
    nominals: Vec<String>,
    /// Per state, the single source coordinate its binding row integrates.
    #[serde(skip)]
    sources: Vec<Option<usize>>,
    /// Largest outputs of one entry's or one binding row's program.
    jvp_max_outputs: usize,
    row_max_outputs: usize,
}

impl ChartRecord {
    /// Doubles one conditioning evaluation or one transfer allocates.
    pub(super) fn workspace(&self) -> usize {
        self.nrows * (self.ncols + self.ndep) + self.jvp_max_outputs + self.row_max_outputs + 8
    }
}

/// The owners one chart's record reads.
pub(super) struct ChartSources<'a> {
    pub(super) chart: usize,
    pub(super) system: &'a ChartSystem<'a>,
    pub(super) implicit: &'a PreparedScalarProgramBlock,
    /// The primary's reduced chart set; an alternate's own problem carries none.
    pub(super) set: &'a solve::ReducedChartSet,
}

/// The switching descriptor of one chart.
pub(super) fn chart_record(
    sources: &ChartSources<'_>,
    point: &BindingPoint<'_>,
    table: &mut ProgramTable,
) -> Result<ChartRecord, CodegenError> {
    let root = &sources.system.problem.continuous;
    let set = sources.set;
    let chart = set
        .charts
        .get(sources.system.index)
        .ok_or_else(|| refuse("a chart index is outside the reduced chart set"))?;
    let group = chart_columns(&set.charts[0]);
    if chart_columns(chart) != group {
        return Err(refuse("reduced charts span different coordinate groups"));
    }
    let (rows, cols, dependent) = slope_geometry(
        &root.algebraic_projection_plan,
        &chart.dependent_y_indices,
        &group,
    )?;
    let jvp_block = rumoca_eval_solve::to_scalar_program_block(
        &sources.system.artifacts.continuous.implicit_jacobian_v,
    )
    .map_err(|error| refuse(&error.to_string()))?;
    let jvp = PreparedScalarProgramBlock::new(jvp_block.clone())
        .map_err(|error| refuse(&error.to_string()))?;
    let mut record = ChartRecord {
        nrows: rows.len(),
        ncols: cols.len(),
        ndep: dependent.len(),
        dep: table.push(dependent),
        trial_rcond: format!("{:?}", chart.trial_rcond),
        ..ChartRecord::default()
    };
    record_entries(
        sources,
        (&jvp_block, &jvp),
        (&rows, &cols),
        point,
        table,
        &mut record,
    )?;
    record_binding(sources, &jvp, point, table, &mut record)?;
    Ok(record)
}

/// Slope rows, conditioning columns, and the unknowns' positions among them.
type SlopeGeometry = (Vec<usize>, Vec<usize>, Vec<usize>);

/// The rows and unknowns of every projection block of a chart's plan that
/// reconstructs one of its folding coordinates, the sorted conditioning
/// columns (those unknowns plus the group columns), and the unknowns'
/// positions among them (`dynamic_chart::slope_geometry`).
fn slope_geometry(
    plan: &solve::AlgebraicProjectionPlan,
    dependent: &[usize],
    group: &[usize],
) -> Result<SlopeGeometry, CodegenError> {
    let blocks = plan
        .blocks
        .iter()
        .filter(|block| block.y_indices.iter().any(|y| dependent.contains(y)))
        .collect::<Vec<_>>();
    if blocks.is_empty() {
        return Err(refuse(
            "a reduced chart has no reconstruction block for its dependent coordinate",
        ));
    }
    let rows = blocks
        .iter()
        .flat_map(|block| block.rows.iter().copied())
        .collect::<Vec<_>>();
    let unknowns = blocks
        .iter()
        .flat_map(|block| block.y_indices.iter().copied())
        .collect::<Vec<_>>();
    let mut cols = unknowns.iter().chain(group).copied().collect::<Vec<_>>();
    cols.sort_unstable();
    cols.dedup();
    let positions = unknowns
        .iter()
        .map(|unknown| cols.iter().position(|col| col == unknown))
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| refuse("a slope unknown is outside the conditioning columns"))?;
    Ok((rows, cols, positions))
}

/// Every conditioning entry the implicit pattern does not prove zero
/// (`refresh_projection.rs: folding_jacobian_entry`), with the row's
/// forward-Jacobian program over a unit seed of the entry's column.
fn record_entries(
    sources: &ChartSources<'_>,
    (jvp_block, jvp): (&solve::ScalarProgramBlock, &PreparedScalarProgramBlock),
    (rows, cols): (&[usize], &[usize]),
    point: &BindingPoint<'_>,
    table: &mut ProgramTable,
    record: &mut ChartRecord,
) -> Result<(), CodegenError> {
    let pattern = sources
        .system
        .artifacts
        .continuous
        .structural
        .implicit()
        .map(solve::JacobianStructure::pattern);
    let source = table.jvp_source(jvp_block);
    let mut entries = Vec::new();
    record.jvp_max_outputs = 1;
    for (r, &row) in rows.iter().enumerate() {
        let (program, offset) = jvp
            .row_output_position(row)
            .ok_or_else(|| refuse("a folding residual row has no scalar Jacobian view"))?;
        for (c, &col) in cols.iter().enumerate() {
            if pattern.is_some_and(|pattern| !pattern.contains(row as u32, col as u32)) {
                continue;
            }
            let function = table.jvp.intern((source, program), || {
                let operations = jvp_block.programs()[program].clone();
                check_seed_loads(row, &operations, point.seed_len, 1)?;
                let span = jvp_block
                    .program_span(program)
                    .ok_or_else(|| refuse("a folding Jacobian program has no provenance"))?;
                Ok((operations, span))
            })?;
            record.jvp_max_outputs = record.jvp_max_outputs.max(table.jvp.output_count(function));
            entries.extend([r, c, col, function, offset]);
        }
    }
    record.nentries = entries.len() / 5;
    record.entries = table.push(entries);
    Ok(())
}

/// Per state coordinate, the unique implicit residual row whose Jacobian with
/// respect to that state column is a unit pivot at the instantiation point
/// (`refresh_projection.rs: implicit_state_binding_rows`), as the residual
/// program the transfer evaluates.
fn record_binding(
    sources: &ChartSources<'_>,
    jvp: &PreparedScalarProgramBlock,
    point: &BindingPoint<'_>,
    table: &mut ProgramTable,
    record: &mut ChartRecord,
) -> Result<(), CodegenError> {
    let row_count = sources.system.problem.continuous.implicit_row_targets.len();
    let solver_count = sources.system.problem.solve_layout.solver_scalar_count();
    let mut seed = vec![0.0; solver_count];
    let mut functions = Vec::with_capacity(2 * point.state_count);
    record.row_max_outputs = 1;
    for state in 0..point.state_count {
        let slot = seed
            .get_mut(state)
            .ok_or_else(|| refuse("a state coordinate is out of solver range"))?;
        *slot = 1.0;
        let row = unit_binding_row(jvp, row_count, point, &seed)?;
        seed[state] = 0.0;
        let (program, offset) = sources
            .implicit
            .row_output_position(row)
            .ok_or_else(|| refuse("a state-binding row has no scalar view"))?;
        let block = sources.implicit.block();
        let function = table.rows.intern((sources.chart, program), || {
            let span = block
                .program_span(program)
                .ok_or_else(|| refuse("a state-binding program has no provenance"))?;
            Ok((block.programs()[program].clone(), span))
        })?;
        record.row_max_outputs = record
            .row_max_outputs
            .max(table.rows.output_count(function));
        functions.extend([function, offset]);
        record
            .sources
            .push(binding_source(&block.programs()[program], state));
    }
    record.binding = table.push(functions);
    Ok(())
}

/// The one solver coordinate other than `state` a binding program reads, if
/// it reads exactly one (`refresh_projection.rs: binding_row_source`).
fn binding_source(program: &[solve::LinearOp], state: usize) -> Option<usize> {
    let mut sources = program
        .iter()
        .filter_map(|op| match op {
            solve::LinearOp::LoadY { index, .. } if *index != state => Some(*index),
            _ => None,
        })
        .collect::<Vec<_>>();
    sources.sort_unstable();
    sources.dedup();
    match sources.as_slice() {
        [source] => Some(*source),
        _ => None,
    }
}

/// Every chart's state nominals (`dynamic_chart.rs: alternate_state_nominals`):
/// the primary's are the solver scales of its states; an alternate state whose
/// binding row integrates another source than the primary's takes that
/// source's scale and otherwise keeps the primary's nominal.
pub(super) fn assign_nominals(records: &mut [ChartRecord], scale: impl Fn(usize) -> f64) {
    let Some((primary, alternates)) = records.split_first_mut() else {
        return;
    };
    let primary_nominals = (0..primary.sources.len()).map(&scale).collect::<Vec<_>>();
    for record in alternates {
        record.nominals = record
            .sources
            .iter()
            .enumerate()
            .map(|(state, &source)| match source {
                Some(source) if Some(source) != primary.sources[state] => scale(source),
                _ => primary_nominals[state],
            })
            .map(|value| format!("{value:?}"))
            .collect();
    }
    primary.nominals = primary_nominals
        .iter()
        .map(|value| format!("{value:?}"))
        .collect();
}

/// The one row whose seeded Jacobian exceeds one half in magnitude.
fn unit_binding_row(
    jvp: &PreparedScalarProgramBlock,
    row_count: usize,
    point: &BindingPoint<'_>,
    seed: &[f64],
) -> Result<usize, CodegenError> {
    let mut binding = None;
    for row in 0..row_count {
        let Some((program, offset)) = jvp.row_output_position(row) else {
            continue;
        };
        let context = RowEvalContext {
            seed: Some(seed),
            pure_calls: Some(point.pure_calls),
            ..RowEvalContext::default()
        };
        let value = jvp
            .eval_row_output_unchecked_with_context(program, offset, point.y, point.p, 0.0, context)
            .map_err(|error| refuse(&format!("a state-binding row does not evaluate: {error}")))?;
        if value.abs() > 0.5 {
            if binding.is_some() {
                return Err(refuse(
                    "a state coordinate binds more than one implicit residual row",
                ));
            }
            binding = Some(row);
        }
    }
    binding.ok_or_else(|| refuse("a state coordinate has no identity residual row"))
}
