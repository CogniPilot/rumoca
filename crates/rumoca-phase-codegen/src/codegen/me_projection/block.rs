//! Execution descriptor of one canonical algebraic projection block.
//!
//! Every datum mirrors one query the linked projection model answers at run
//! time: the block's residual rows and their implicit targets, the issued
//! colored Jacobian application, the tearing's certified causal isolators,
//! the affine elimination layout, and the unchecked isolation of every row
//! for every block unknown. Each is read from its construction owner once and
//! recorded as program indices and pool ranges of the shared [`ProgramTable`].

use std::collections::BTreeMap;

use rumoca_core::Span;
use rumoca_eval_solve::projection_policy::affine_elimination_capacity;
use rumoca_eval_solve::{PreparedScalarProgramBlock, TargetIsolationProgram};
use rumoca_ir_solve as solve;
use serde::Serialize;

use super::table::ProgramTable;
use crate::errors::CodegenError;

/// Solve owners every block descriptor reads.
pub(super) struct BlockSources<'a> {
    pub(super) problem: &'a solve::SolveProblem,
    pub(super) artifacts: &'a solve::SolveArtifacts,
    pub(super) implicit: &'a PreparedScalarProgramBlock,
    pub(super) seed_len: usize,
}

/// Isolation kinds as the C kernel encodes them.
const ISOLATION_UNAVAILABLE: usize = 0;
const ISOLATION_OUTPUT_VALUE: usize = 1;
const ISOLATION_PROGRAM: usize = 2;
/// The pool's encoding of a residual row without an implicit target; a row
/// with a target stores its solver-Y index plus one.
const NO_TARGET: usize = 0;
/// Causal run kinds as the C kernel encodes them: a multi-step isolation
/// chain, or one step answered by its row's grouped isolator.
const RUN_CHAIN: usize = 0;
const RUN_ISOLATOR: usize = 1;

/// One block descriptor: counts, flags, and pool offsets, in the field order
/// of the C `RmcBlock`.
#[derive(Serialize, Default)]
pub(super) struct BlockRecord {
    canonical: usize,
    n: usize,
    y: usize,
    row_target: usize,
    row_program: usize,
    row_offset: usize,
    max_outputs: usize,
    affine: bool,
    singleton_exact: bool,
    has_tearing: bool,
    row_ptr: usize,
    col_idx: usize,
    nnz: usize,
    ncolors: usize,
    colors: usize,
    color_seeds: usize,
    color_calls: usize,
    placements: usize,
    jvp_max_outputs: usize,
    torn: bool,
    k: usize,
    tear_col: usize,
    residual_row: usize,
    ncausal: usize,
    ncruns: usize,
    cruns: usize,
    causal_target: usize,
    causal_col: usize,
    tear_deps: usize,
    elimination: bool,
    nelim: usize,
    elim_row: usize,
    elim_col: usize,
    nelim_tear: usize,
    elim_residual: usize,
    elim_tear: usize,
    nguards: usize,
    guards: usize,
    guard_step: usize,
    elim_capacity: usize,
    iso_default: usize,
    iso_start: usize,
    iso_entries: usize,
    nlane_calls: usize,
    lane_calls: usize,
    lane_max_outputs: usize,
    lane_max: usize,
}

pub(super) fn refuse(canonical: usize, reason: &str) -> CodegenError {
    CodegenError::dae_preparation_failed(
        format!(
            "unsupported-feature:algebraic_projection: projection block {canonical} {reason}; \
             the generated component cannot reproduce the linked ME projection"
        ),
        None,
    )
}

/// Compressed-row pattern of one block Jacobian in the pattern's own visit
/// order, which is also the order the affine elimination layout sums in.
struct Csr {
    row_ptr: Vec<usize>,
    col_idx: Vec<usize>,
    positions: BTreeMap<(usize, usize), usize>,
}

impl Csr {
    fn position(&self, canonical: usize, row: usize, column: usize) -> Result<usize, CodegenError> {
        self.positions
            .get(&(row, column))
            .copied()
            .ok_or_else(|| refuse(canonical, "places a Jacobian entry outside its pattern"))
    }

    fn new(pattern: &solve::StructuralPattern, n: usize) -> Self {
        let mut row_ptr = vec![0];
        let mut col_idx = Vec::new();
        let mut positions = BTreeMap::new();
        for row in 0..n {
            pattern.visit_row_columns(row, |column| {
                positions.insert((row, column), col_idx.len());
                col_idx.push(column);
            });
            row_ptr.push(col_idx.len());
        }
        Self {
            row_ptr,
            col_idx,
            positions,
        }
    }
}

pub(super) fn block_record(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
) -> Result<BlockRecord, CodegenError> {
    let block = sources
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .get(canonical)
        .ok_or_else(|| refuse(canonical, "is outside the canonical projection plan"))?;
    let structure = sources
        .artifacts
        .continuous
        .structural
        .algebraic_projection()
        .get(canonical)
        .ok_or_else(|| refuse(canonical, "has no construction-issued Jacobian structure"))?;
    let n = block.rows.len();
    if n == 0 || n != block.y_indices.len() {
        return Err(refuse(canonical, "is not a non-empty square block"));
    }
    let positions = row_positions(sources, canonical, block)?;
    let affine = sources
        .problem
        .continuous
        .refresh_owners
        .algebraic_projection_block_is_affine(canonical);
    let singleton_exact = n == 1
        && sources.implicit.certifies_exact_target_assignment_output(
            positions[0].0,
            positions[0].1,
            block.y_indices[0],
        );
    let mut record = BlockRecord {
        canonical,
        n,
        affine,
        singleton_exact,
        has_tearing: block.tearing.is_some(),
        y: table.push(block.y_indices.iter().copied()),
        row_target: table.push(
            block
                .rows
                .iter()
                .map(|&row| row_target(sources.problem, row)),
        ),
        ..BlockRecord::default()
    };
    record_rows(sources, table, canonical, &positions, &mut record)?;
    let csr = Csr::new(structure.pattern(), n);
    record.row_ptr = table.push(csr.row_ptr.iter().copied());
    record.col_idx = table.push(csr.col_idx.iter().copied());
    record.nnz = csr.col_idx.len();
    // An exact singleton settles or declines through its isolator alone; no
    // path of the linked projection linearizes it.
    if !singleton_exact {
        record_jacobian(
            sources,
            table,
            canonical,
            block,
            structure,
            &csr,
            &mut record,
        )?;
    }
    record_torn(sources, table, canonical, block, &mut record)?;
    if affine {
        record_elimination(table, canonical, block, structure, &csr, &mut record)?;
    }
    // A coupled affine block is always settled by the affine solve; its rows
    // reach isolation only through the singleton path.
    let isolation_reachable = !(affine && n > 1);
    record_isolation(
        sources,
        table,
        canonical,
        block,
        &positions,
        isolation_reachable,
        &mut record,
    )?;
    Ok(record)
}

fn row_target(problem: &solve::SolveProblem, row: usize) -> usize {
    match problem
        .continuous
        .implicit_row_targets
        .get(row)
        .copied()
        .flatten()
    {
        Some(solve::ScalarSlot::Y { index, .. }) => index + 1,
        _ => NO_TARGET,
    }
}

/// Each block row's (scalar-projection program, output offset).
fn row_positions(
    sources: &BlockSources<'_>,
    canonical: usize,
    block: &solve::AlgebraicProjectionBlock,
) -> Result<Vec<(usize, usize)>, CodegenError> {
    block
        .rows
        .iter()
        .map(|&row| {
            sources
                .implicit
                .row_output_position(row)
                .ok_or_else(|| refuse(canonical, "has a residual row without a scalar view"))
        })
        .collect()
}

fn record_rows(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
    positions: &[(usize, usize)],
    record: &mut BlockRecord,
) -> Result<(), CodegenError> {
    let source = sources.implicit.block();
    let mut functions = Vec::with_capacity(positions.len());
    record.max_outputs = 1;
    for &(program, _) in positions {
        let function = table.rows.intern(program, || {
            Ok((
                source.programs()[program].clone(),
                program_span(source, canonical, program)?,
            ))
        })?;
        record.max_outputs = record.max_outputs.max(table.rows.output_count(function));
        functions.push(function);
    }
    record.row_program = table.push(functions);
    record.row_offset = table.push(positions.iter().map(|&(_, offset)| offset));
    Ok(())
}

/// The issued colored forward application of the block Jacobian, flattened
/// into seed, call, and placement ranges over the compressed-row pattern.
// SPEC_0021: Exception - the Jacobian record threads the sources, table, block, structure, pattern, and record together.
#[allow(clippy::too_many_arguments)]
fn record_jacobian(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
    block: &solve::AlgebraicProjectionBlock,
    structure: &solve::JacobianStructure,
    csr: &Csr,
    record: &mut BlockRecord,
) -> Result<(), CodegenError> {
    let application = structure
        .jacobian_application()
        .filter(|application| {
            application.rows() == block.rows && application.y_indices() == block.y_indices
        })
        .ok_or_else(|| refuse(canonical, "has no issued colored Jacobian application"))?;
    let source = application.source();
    let source_id = table.jvp_source(source);
    let n = block.rows.len();
    let (mut colors, mut seeds, mut calls, mut placements) =
        (Vec::new(), Vec::new(), Vec::new(), Vec::new());
    record.jvp_max_outputs = 1;
    for color in application.colors() {
        let seed_start = seeds.len();
        seeds.extend_from_slice(color.seed_indices());
        let call_start = calls.len() / 3;
        for call in color.outputs().programs() {
            let function = table.jvp.intern((source_id, call.program()), || {
                let operations = source
                    .programs()
                    .get(call.program())
                    .ok_or_else(|| refuse(canonical, "names a missing JVP program"))?
                    .clone();
                check_seed_loads(canonical, &operations, sources.seed_len)?;
                Ok((operations, program_span(source, canonical, call.program())?))
            })?;
            record.jvp_max_outputs = record.jvp_max_outputs.max(table.jvp.output_count(function));
            let placement_start = placements.len() / 2;
            for &(offset, target) in call.placements() {
                placements.extend([offset, csr.position(canonical, target % n, target / n)?]);
            }
            calls.extend([function, placement_start, placements.len() / 2]);
        }
        colors.extend([seed_start, seeds.len(), call_start, calls.len() / 3]);
    }
    record.ncolors = colors.len() / 4;
    record.colors = table.push(colors);
    record.color_seeds = table.push(seeds);
    record.color_calls = table.push(calls);
    record.placements = table.push(placements);
    record_lane_calls(sources, table, canonical, (application, csr), record)
}

/// The colored application as one multi-lane call per program
/// ([`solve::ColoredTangentPlan`]): `(function, lanes, colors, placements,
/// placement count, lane outputs)` per call, with `(lane, output offset,
/// pattern position)` placements. Left empty when the plan does not construct
/// or the policy keeps the one-direction colors.
fn record_lane_calls(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
    (application, csr): (&solve::ProjectionJacobianApplication, &Csr),
    record: &mut BlockRecord,
) -> Result<(), CodegenError> {
    if !rumoca_eval_solve::projection_policy::COLORED_TANGENT_LANES {
        return Ok(());
    }
    let Ok(plan) = solve::ColoredTangentPlan::derive(application) else {
        return Ok(());
    };
    let n = application.rows().len();
    let span = program_span(application.source(), canonical, 0)?;
    let mut calls = Vec::new();
    for call in plan.calls() {
        let lanes = call.colors.len();
        let program = plan.programs()[call.program].clone();
        check_seed_loads(canonical, program.ops(), sources.seed_len * lanes)?;
        let outputs = program.lane_outputs();
        let function = table.lanes.push(program, span);
        let colors = table.push(call.colors.iter().copied());
        let mut placements = Vec::with_capacity(3 * call.placements.len());
        for &(lane, offset, destination) in call.placements.iter() {
            let position = csr.position(canonical, destination % n, destination / n)?;
            placements.extend([lane, offset, position]);
        }
        let placement_start = table.push(placements);
        calls.extend([
            function,
            lanes,
            colors,
            placement_start,
            call.placements.len(),
            outputs,
        ]);
        record.lane_max_outputs = record.lane_max_outputs.max(lanes * outputs);
        record.lane_max = record.lane_max.max(lanes);
    }
    record.nlane_calls = calls.len() / 6;
    record.lane_calls = table.push(calls);
    Ok(())
}

fn check_seed_loads(
    canonical: usize,
    operations: &[solve::LinearOp],
    seed_len: usize,
) -> Result<(), CodegenError> {
    for operation in operations {
        match operation {
            solve::LinearOp::LoadSeed { index, .. } if *index >= seed_len => {
                return Err(refuse(
                    canonical,
                    "loads a seed outside the solver-Y and parameter lanes",
                ));
            }
            solve::LinearOp::TensorLoad {
                seed_start: Some(start),
                count,
                ..
            } if start.checked_add(*count).is_none_or(|end| end > seed_len) => {
                return Err(refuse(
                    canonical,
                    "loads a seed range outside the solver-Y and parameter lanes",
                ));
            }
            solve::LinearOp::LoadIndexedSeed { .. } => {
                return Err(refuse(canonical, "loads a dynamically indexed seed"));
            }
            _ => {}
        }
    }
    Ok(())
}

/// The tearing's certified batched sweep. It is recorded only when the linked
/// torn solve can run: an uncertified causal step, a constant singular
/// coefficient, or an unbalanced tear set makes the linked kernel decline on
/// every call and fall through to its next path, exactly as the generated one
/// does without a sweep.
fn record_torn(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
    block: &solve::AlgebraicProjectionBlock,
    record: &mut BlockRecord,
) -> Result<(), CodegenError> {
    let Some(tearing) = block.tearing.as_ref() else {
        return Ok(());
    };
    if tearing.tear_y_indices.len() != tearing.residual_rows.len() {
        return Ok(());
    }
    let causal = tearing
        .causal_steps
        .iter()
        .map(|step| (step.row, step.y_index))
        .collect::<Vec<_>>();
    if sources
        .implicit
        .prepare_torn_sweep(&causal, &tearing.residual_rows)
        .and_then(|sweep| sources.implicit.torn_sweep_composite(&sweep))
        .is_none()
    {
        return Ok(());
    }
    let local_row = |row: usize| block.rows.iter().position(|&candidate| candidate == row);
    let local_col = |y: usize| block.y_indices.iter().position(|&candidate| candidate == y);
    let local =
        |indices: &mut dyn Iterator<Item = Option<usize>>| indices.collect::<Option<Vec<_>>>();
    let (Some(residual_row), Some(tear_col), Some(causal_col)) = (
        local(&mut tearing.residual_rows.iter().map(|&row| local_row(row))),
        local(&mut tearing.tear_y_indices.iter().map(|&y| local_col(y))),
        local(&mut causal.iter().map(|&(_, y)| local_col(y))),
    ) else {
        return Err(refuse(canonical, "tears outside its own rows or unknowns"));
    };
    let runs = causal_runs(sources, table, canonical, &causal)?;
    record.torn = true;
    record.k = tear_col.len();
    record.ncausal = causal.len();
    record.tear_col = table.push(tear_col);
    record.residual_row = table.push(residual_row);
    record.ncruns = runs.len() / 4;
    record.cruns = table.push(runs.iter().copied());
    record.causal_target = table.push(causal.iter().map(|&(_, y)| y));
    record.tear_deps = tear_dependencies(sources, table, tearing, &runs, &causal);
    record.causal_col = table.push(causal_col);
    Ok(())
}

/// Per tear column, the causal runs and reduced residual rows a perturbation
/// of that tear can change: a run is dependent when its residual program reads
/// the tear or a target of an earlier dependent run, and a residual row when
/// its program reads any of those. The reads are those of the whole residual
/// program, a superset of every isolation prefix. A perturbation sweep of the
/// reduced Jacobian re-evaluates only these; every other run and row reads the
/// same values as the base sweep and so has the base result, bit for bit.
/// Returns the pool offset of `k` entries, each the pool offset of one list
/// `[nruns, runs.., nrows, rows..]`.
fn tear_dependencies(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    tearing: &solve::BlockTearing,
    runs: &[usize],
    causal: &[(usize, usize)],
) -> usize {
    let program = |row: usize| sources.implicit.row_output_position(row).map(|(p, _)| p);
    let reads = |row: usize, dirty: &std::collections::BTreeSet<usize>| {
        program(row).is_none_or(|p| dirty.iter().any(|&y| sources.implicit.row_reads_y(p, y)))
    };
    let lists = tearing
        .tear_y_indices
        .iter()
        .map(|&tear| {
            let mut dirty = std::collections::BTreeSet::from([tear]);
            let mut dependent_runs = Vec::new();
            for (index, run) in runs.chunks(4).enumerate() {
                let steps = &causal[run[2]..run[2] + run[3]];
                if steps.iter().any(|&(row, _)| reads(row, &dirty)) {
                    dependent_runs.push(index);
                    dirty.extend(steps.iter().map(|&(_, target)| target));
                }
            }
            let rows = tearing
                .residual_rows
                .iter()
                .enumerate()
                .filter(|&(_, &row)| reads(row, &dirty))
                .map(|(index, _)| index)
                .collect::<Vec<_>>();
            let mut list = vec![dependent_runs.len()];
            list.extend(dependent_runs);
            list.push(rows.len());
            list.extend(rows);
            list
        })
        .collect::<Vec<_>>();
    let starts = lists
        .into_iter()
        .map(|list| table.push(list))
        .collect::<Vec<_>>();
    table.push(starts)
}

/// The causal sweep as runs of consecutive steps recovered from one residual
/// program, each a (chain function, first step, step count) triple, from the
/// linked kernel's own grouping (`torn_sweep_runs`: one chain program answers
/// every step of a run in order, non-decreasing prefixes, and no isolated
/// value depends on an earlier step's target), so each run evaluates its row
/// prefix once and the sweep computes exactly the values of the per-step
/// isolators.
/// A run under construction: its residual program, its (output, target)
/// pairs, and its first causal step.
type CausalRun = (usize, Vec<(usize, usize)>, usize);

fn causal_runs(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
    causal: &[(usize, usize)],
) -> Result<Vec<usize>, CodegenError> {
    for &(row, target) in causal {
        let (program, offset) = sources
            .implicit
            .row_output_position(row)
            .ok_or_else(|| refuse(canonical, "recovers through a row without a scalar view"))?;
        if !matches!(
            sources
                .implicit
                .target_isolation_output_program(program, offset, target),
            TargetIsolationProgram::Isolator(_)
        ) {
            return Err(refuse(
                canonical,
                "recovers through a row without an isolator",
            ));
        }
    }
    let grouped = sources
        .implicit
        .torn_sweep_runs(causal)
        .ok_or_else(|| refuse(canonical, "recovers through a row without an isolator"))?;
    let mut runs = Vec::new();
    for run in grouped {
        push_run(
            sources,
            table,
            canonical,
            (run.program_row, run.pairs, run.first_step),
            &mut runs,
        )?;
    }
    Ok(runs)
}

fn push_run(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
    (program, pairs, first): CausalRun,
    runs: &mut Vec<usize>,
) -> Result<(), CodegenError> {
    let count = pairs.len();
    if let [(offset, target)] = pairs[..] {
        runs.extend([
            RUN_ISOLATOR,
            table.isolators.intern((program, offset, target)),
            first,
            1,
        ]);
        return Ok(());
    }
    let function = table.causal.intern((program, pairs.clone()), || {
        let operations = sources
            .implicit
            .target_isolation_chain_program(program, &pairs)
            .ok_or_else(|| {
                refuse(
                    canonical,
                    "recovers through an isolator chain that does not materialize",
                )
            })?;
        Ok((
            operations,
            program_span(sources.implicit.block(), canonical, program)?,
        ))
    })?;
    runs.extend([RUN_CHAIN, function, first, count]);
    Ok(())
}

/// The affine elimination layout, when the linked kernel selects it under the
/// shared admission rule (`projection_policy::affine_elimination_capacity`),
/// with each guard's holding and solving causal positions and the tear
/// capacity up to which the kernel promotes steps in place.
fn record_elimination(
    table: &mut ProgramTable,
    canonical: usize,
    block: &solve::AlgebraicProjectionBlock,
    structure: &solve::JacobianStructure,
    csr: &Csr,
    record: &mut BlockRecord,
) -> Result<(), CodegenError> {
    let Some(layout) = structure.affine_elimination() else {
        return Ok(());
    };
    let n = block.rows.len();
    let Some(capacity) = affine_elimination_capacity(layout) else {
        return Ok(());
    };
    if layout.pattern() != structure.pattern() || layout.pattern().rows() as usize != n {
        return Ok(());
    }
    for row in 0..n {
        if layout.row_columns(row) != &csr.col_idx[csr.row_ptr[row]..csr.row_ptr[row + 1]] {
            return Err(refuse(
                canonical,
                "orders its elimination rows unlike its pattern",
            ));
        }
    }
    let guards = layout
        .zero_guards()
        .iter()
        .map(|pair| csr.positions.get(pair).copied())
        .collect::<Option<Vec<_>>>()
        .ok_or_else(|| refuse(canonical, "guards an entry outside its pattern"))?;
    record.elimination = true;
    record.nelim = layout.causal().len();
    record.elim_row = table.push(layout.causal().iter().map(|&(row, _)| row));
    record.elim_col = table.push(layout.causal().iter().map(|&(_, column)| column));
    record.nelim_tear = layout.tears().len();
    record.elim_residual = table.push(layout.residuals().iter().copied());
    record.elim_tear = table.push(layout.tears().iter().copied());
    record.nguards = guards.len();
    record.guards = table.push(guards);
    record.guard_step = table.push(
        layout
            .guard_steps()
            .iter()
            .flat_map(|&(holder, solver)| [holder, solver]),
    );
    record.elim_capacity = capacity;
    Ok(())
}

/// The unchecked isolation of every block row for every block unknown, as
/// the dense path's non-finite seeding and the singleton path query it.
/// Each row stores its majority kind plus the exceptions in column order.
fn record_isolation(
    sources: &BlockSources<'_>,
    table: &mut ProgramTable,
    canonical: usize,
    block: &solve::AlgebraicProjectionBlock,
    positions: &[(usize, usize)],
    reachable: bool,
    record: &mut BlockRecord,
) -> Result<(), CodegenError> {
    let n = block.rows.len();
    if !reachable {
        record.iso_default = table.push(std::iter::repeat_n(ISOLATION_UNAVAILABLE, n));
        record.iso_start = table.push(std::iter::repeat_n(0, n + 1));
        record.iso_entries = table.push([]);
        return Ok(());
    }
    let (mut defaults, mut starts, mut entries) = (Vec::with_capacity(n), vec![0], Vec::new());
    for &(program, offset) in positions {
        let mut kinds = Vec::with_capacity(n);
        for (column, &target) in block.y_indices.iter().enumerate() {
            let kind = isolation_kind(
                canonical,
                &sources
                    .implicit
                    .target_isolation_output_program(program, offset, target),
                || table.isolators.intern((program, offset, target)),
            )?;
            kinds.push((column, kind));
        }
        let output_values = kinds
            .iter()
            .filter(|(_, (kind, _))| *kind == ISOLATION_OUTPUT_VALUE)
            .count();
        let default = if 2 * output_values >= kinds.len() {
            ISOLATION_OUTPUT_VALUE
        } else {
            ISOLATION_UNAVAILABLE
        };
        defaults.push(default);
        for (column, (kind, function)) in kinds {
            if kind != default {
                entries.extend([column, kind, function]);
            }
        }
        starts.push(entries.len() / 3);
    }
    record.iso_default = table.push(defaults);
    record.iso_start = table.push(starts);
    record.iso_entries = table.push(entries);
    Ok(())
}

/// The descriptor kind and isolator id of one row output isolated for one
/// block unknown. An isolation no scalar program reproduces refuses the
/// block: the generated kernel never guesses an answer the evaluator gives.
pub(super) fn isolation_kind(
    canonical: usize,
    program: &TargetIsolationProgram,
    intern: impl FnOnce() -> usize,
) -> Result<(usize, usize), CodegenError> {
    match program {
        TargetIsolationProgram::Unavailable => Ok((ISOLATION_UNAVAILABLE, 0)),
        TargetIsolationProgram::OutputValue => Ok((ISOLATION_OUTPUT_VALUE, 0)),
        TargetIsolationProgram::Isolator(_) => Ok((ISOLATION_PROGRAM, intern())),
        TargetIsolationProgram::Unrepresentable => Err(refuse(
            canonical,
            "needs a row isolation no scalar program reproduces",
        )),
    }
}

fn program_span(
    source: &solve::ScalarProgramBlock,
    canonical: usize,
    program: usize,
) -> Result<Span, CodegenError> {
    source
        .program_span(program)
        .ok_or_else(|| refuse(canonical, "reads a program without source provenance"))
}

impl BlockRecord {
    /// Upper bound of the arena this block's deepest projection frame uses,
    /// as (doubles, size indices). Every C path allocates within one frame
    /// per call chain; the bound sums the chain's buffers: saved and scaled
    /// vectors, the pattern values, three dense square matrices (the
    /// expanded Jacobian, its scaled copy, and its LU) plus the two SVD
    /// factors, the torn reduced system and recovered derivatives, the affine
    /// elimination's recovery matrix at its promotion capacity with its step
    /// flags and reduced row and tear lists, and one program's outputs and
    /// seeds.
    pub(super) const fn lane_max(&self) -> usize {
        self.lane_max
    }

    pub(super) fn workspace(&self, seed_len: usize) -> (usize, usize) {
        let n = self.n;
        let k = self.k.max(self.nelim_tear).max(self.elim_capacity);
        let doubles = 24 * n
            + 3 * (self.nnz + 1)
            + 5 * n * n
            + n * k
            + self.ncausal * k
            + 4 * k * k
            + 24 * (k + 1)
            + self.max_outputs
            + self.jvp_max_outputs
            + self.lane_max_outputs
            + seed_len
            + 64;
        let sizes = 9 * n + 6 * (k + 1) + 16;
        (doubles, sizes)
    }
}
