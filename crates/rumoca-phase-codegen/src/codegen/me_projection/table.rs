//! The one program table and index pool every projection block reads.
//!
//! A generated component emits each implicit-residual program, each forward
//! Jacobian program, and each target isolator exactly once. Block descriptors
//! name them by index and keep their integer data (unknowns, patterns, colors,
//! tearing, elimination, isolation catalogs) as ranges of one shared pool, so
//! the staged refresh, the torn and dense paths, and the complete-plan
//! projection all dispatch through the same emitted text.

use std::collections::BTreeMap;
use std::sync::Arc;

use minijinja::Value;
use rumoca_core::Span;
use rumoca_ir_solve as solve;

use super::super::scalar_program_plan::ScalarProgramPlan;
use crate::errors::CodegenError;

/// One interned family of single-program C functions.
///
/// A component that switches reduced charts keys its programs by chart, and
/// every alternate repeats most of the primary's programs unchanged. Such a
/// family also interns by content, so an unchanged program keeps one C
/// function.
#[derive(Default)]
pub(super) struct FunctionFamily<K> {
    ids: BTreeMap<K, usize>,
    contents: Option<BTreeMap<String, usize>>,
    programs: Vec<(Vec<solve::LinearOp>, Span)>,
}

impl<K: Ord> FunctionFamily<K> {
    /// The function index of `key`, emitting `build`'s program on first use.
    pub(super) fn intern(
        &mut self,
        key: K,
        build: impl FnOnce() -> Result<(Vec<solve::LinearOp>, Span), CodegenError>,
    ) -> Result<usize, CodegenError> {
        if let Some(&id) = self.ids.get(&key) {
            return Ok(id);
        }
        let program = build()?;
        Ok(self.insert(key, program))
    }

    /// Emit `program` under `key`, or reuse the function of an identical
    /// program when the family interns by content.
    fn insert(&mut self, key: K, program: (Vec<solve::LinearOp>, Span)) -> usize {
        let id = match self.contents.as_mut() {
            Some(contents) => {
                let content = format!("{:?}", program.0);
                if let Some(&known) = contents.get(&content) {
                    known
                } else {
                    let id = self.programs.len();
                    contents.insert(content, id);
                    self.programs.push(program);
                    id
                }
            }
            None => {
                self.programs.push(program);
                self.programs.len() - 1
            }
        };
        self.ids.insert(key, id);
        id
    }

    fn share_contents(&mut self) {
        self.contents.get_or_insert_with(BTreeMap::new);
    }

    pub(super) fn output_count(&self, id: usize) -> usize {
        solve::ScalarProgramBlock::program_output_count(&self.programs[id].0)
    }

    /// The emitted plan: every program stores its outputs at local positions.
    pub(super) fn into_plan(self) -> Result<Value, CodegenError> {
        let mut targets = Vec::new();
        let mut operations = Vec::with_capacity(self.programs.len());
        let mut spans = Vec::with_capacity(self.programs.len());
        for (program, span) in self.programs {
            targets.extend(0..solve::ScalarProgramBlock::program_output_count(&program));
            operations.push(emitted(program));
            spans.push(span);
        }
        let block = solve::ScalarProgramBlock::with_output_indices(operations, spans, targets)
            .map_err(|error| CodegenError::template(error.to_string()))?;
        Ok(Value::from_object(ScalarProgramPlan::new(Arc::new(block))?))
    }
}

/// Rows sharing one row function: at least this many rows whose packed
/// programs differ only in their load indices.
const MIN_SHARED_ROWS: usize = 2;

impl<K: Ord> FunctionFamily<K> {
    /// The emitted plan of the residual rows, as [`Self::into_plan`], and
    /// the rows that share a row function: the classes of at least
    /// [`MIN_SHARED_ROWS`] packed programs equal up to their load indices
    /// (`solve::load_index_classes`). Each class emits its representative
    /// once, reading every load index that differs between its members from
    /// a per-row table; a load index all members agree on stays a literal.
    /// Every row's program still enters the plan, so its construction is
    /// checked as before.
    pub(super) fn into_rows_plan(self) -> Result<(Value, Value), CodegenError> {
        let mut targets = Vec::new();
        let mut operations = Vec::with_capacity(self.programs.len());
        let mut spans = Vec::with_capacity(self.programs.len());
        for (program, span) in self.programs {
            targets.extend(0..solve::ScalarProgramBlock::program_output_count(&program));
            operations.push(emitted(program));
            spans.push(span);
        }
        let classes: Vec<_> = solve::load_index_classes(&operations)
            .into_iter()
            .filter(|class| class.members.len() >= MIN_SHARED_ROWS)
            .collect();
        let shared = row_sharing(&operations, &spans, &classes)?;
        let block = solve::ScalarProgramBlock::with_output_indices(operations, spans, targets)
            .map_err(|error| CodegenError::template(error.to_string()))?;
        Ok((
            Value::from_object(ScalarProgramPlan::new(Arc::new(block))?),
            shared,
        ))
    }
}

/// The row-sharing tables: the representatives of `classes` with their
/// varying loads marked, each row's class (`classes.len()` for a row with
/// its own function) and argument offset, the argument pool, and the rows
/// with their own function.
fn row_sharing(
    operations: &[Vec<solve::LinearOp>],
    spans: &[Span],
    classes: &[solve::LoadIndexClass],
) -> Result<Value, CodegenError> {
    let mut shape_of = vec![classes.len(); operations.len()];
    let mut arg = vec![0usize; operations.len()];
    let mut pool = Vec::new();
    let (mut representatives, mut rep_spans, mut rep_targets, mut slots) =
        (Vec::new(), Vec::new(), Vec::new(), Vec::new());
    for (shape, class) in classes.iter().enumerate() {
        let (slot_of, ordinal_of) = argument_slots(class);
        for (&member, indices) in class.members.iter().zip(&class.indices) {
            shape_of[member] = shape;
            arg[member] = pool.len();
            pool.extend(ordinal_of.iter().map(|&ordinal| indices[ordinal]));
        }
        let representative = operations[class.members[0]].clone();
        slots.push(load_slots(&representative, &slot_of));
        rep_targets.extend(0..solve::ScalarProgramBlock::program_output_count(&representative));
        representatives.push(representative);
        rep_spans.push(spans[class.members[0]]);
    }
    let own: Vec<usize> = (0..operations.len())
        .filter(|&program| shape_of[program] == classes.len())
        .collect();
    let block =
        solve::ScalarProgramBlock::with_output_indices(representatives, rep_spans, rep_targets)
            .map_err(|error| CodegenError::template(error.to_string()))?;
    let shapes = ScalarProgramPlan::with_load_slots(Arc::new(block), slots)?;
    Ok(minijinja::context! {
        shapes => Value::from_object(shapes),
        shape_of => shape_of,
        arg => arg,
        pool => if pool.is_empty() { vec![0] } else { pool },
        own => own,
    })
}

/// The argument slot of each load ordinal of `class`, `None` for an index
/// every member shares (it stays a literal), and the ordinal each slot
/// takes its value from. Ordinals whose indices agree for every member
/// read one slot.
fn argument_slots(class: &solve::LoadIndexClass) -> (Vec<Option<usize>>, Vec<usize>) {
    let column = |ordinal: usize| class.indices.iter().map(move |indices| indices[ordinal]);
    let mut slot_of = vec![None; class.indices[0].len()];
    let mut ordinal_of: Vec<usize> = Vec::new();
    for (ordinal, slot) in slot_of.iter_mut().enumerate() {
        let first = class.indices[0][ordinal];
        if column(ordinal).all(|index| index == first) {
            continue;
        }
        let known = ordinal_of
            .iter()
            .position(|&other| column(other).eq(column(ordinal)));
        *slot = Some(known.unwrap_or_else(|| {
            ordinal_of.push(ordinal);
            ordinal_of.len() - 1
        }));
    }
    (slot_of, ordinal_of)
}

/// Per operation of `program`, the argument slot its load reads, from the
/// slot of each load ordinal.
fn load_slots(program: &[solve::LinearOp], slot_of: &[Option<usize>]) -> Vec<Option<usize>> {
    let mut ordinal = 0usize;
    program
        .iter()
        .map(|op| match op {
            solve::LinearOp::LoadY { .. } | solve::LinearOp::LoadP { .. } => {
                ordinal += 1;
                slot_of[ordinal - 1]
            }
            _ => None,
        })
        .collect()
}

/// A program as the kernel emits it: its registers packed by value lifetime
/// (`rumoca_ir_solve::pack_registers`), so its C register array holds the
/// values live at one time rather than one register per value.
pub(super) fn emitted(program: Vec<solve::LinearOp>) -> Vec<solve::LinearOp> {
    solve::pack_registers(&program).unwrap_or(program)
}

/// Colored Jacobian lane programs (tangent-lane programs), emitted with their
/// own checked block because their aggregates are wider than two lanes.
#[derive(Default)]
pub(super) struct LaneFamily {
    ids: BTreeMap<(usize, u64, usize, usize), usize>,
    programs: Vec<(Arc<solve::TangentLaneProgram>, Span)>,
}

impl LaneFamily {
    /// Reuse only the same issued lane owner and complete diagnostic span.
    /// Retaining the owner keeps its address live for every stored key.
    pub(super) fn intern(&mut self, program: Arc<solve::TangentLaneProgram>, span: Span) -> usize {
        let key = (
            Arc::as_ptr(&program) as usize,
            span.source.0,
            span.start.0,
            span.end.0,
        );
        if let Some(&id) = self.ids.get(&key) {
            return id;
        }
        let id = self.programs.len();
        self.ids.insert(key, id);
        self.programs.push((program, span));
        id
    }

    pub(super) fn into_plan(self) -> Result<Value, CodegenError> {
        let (programs, spans): (Vec<_>, Vec<_>) = self
            .programs
            .into_iter()
            .map(|(program, span)| (program.as_ref().clone(), span))
            .unzip();
        let block = solve::ScalarProgramBlock::with_tangent_lane_programs(&programs, spans)
            .map_err(|error| CodegenError::template(error.to_string()))?;
        Ok(Value::from_object(ScalarProgramPlan::new(Arc::new(block))?))
    }
}

/// (chart, residual program, ordered (output, target) pairs).
type CausalKey = (usize, usize, Vec<(usize, usize)>);

/// The output key of a row program that is not sliced.
pub(super) const FULL_PROGRAM: usize = usize::MAX;

/// Shared function families and the index pool of one component.
#[derive(Default)]
pub(super) struct ProgramTable {
    /// Implicit-residual programs keyed by (chart, scalar-projection row).
    /// Keyed by (chart, program, stored output): a block row reads one
    /// output, so its program is the slice that computes that output alone;
    /// `FULL_PROGRAM` keys the unsliced program.
    pub(super) rows: FunctionFamily<(usize, usize, usize)>,
    /// Forward Jacobian programs keyed by (application source, program).
    pub(super) jvp: FunctionFamily<(usize, usize)>,
    /// Multi-lane forward Jacobian programs, one per colored application
    /// program ([`solve::ColoredTangentPlan`]).
    pub(super) lanes: LaneFamily,
    /// One checked widening per source program and lane count across blocks.
    pub(super) lane_catalog: solve::TangentLaneCatalog,
    /// Invariant parts of block residual splits keyed by (chart, block,
    /// program): each stores its live-out registers as its outputs.
    pub(super) inv: FunctionFamily<(usize, usize, usize)>,
    /// Dependent parts of block residual splits keyed by (chart, block,
    /// program): each reads the block's live-out values from its seed vector.
    pub(super) dep: FunctionFamily<(usize, usize, usize)>,
    /// Target isolators keyed by (chart, program, output offset, target).
    pub(super) isolators: IsolatorCatalog,
    /// Causal isolation chains keyed by (chart, program, ordered (output,
    /// target) pairs).
    pub(super) causal: FunctionFamily<CausalKey>,
    /// Distinct Jacobian application sources, compared by owner identity.
    pub(super) jvp_sources: Vec<solve::ScalarProgramBlock>,
    pool: Vec<usize>,
}

impl ProgramTable {
    /// Intern every program family by content as well as by key: the table of
    /// a component whose alternate reduced charts repeat the primary's
    /// programs.
    pub(super) fn share_contents(&mut self) {
        self.rows.share_contents();
        self.jvp.share_contents();
        self.inv.share_contents();
        self.dep.share_contents();
        self.causal.share_contents();
        self.isolators.share_contents = true;
    }

    /// Append `values` to the pool and return their start offset.
    pub(super) fn push(&mut self, values: impl IntoIterator<Item = usize>) -> usize {
        let start = self.pool.len();
        self.pool.extend(values);
        start
    }

    /// The index of an application source, interned by owner identity.
    pub(super) fn jvp_source(&mut self, source: &solve::ScalarProgramBlock) -> usize {
        if let Some(index) = self
            .jvp_sources
            .iter()
            .position(|known| known.shares_program_owner(source))
        {
            return index;
        }
        self.jvp_sources.push(source.clone());
        self.jvp_sources.len() - 1
    }

    /// The emitted table; `implicits` are the implicit-residual blocks of the
    /// component's charts, in chart order.
    pub(super) fn into_value(
        self,
        implicits: &[rumoca_eval_solve::PreparedScalarProgramBlock],
    ) -> Result<Value, CodegenError> {
        let (isolators, isolator_group, isolator_slot) = self.isolators.into_groups(implicits)?;
        let row_max_outputs = (0..self.rows.programs.len())
            .map(|id| self.rows.output_count(id))
            .max()
            .unwrap_or(1);
        let (rows, row_sharing) = self.rows.into_rows_plan()?;
        let iso_max_outputs = (0..isolators.programs.len())
            .map(|id| isolators.output_count(id))
            .max()
            .unwrap_or(1);
        // A non-empty pool keeps every descriptor offset a valid address.
        let pool = if self.pool.is_empty() {
            vec![0]
        } else {
            self.pool
        };
        Ok(minijinja::context! {
            rows => rows,
            row_sharing => row_sharing,
            jvp => self.jvp.into_plan()?,
            lanes => self.lanes.into_plan()?,
            inv => self.inv.into_plan()?,
            dep => self.dep.into_plan()?,
            isolators => isolators.into_plan()?,
            causal => self.causal.into_plan()?,
            isolator_group => isolator_group,
            isolator_slot => isolator_slot,
            iso_max_outputs => iso_max_outputs,
            row_max_outputs => row_max_outputs,
            pool => pool,
        })
    }
}

/// Target isolators interned per (chart, program, output offset, target) and
/// grouped per residual program and isolator prefix length: that prefix of
/// the row is evaluated once and every requested isolation over it is
/// stored, and each isolation runs the slice of the group that computes its
/// own slot when the group slices (`slice_to_outputs`). Grouping only equal prefixes keeps each group's
/// failure set that of its members, so a pure call in a longer prefix never
/// fails an isolation the evaluator answers.
#[derive(Default)]
pub(super) struct IsolatorCatalog {
    ids: BTreeMap<IsolatorKey, usize>,
    keys: Vec<IsolatorKey>,
    share_contents: bool,
}

/// (chart, program, output offset, target).
type IsolatorKey = (usize, usize, usize, usize);

impl IsolatorCatalog {
    /// The isolator id of one (chart, program, output offset, target).
    pub(super) fn intern(&mut self, key: IsolatorKey) -> usize {
        if let Some(&id) = self.ids.get(&key) {
            return id;
        }
        let id = self.keys.len();
        self.keys.push(key);
        self.ids.insert(key, id);
        id
    }

    /// One grouped function per program plus, per isolator id, its group and
    /// output slot.
    fn into_groups(
        self,
        implicits: &[rumoca_eval_solve::PreparedScalarProgramBlock],
    ) -> Result<IsolatorGroups, CodegenError> {
        let implicit = |chart: usize| {
            implicits
                .get(chart)
                .ok_or_else(|| CodegenError::template("a target isolator names an unknown chart"))
        };
        let mut by_prefix = BTreeMap::<(usize, usize, usize), Vec<usize>>::new();
        for (id, &(chart, program, output, target)) in self.keys.iter().enumerate() {
            let prefix = implicit(chart)?
                .target_isolation_prefix_len(program, output, target)
                .ok_or_else(|| {
                    CodegenError::template("a target isolator has no assignment shape")
                })?;
            by_prefix
                .entry((chart, program, prefix))
                .or_default()
                .push(id);
        }
        let mut groups = FunctionFamily::default();
        if self.share_contents {
            groups.share_contents();
        }
        let mut group_of = vec![0; self.keys.len()];
        let mut slot_of = vec![0; self.keys.len()];
        for ((chart, program, prefix), ids) in by_prefix {
            let pairs = ids
                .iter()
                .map(|&id| (self.keys[id].2, self.keys[id].3))
                .collect::<Vec<_>>();
            let (operations, span) = group_program(implicit(chart)?, program, &pairs)?;
            // Each isolation reads one slot, so it runs the slice computing
            // that slot alone; a slot that does not slice runs the shared
            // group program.
            let slices = isolation_slices(&operations, ids.len());
            for ((slot, id), slice) in ids.into_iter().enumerate().zip(slices) {
                let (key, read) = slice.as_ref().map_or((FULL_PROGRAM, slot), |_| (slot, 0));
                let ops = || Ok((slice.unwrap_or_else(|| operations.clone()), span));
                group_of[id] = groups.intern((chart, program, prefix, key), ops)?;
                slot_of[id] = read;
            }
        }
        Ok((groups, group_of, slot_of))
    }
}

/// How far slicing may grow the code of an isolator group. An isolation runs
/// alone and reads one slot, so its slice saves work on every call; but
/// `K` slots that share a long prefix each repeat it, `K` functions of
/// about the group's length where the shared group is one. Keeping the
/// slices only while they total at most twice the group program bounds the
/// code to double, and the mean slice to `2/K` of the program.
const ISOLATION_SLICE_GROWTH: usize = 2;

/// The slice each of `slots` slots of a group program runs, or `None` for a
/// slot that runs the shared program: every slot's, once the slices total
/// more than [`ISOLATION_SLICE_GROWTH`] times the program's operations.
pub(super) fn isolation_slices(
    operations: &[solve::LinearOp],
    slots: usize,
) -> Vec<Option<Vec<solve::LinearOp>>> {
    let shared = || vec![None; slots];
    let outputs: Vec<usize> = (0..slots).collect();
    let Some(slices) = solve::slice_to_outputs(operations, &outputs, false) else {
        return shared();
    };
    let code = slices
        .iter()
        .flatten()
        .map(Vec::len)
        .fold(0usize, usize::saturating_add);
    if code > ISOLATION_SLICE_GROWTH.saturating_mul(operations.len()) {
        return shared();
    }
    slices
}

/// Grouped isolator functions, keyed by (chart, program, prefix, slot) for a
/// sliced isolation or `FULL_PROGRAM` for the shared group, plus, per
/// isolator id, its function and slot.
type IsolatorGroups = (
    FunctionFamily<(usize, usize, usize, usize)>,
    Vec<usize>,
    Vec<usize>,
);

fn group_program(
    implicit: &rumoca_eval_solve::PreparedScalarProgramBlock,
    program: usize,
    pairs: &[(usize, usize)],
) -> Result<(Vec<solve::LinearOp>, Span), CodegenError> {
    let operations = implicit
        .target_isolation_group_program(program, pairs)
        .ok_or_else(|| CodegenError::template("a grouped target isolation does not materialize"))?;
    let span = implicit
        .block()
        .program_span(program)
        .ok_or_else(|| CodegenError::template("a target isolator has no source provenance"))?;
    Ok((operations, span))
}
