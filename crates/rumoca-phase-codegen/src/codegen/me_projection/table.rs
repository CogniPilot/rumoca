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
        let content = self.contents.as_ref().map(|_| format!("{:?}", program.0));
        let known = content
            .as_ref()
            .and_then(|content| self.contents.as_ref()?.get(content).copied());
        let id = known.unwrap_or(self.programs.len());
        if known.is_none() {
            self.programs.push(program);
            if let (Some(contents), Some(content)) = (self.contents.as_mut(), content) {
                contents.insert(content, id);
            }
        }
        self.ids.insert(key, id);
        Ok(id)
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
            operations.push(program);
            spans.push(span);
        }
        let block = solve::ScalarProgramBlock::with_output_indices(operations, spans, targets)
            .map_err(|error| CodegenError::template(error.to_string()))?;
        Ok(Value::from_object(ScalarProgramPlan::new(Arc::new(block))?))
    }
}

/// Colored Jacobian lane programs (tangent-lane programs), emitted with their
/// own checked block because their aggregates are wider than two lanes.
#[derive(Default)]
pub(super) struct LaneFamily {
    programs: Vec<(solve::TangentLaneProgram, Span)>,
}

impl LaneFamily {
    pub(super) fn push(&mut self, program: solve::TangentLaneProgram, span: Span) -> usize {
        self.programs.push((program, span));
        self.programs.len() - 1
    }

    pub(super) fn into_plan(self) -> Result<Value, CodegenError> {
        let (programs, spans): (Vec<_>, Vec<_>) = self.programs.into_iter().unzip();
        let block = solve::ScalarProgramBlock::with_tangent_lane_programs(&programs, spans)
            .map_err(|error| CodegenError::template(error.to_string()))?;
        Ok(Value::from_object(ScalarProgramPlan::new(Arc::new(block))?))
    }
}

/// (chart, residual program, ordered (output, target) pairs).
type CausalKey = (usize, usize, Vec<(usize, usize)>);

/// Shared function families and the index pool of one component.
#[derive(Default)]
pub(super) struct ProgramTable {
    /// Implicit-residual programs keyed by (chart, scalar-projection row).
    pub(super) rows: FunctionFamily<(usize, usize)>,
    /// Forward Jacobian programs keyed by (application source, program).
    pub(super) jvp: FunctionFamily<(usize, usize)>,
    /// Multi-lane forward Jacobian programs, one per colored application
    /// program ([`solve::ColoredTangentPlan`]).
    pub(super) lanes: LaneFamily,
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
            rows => self.rows.into_plan()?,
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
/// emitted as one function per residual program and isolator prefix length:
/// that prefix of the row is evaluated once and every requested isolation
/// over it is stored. Grouping only equal prefixes keeps each group's
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
            let group = groups.intern((chart, program, prefix), || {
                group_program(implicit(chart)?, program, &pairs)
            })?;
            for (slot, id) in ids.into_iter().enumerate() {
                group_of[id] = group;
                slot_of[id] = slot;
            }
        }
        Ok((groups, group_of, slot_of))
    }
}

/// Grouped isolator functions plus, per isolator id, its group and slot.
type IsolatorGroups = (
    FunctionFamily<(usize, usize, usize)>,
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
