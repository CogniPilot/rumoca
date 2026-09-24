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
#[derive(Default)]
pub(super) struct FunctionFamily<K> {
    ids: BTreeMap<K, usize>,
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
        let id = self.programs.len();
        self.programs.push(build()?);
        self.ids.insert(key, id);
        Ok(id)
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

/// Shared function families and the index pool of one component.
#[derive(Default)]
pub(super) struct ProgramTable {
    /// Implicit-residual programs keyed by their scalar-projection row.
    pub(super) rows: FunctionFamily<usize>,
    /// Forward Jacobian programs keyed by (application source, program).
    pub(super) jvp: FunctionFamily<(usize, usize)>,
    /// Target isolators keyed by (program, output offset, target).
    pub(super) isolators: IsolatorCatalog,
    /// Distinct Jacobian application sources, compared by owner identity.
    pub(super) jvp_sources: Vec<solve::ScalarProgramBlock>,
    pool: Vec<usize>,
}

impl ProgramTable {
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

    pub(super) fn into_value(
        self,
        implicit: &rumoca_eval_solve::PreparedScalarProgramBlock,
    ) -> Result<Value, CodegenError> {
        let (isolators, isolator_group, isolator_slot) = self.isolators.into_groups(implicit)?;
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
            isolators => isolators.into_plan()?,
            isolator_group => isolator_group,
            isolator_slot => isolator_slot,
            iso_max_outputs => iso_max_outputs,
            pool => pool,
        })
    }
}

/// Target isolators interned per (program, output offset, target) and
/// emitted as one function per residual program and isolator prefix length:
/// that prefix of the row is evaluated once and every requested isolation
/// over it is stored. Grouping only equal prefixes keeps each group's
/// failure set that of its members, so a pure call in a longer prefix never
/// fails an isolation the evaluator answers.
#[derive(Default)]
pub(super) struct IsolatorCatalog {
    ids: BTreeMap<(usize, usize, usize), usize>,
    keys: Vec<(usize, usize, usize)>,
}

impl IsolatorCatalog {
    /// The isolator id of one (program, output offset, target) pair.
    pub(super) fn intern(&mut self, key: (usize, usize, usize)) -> usize {
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
        implicit: &rumoca_eval_solve::PreparedScalarProgramBlock,
    ) -> Result<IsolatorGroups, CodegenError> {
        let mut by_prefix = BTreeMap::<(usize, usize), Vec<usize>>::new();
        for (id, &(program, output, target)) in self.keys.iter().enumerate() {
            let prefix = implicit
                .target_isolation_prefix_len(program, output, target)
                .ok_or_else(|| {
                    CodegenError::template("a target isolator has no assignment shape")
                })?;
            by_prefix.entry((program, prefix)).or_default().push(id);
        }
        let mut groups = FunctionFamily::default();
        let mut group_of = vec![0; self.keys.len()];
        let mut slot_of = vec![0; self.keys.len()];
        for ((program, prefix), ids) in by_prefix {
            let pairs = ids
                .iter()
                .map(|&id| (self.keys[id].1, self.keys[id].2))
                .collect::<Vec<_>>();
            let group = groups.intern((program, prefix), || {
                group_program(implicit, program, &pairs)
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
type IsolatorGroups = (FunctionFamily<(usize, usize)>, Vec<usize>, Vec<usize>);

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
