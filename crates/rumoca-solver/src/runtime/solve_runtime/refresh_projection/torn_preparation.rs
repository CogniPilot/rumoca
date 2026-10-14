//! Eager optional torn composites over the immutable scalar source inventory.

use super::*;
use std::hash::{Hash, Hasher};

#[derive(Clone)]
struct CachedSweep {
    tearing: solve::BlockTearing,
    entry: Option<Rc<PreparedTornSweepEntry>>,
}

/// Exact tearing values disambiguate hash collisions and survive runtime clones.
/// Every entry belongs to this inventory's immutable implicit source program.
#[derive(Clone, Default)]
pub(in super::super) struct TornSweepCache(RefCell<FxHashMap<u64, Vec<CachedSweep>>>);

fn key(tearing: &solve::BlockTearing) -> u64 {
    let mut hash = rustc_hash::FxHasher::default();
    tearing.tear_y_indices.hash(&mut hash);
    tearing.residual_rows.hash(&mut hash);
    tearing.causal_steps.len().hash(&mut hash);
    for step in &tearing.causal_steps {
        step.row.hash(&mut hash);
        step.y_index.hash(&mut hash);
        std::mem::discriminant(&step.coefficient).hash(&mut hash);
    }
    hash.finish()
}

impl TornSweepCache {
    fn lookup(&self, tearing: &solve::BlockTearing) -> Option<Option<Rc<PreparedTornSweepEntry>>> {
        self.0
            .borrow()
            .get(&key(tearing))?
            .iter()
            .find(|entry| entry.tearing == *tearing)
            .map(|entry| entry.entry.clone())
    }

    fn insert(&self, tearing: &solve::BlockTearing, entry: Option<Rc<PreparedTornSweepEntry>>) {
        self.0
            .borrow_mut()
            .entry(key(tearing))
            .or_default()
            .push(CachedSweep {
                tearing: tearing.clone(),
                entry,
            });
    }
}

impl ScalarExecutionView<'_> {
    /// Prepare every original and alternate tearing before returning a compiled runtime.
    pub(in super::super) fn prepare_torn_sweeps(&self) {
        if !matches!(self.scalar.expressions, PreparedExpressions::Compiled(_)) {
            return;
        }
        self.prepare_torn_plan(&self.model.problem.continuous.algebraic_projection_plan);
        let owners = &self.model.problem.continuous.refresh_owners;
        for plan in [
            owners.algebraic(),
            owners.derivative(),
            owners.root(),
            owners.event(),
        ]
        .into_iter()
        .chain(owners.clock_events())
        .chain(
            owners
                .root_after_derivative()
                .map(|relation| relation.remainder()),
        )
        .chain(
            owners
                .root_after_algebraic()
                .map(|relation| relation.remainder()),
        )
        .chain(
            owners
                .algebraic_after_derivative()
                .map(|relation| relation.remainder()),
        )
        .chain(
            owners
                .clock_events_after_event()
                .iter()
                .map(|relation| relation.remainder()),
        ) {
            self.prepare_torn_refresh(plan);
        }
    }

    fn prepare_torn_refresh(&self, plan: &solve::RefreshPlan) {
        self.prepare_torn_plan(&plan.simultaneous_plan);
        self.prepare_torn_plan(&plan.value_projection_plan);
        for stage in &plan.value_stages {
            if let solve::RefreshStage::ProjectionBlock { plan, .. } = stage {
                self.prepare_torn_plan(plan);
            }
        }
    }

    fn prepare_torn_plan(&self, plan: &solve::AlgebraicProjectionPlan) {
        for tearing in plan
            .blocks
            .scalar_blocks()
            .iter()
            .flat_map(|block| block.tearing.iter().chain(&block.alternate_charts))
        {
            if self.scalar.torn_sweep_cache.lookup(tearing).is_none() {
                let prepared = self.build_torn_sweep_entry(tearing).map(Rc::new);
                self.scalar.torn_sweep_cache.insert(tearing, prepared);
            }
        }
    }

    /// Compiled inventories never perform late compilation. Unissued or
    /// unbatchable tearings use their already prepared mandatory native base.
    /// Explicit reference execution may lazily prepare an interpreted batch.
    pub(in super::super) fn prepared_torn_sweep(
        &self,
        tearing: &solve::BlockTearing,
    ) -> Option<Rc<PreparedTornSweepEntry>> {
        if let Some(prepared) = self.scalar.torn_sweep_cache.lookup(tearing) {
            return prepared;
        }
        if matches!(self.scalar.expressions, PreparedExpressions::Compiled(_)) {
            return None;
        }
        let prepared = self.build_torn_sweep_entry(tearing).map(Rc::new);
        self.scalar
            .torn_sweep_cache
            .insert(tearing, prepared.clone());
        prepared
    }

    fn build_torn_sweep_entry(
        &self,
        tearing: &solve::BlockTearing,
    ) -> Option<PreparedTornSweepEntry> {
        let causal_steps = tearing
            .causal_steps
            .iter()
            .map(|step| (step.row, step.y_index))
            .collect::<Vec<_>>();
        let sweep = self
            .scalar
            .implicit_scalar_rhs
            .source()
            .prepare_torn_sweep(&causal_steps, &tearing.residual_rows)?;
        let compiled = self.scalar.execution_backend.as_ref().and_then(|backend| {
            let composite = self
                .scalar
                .implicit_scalar_rhs
                .source()
                .torn_sweep_composite(&sweep)?;
            let schedule = optional_compiled(
                "torn_assignment_schedule",
                backend.compile_torn_assignment_rows(
                    &composite.assignment_rows,
                    &composite.assignment_targets,
                ),
            )?;
            let residual_block = optional_compiled(
                "torn_residual_block",
                backend.compile_expression(&composite.residual_block),
            )?;
            let source_spans = tearing
                .causal_steps
                .iter()
                .filter_map(|step| {
                    self.scalar
                        .implicit_scalar_rhs
                        .source()
                        .row_output_position(step.row)
                })
                .filter_map(|(program, _)| {
                    self.scalar
                        .implicit_scalar_rhs
                        .source()
                        .program_span(program)
                })
                .chain(composite.residual_block.program_spans().iter().copied())
                .collect::<Vec<_>>()
                .into_boxed_slice();
            Some(CompiledTornSweep {
                schedule,
                residual_block,
                source_spans,
                affected_y: composite.assignment_targets.into_boxed_slice(),
                rollback: RefCell::new(Vec::new()),
                residual_len: composite.residual_block.stored_output_count(),
                residual_outputs: composite.residual_outputs.into_boxed_slice(),
            })
        });
        Some(PreparedTornSweepEntry { sweep, compiled })
    }
}
