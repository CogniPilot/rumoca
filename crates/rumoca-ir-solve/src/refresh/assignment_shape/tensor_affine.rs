//! Source-bound affine rules; tensor coordinates are not stored as a new graph.

pub(in crate::refresh) mod operation;

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use super::{ProgramPrefix, ScalarProgramYDependency, producer_position};
use crate::{LinearOp, Reg, TensorInputKind};
use operation::{OperandRange, ProjectionOperation};

/// Compact offset/coefficient relation for one selected residual output.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AffineTensorProjection {
    pub(in crate::refresh) output: Reg,
    pub(in crate::refresh) steps: Arc<[(Reg, ProjectionRule)]>,
    independent_ranges: Arc<[(Reg, usize)]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(in crate::refresh) enum ProjectionRule {
    Independent,
    Target { offset: usize },
    Linear,
    LeftProduct,
    RightProduct,
}

impl AffineTensorProjection {
    pub const fn output_register(&self) -> Reg {
        self.output
    }

    /// Whether the target's coefficient is a signed constant: every step on the
    /// path from the target to the output is a sum or difference, never a
    /// product with an independent factor.
    pub fn constant_coefficient(&self) -> bool {
        self.steps.iter().all(|(_, rule)| {
            !matches!(
                rule,
                ProjectionRule::LeftProduct | ProjectionRule::RightProduct
            )
        })
    }

    pub fn value_register_ranges(&self) -> impl Iterator<Item = (Reg, usize)> + '_ {
        self.independent_ranges.iter().copied()
    }

    pub fn value_registers(&self) -> impl Iterator<Item = Reg> + '_ {
        self.value_register_ranges().flat_map(|(start, count)| {
            (0..count).filter_map(move |offset| start.checked_add(u32::try_from(offset).ok()?))
        })
    }
}

pub(super) fn derive(
    program: ProgramPrefix<'_>,
    output: Reg,
    target: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<AffineTensorProjection> {
    let mut pending = BTreeSet::from([producer_position(program, output)?]);
    let mut steps = BTreeMap::new();
    let mut independent_ranges = BTreeSet::new();
    while let Some(position) = pending.pop_last() {
        if steps.contains_key(&position) {
            continue;
        }
        let operation = program.operation(position)?;
        let start = operation.dst_register()?;
        let count = operation.dst_register_count();
        let rule = projection_rule(operation, target, dependencies)?;
        reject_zero_scalar_factor(program.before(position)?, operation, rule)?;
        match rule {
            ProjectionRule::Independent => {
                independent_ranges.insert((start, count));
            }
            ProjectionRule::Target { offset } => {
                independent_ranges.insert((start, offset));
                independent_ranges.insert((
                    start.checked_add(u32::try_from(offset.checked_add(1)?).ok()?)?,
                    count.checked_sub(offset.checked_add(1)?)?,
                ));
            }
            _ => {
                for operand in ProjectionOperation::new(operation)?
                    .operands()
                    .into_iter()
                    .flatten()
                {
                    require_producers(program.before(position)?, operand, &mut pending)?;
                }
            }
        }
        steps.insert(position, rule);
    }
    // Constant additive coefficients already have their canonical weighted
    // selection. This relation extends that vocabulary to bilinear operations.
    if !steps.values().any(|rule| {
        matches!(
            rule,
            ProjectionRule::LeftProduct | ProjectionRule::RightProduct
        )
    }) {
        return None;
    }
    Some(AffineTensorProjection {
        output,
        steps: steps
            .into_iter()
            .map(|(position, rule)| Some((program.operation(position)?.dst_register()?, rule)))
            .collect::<Option<Vec<_>>>()?
            .into(),
        independent_ranges: independent_ranges
            .into_iter()
            .filter(|(_, count)| *count != 0)
            .collect(),
    })
}

/// The projections of the operations whose lanes one store certifies.
///
/// [`derive`] reads its output only through the position of the operation that
/// produces it, so every lane of that operation has the same projection up to
/// its output register: each (operation, target) is derived once and rebound
/// to the asking lane. `dependency_candidates_tests` checks the memo against
/// the exhaustive inventory, which derives every pair afresh.
#[derive(Default)]
pub(super) struct OperationProjections {
    by_operation: BTreeMap<(usize, usize), Option<AffineTensorProjection>>,
    certified: BTreeMap<usize, Vec<usize>>,
}

impl OperationProjections {
    /// Exactly `derive(program, output, target, dependencies)`.
    pub(super) fn derive(
        &mut self,
        program: ProgramPrefix<'_>,
        output: Reg,
        target: usize,
        dependencies: &ScalarProgramYDependency<'_>,
    ) -> Option<AffineTensorProjection> {
        let position = producer_position(program, output)?;
        let key = (position, hooks::memo_target(target));
        let projection = self
            .by_operation
            .entry(key)
            .or_insert_with(|| {
                hooks::count(hooks::Work::Derived, 1);
                derive(program, output, target, dependencies)
            })
            .clone()?;
        Some(hooks::rebind(projection, output))
    }

    /// The only targets among `targets` that [`derive`] certifies for
    /// `output`: those with a projection of the operation producing it.
    /// `targets` must be the same set for every lane of one operation.
    pub(super) fn candidate_targets(
        &mut self,
        program: ProgramPrefix<'_>,
        output: Reg,
        targets: &BTreeSet<usize>,
        dependencies: &ScalarProgramYDependency<'_>,
    ) -> Vec<usize> {
        let Some(position) = producer_position(program, output) else {
            return Vec::new();
        };
        if let Some(certified) = self.certified.get(&position) {
            return certified.clone();
        }
        hooks::count(hooks::Work::Scanned, targets.len());
        let certified: Vec<usize> = targets
            .iter()
            .copied()
            .filter(|&target| self.derive(program, output, target, dependencies).is_some())
            .filter(|_| hooks::keep_candidates())
            .collect();
        if hooks::memoize_certified() {
            self.certified.insert(position, certified.clone());
        }
        certified
    }
}

fn reject_zero_scalar_factor(
    program: ProgramPrefix<'_>,
    operation: &LinearOp,
    rule: ProjectionRule,
) -> Option<()> {
    let LinearOp::Binary {
        op: crate::BinaryOp::Mul,
        lhs,
        rhs,
        ..
    } = operation
    else {
        return Some(());
    };
    let factor = match rule {
        ProjectionRule::LeftProduct => *rhs,
        ProjectionRule::RightProduct => *lhs,
        _ => return Some(()),
    };
    match program.operation(producer_position(program, factor)?)? {
        LinearOp::Const { value, .. } if *value == 0.0 || !value.is_finite() => None,
        _ => Some(()),
    }
}

fn projection_rule(
    operation: &LinearOp,
    target: usize,
    dependencies: &ScalarProgramYDependency<'_>,
) -> Option<ProjectionRule> {
    let range = OperandRange::new(operation.dst_register()?, operation.dst_register_count());
    if range.independent_of(target, dependencies) {
        return Some(ProjectionRule::Independent);
    }
    match *operation {
        LinearOp::LoadY { index, .. } if index == target => {
            return Some(ProjectionRule::Target { offset: 0 });
        }
        LinearOp::TensorLoad {
            input: TensorInputKind::Y,
            input_start,
            count,
            lanes: 1,
            ..
        } => {
            let offset = target.checked_sub(input_start)?;
            return (offset < count).then_some(ProjectionRule::Target { offset });
        }
        _ => {}
    }
    ProjectionOperation::new(operation)?.rule(target, dependencies)
}

fn require_producers(
    program: ProgramPrefix<'_>,
    range: OperandRange,
    pending: &mut BTreeSet<usize>,
) -> Option<()> {
    let mut cursor = range.start;
    let end = range.end()?;
    while cursor < end {
        let position = producer_position(program, cursor)?;
        let operation = program.operation(position)?;
        let operation_end = operation
            .dst_register()?
            .checked_add(u32::try_from(operation.dst_register_count()).ok()?)?;
        pending.insert(position);
        cursor = operation_end.min(end);
    }
    Some(())
}

/// The one implementation of the memo key, the lane rebinding, and the
/// candidate filter of [`OperationProjections`]. A test injects one fault at
/// a time into it, to prove the exhaustive oracle catches each, and counts the
/// work the inventory does; a production build injects none and counts
/// nothing.
pub(super) mod hooks {
    use super::AffineTensorProjection;
    use crate::Reg;

    /// A fault a test injects.
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub(in crate::refresh) enum Fault {
        /// Lanes keep the output of the lane that derived the projection.
        NoRebind,
        /// The memo forgets the target, sharing one projection across targets.
        KeyByOperation,
        /// The operation's certified targets are dropped.
        NoCandidates,
        /// Each lane scans its operation's targets again.
        NoCertifiedMemo,
    }

    /// Work the inventory does: pairs asked of a rule, projections derived,
    /// and targets scanned for an operation's candidates.
    #[derive(Clone, Copy, Debug)]
    pub(in crate::refresh) enum Work {
        Asked,
        Derived,
        Scanned,
    }

    pub(super) fn memo_target(target: usize) -> usize {
        if injected(Fault::KeyByOperation) {
            0
        } else {
            target
        }
    }

    pub(super) fn rebind(
        projection: AffineTensorProjection,
        output: Reg,
    ) -> AffineTensorProjection {
        if injected(Fault::NoRebind) {
            return projection;
        }
        AffineTensorProjection {
            output,
            ..projection
        }
    }

    pub(super) fn keep_candidates() -> bool {
        !injected(Fault::NoCandidates)
    }

    pub(super) fn memoize_certified() -> bool {
        !injected(Fault::NoCertifiedMemo)
    }

    #[cfg(not(test))]
    const fn injected(_fault: Fault) -> bool {
        false
    }

    #[cfg(not(test))]
    pub(in crate::refresh) const fn count(_work: Work, _amount: usize) {}

    #[cfg(test)]
    use injection::injected;
    #[cfg(test)]
    pub(in crate::refresh) use injection::{Counts, count, counted, with};

    #[cfg(test)]
    mod injection {
        use std::cell::Cell;

        use super::{Fault, Work};

        /// The work counted while a test runs.
        #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
        pub(in crate::refresh) struct Counts {
            pub(in crate::refresh) asked: usize,
            pub(in crate::refresh) derived: usize,
            pub(in crate::refresh) scanned: usize,
        }

        thread_local! {
            static ACTIVE: Cell<Option<Fault>> = const { Cell::new(None) };
            static COUNTS: Cell<Counts> = const {
                Cell::new(Counts { asked: 0, derived: 0, scanned: 0 })
            };
        }

        pub(super) fn injected(fault: Fault) -> bool {
            ACTIVE.with(Cell::get) == Some(fault)
        }

        pub(in crate::refresh) fn count(work: Work, amount: usize) {
            COUNTS.with(|counts| {
                let mut value = counts.get();
                match work {
                    Work::Asked => value.asked += amount,
                    Work::Derived => value.derived += amount,
                    Work::Scanned => value.scanned += amount,
                }
                counts.set(value);
            });
        }

        /// Clears the injected fault when dropped, so a panicking `run`
        /// leaves no fault behind on its thread.
        struct ClearFault;

        impl Drop for ClearFault {
            fn drop(&mut self) {
                ACTIVE.with(|active| active.set(None));
            }
        }

        /// `run` with `fault` injected, cleared when `run` returns or panics.
        pub(in crate::refresh) fn with<T>(fault: Fault, run: impl FnOnce() -> T) -> T {
            ACTIVE.with(|active| active.set(Some(fault)));
            let _clear = ClearFault;
            run()
        }

        /// `run` and the work it does.
        pub(in crate::refresh) fn counted<T>(run: impl FnOnce() -> T) -> (T, Counts) {
            COUNTS.with(|counts| counts.set(Counts::default()));
            let result = run();
            (result, COUNTS.with(Cell::get))
        }
    }
}
