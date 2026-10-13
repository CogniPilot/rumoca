use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Mutex;

use crate::structural_pattern::{DependencyRegisters, DependencyState};
use crate::{IndexIntervals, LinearOp, Reg, TargetAssignmentShape};

mod queries;
pub use queries::ScalarProgramYDependencyQueries;

/// The solver-Y dependencies of assignment shapes, one analysis per source
/// program and expression prefix however many programs its rows are issued
/// in.
#[derive(Default)]
pub(super) struct AssignmentDependencies<'a> {
    sources: BTreeMap<crate::RefreshScalarProgramSource, AssignmentSourceQueries<'a>>,
}

struct AssignmentSourceQueries<'a> {
    dependencies: ScalarProgramYDependencyQueries<'a>,
    loaded: Option<(usize, IndexIntervals)>,
}

impl<'a> AssignmentDependencies<'a> {
    /// The loaded indices inside each shape's footprint, met interval by
    /// interval: the cost follows the two sets' structure.
    pub(super) fn for_shapes(
        &mut self,
        source: crate::RefreshScalarProgramSource,
        source_program: &'a [LinearOp],
        shapes: &[TargetAssignmentShape],
    ) -> Box<[IndexIntervals]> {
        let queries = self
            .sources
            .entry(source)
            .or_insert_with(|| AssignmentSourceQueries {
                dependencies: ScalarProgramYDependencyQueries::new(source_program),
                loaded: None,
            });
        shapes
            .iter()
            .map(|shape| {
                let prefix_len = shape.expr_eval_len().min(source_program.len());
                let loaded = queries.loaded.get_or_insert_with(|| {
                    (prefix_len, y_load_intervals(&source_program[..prefix_len]))
                });
                if loaded.0 != prefix_len {
                    *loaded = (prefix_len, y_load_intervals(&source_program[..prefix_len]));
                }
                let loaded = &loaded.1;
                match queries
                    .dependencies
                    .footprint_ranges(prefix_len, shape.value_register_ranges())
                {
                    Some(footprint) => footprint.intersection(loaded),
                    None => loaded.clone(),
                }
            })
            .collect()
    }
}

pub(super) fn y_load_indices(program: &[LinearOp]) -> BTreeSet<usize> {
    y_load_intervals(program).iter().collect()
}

fn y_load_intervals(program: &[LinearOp]) -> IndexIntervals {
    let mut indices = IndexIntervals::default();
    collect_y_load_intervals(program, &mut indices);
    indices
}

fn collect_y_load_intervals(program: &[LinearOp], indices: &mut IndexIntervals) {
    for operation in program {
        match operation {
            LinearOp::LoadY { index, .. } => {
                indices.insert(*index);
            }
            LinearOp::TensorLoad {
                input: crate::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => {
                *indices = indices.union(&IndexIntervals::range(
                    *input_start,
                    input_start.saturating_add(*count),
                ));
            }
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                for region in program.regions() {
                    collect_y_load_intervals(region, indices);
                }
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    collect_y_load_intervals(&arm.condition, indices);
                    collect_y_load_intervals(&arm.result, indices);
                }
                collect_y_load_intervals(&program.fallback, indices);
            }
            _ => {}
        }
    }
}

/// Fail-closed solver-Y dependence query for registers in one checked scalar
/// program. The exhaustive dependency walk is owned by `StructuralPattern`;
/// refresh construction consumes that owner instead of maintaining another
/// interpretation of compact tensor and call operations.
///
/// A register range is answered from its footprint: the union of its
/// registers' dependencies as sorted disjoint index intervals, built once per
/// range. A query then costs a binary search, so asking many targets about
/// the same wide operation never rescans its registers.
pub struct ScalarProgramYDependency<'a> {
    dependencies: Option<DependencyRegisters>,
    footprints: Mutex<BTreeMap<(Reg, usize), Option<IndexIntervals>>>,
    program: std::marker::PhantomData<&'a [LinearOp]>,
}

impl<'a> ScalarProgramYDependency<'a> {
    pub fn new(program: &'a [LinearOp]) -> Self {
        Self::from_inventory(
            crate::structural_pattern::program_register_y_dependencies(program).ok(),
        )
    }

    fn complete(program: &'a [LinearOp]) -> Option<Self> {
        Some(Self::from_inventory(Some(
            crate::structural_pattern::program_register_y_dependencies(program).ok()?,
        )))
    }

    fn from_inventory(dependencies: Option<DependencyRegisters>) -> Self {
        Self {
            dependencies,
            footprints: Mutex::new(BTreeMap::new()),
            program: std::marker::PhantomData,
        }
    }

    /// The exact solver-Y dependencies of `register`, or `None` when the
    /// analysis cannot bound them and [`Self::depends_on`] answers `true` for
    /// every target. Shared inventories are borrowed; scalar inventories are
    /// materialized only for the requested view.
    pub fn register_dependencies(&self, register: u32) -> Option<Cow<'_, IndexIntervals>> {
        Some(match self.register_state(register)? {
            Cow::Borrowed(DependencyState::Known(indices)) => Cow::Borrowed(indices),
            state => Cow::Owned(match state.into_owned() {
                DependencyState::Empty => IndexIntervals::default(),
                DependencyState::Singleton(index) => IndexIntervals::singleton(index),
                DependencyState::Known(indices) => (*indices).clone(),
            }),
        })
    }

    fn register_state(&self, register: u32) -> Option<Cow<'_, DependencyState>> {
        self.dependencies
            .as_ref()
            .and_then(|dependencies| dependencies.state(register))
    }

    pub fn depends_on(&self, register: u32, target: usize) -> bool {
        match self.register_state(register).as_deref() {
            None => true,
            Some(DependencyState::Empty) => false,
            Some(DependencyState::Singleton(index)) => *index == target,
            Some(DependencyState::Known(indices)) => indices.contains(target),
        }
    }

    /// Whether any of the `count` registers from `start` depends on `target`.
    pub fn range_depends_on(&self, start: Reg, count: usize, target: usize) -> bool {
        let mut footprints = self
            .footprints
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        footprints
            .entry((start, count))
            .or_insert_with(|| {
                start.checked_add(u32::try_from(count).ok()?)?;
                let dependencies = self.dependencies.as_ref()?.range(start, count)?;
                Some(match dependencies {
                    DependencyState::Empty => IndexIntervals::default(),
                    DependencyState::Singleton(index) => IndexIntervals::singleton(index),
                    DependencyState::Known(indices) => (*indices).clone(),
                })
            })
            .as_ref()
            .is_none_or(|footprint| footprint.contains(target))
    }

    /// The union of the dependencies of `registers`, or `None` when one of
    /// them is unbounded. Registers that share one dependency set are united
    /// once.
    pub fn footprint(&self, registers: impl IntoIterator<Item = Reg>) -> Option<IndexIntervals> {
        self.footprint_checked(registers, |_| true)
    }

    /// Union the exact checked range owners without materializing scalar IDs.
    pub fn footprint_ranges(
        &self,
        ranges: impl IntoIterator<Item = (Reg, usize)>,
    ) -> Option<IndexIntervals> {
        self.footprint_ranges_checked(ranges, |_, _| true)
    }

    fn footprint_ranges_checked(
        &self,
        ranges: impl IntoIterator<Item = (Reg, usize)>,
        mut available: impl FnMut(Reg, usize) -> bool,
    ) -> Option<IndexIntervals> {
        let inventory = self.dependencies.as_ref()?;
        let mut footprint = DependencyState::Empty;
        for (start, count) in ranges {
            if !available(start, count) {
                return None;
            }
            footprint = footprint.union(inventory.range(start, count)?);
        }
        Some(match footprint {
            DependencyState::Empty => IndexIntervals::default(),
            DependencyState::Singleton(index) => IndexIntervals::singleton(index),
            DependencyState::Known(indices) => (*indices).clone(),
        })
    }

    fn footprint_checked(
        &self,
        registers: impl IntoIterator<Item = Reg>,
        mut available: impl FnMut(Reg) -> bool,
    ) -> Option<IndexIntervals> {
        let mut footprint = IndexIntervals::default();
        let mut previous: Option<std::sync::Arc<IndexIntervals>> = None;
        for register in registers {
            if !available(register) {
                return None;
            }
            match self.register_state(register)?.as_ref() {
                DependencyState::Empty => {}
                DependencyState::Singleton(index) => footprint.insert(*index),
                DependencyState::Known(set)
                    if previous
                        .as_ref()
                        .is_some_and(|previous| std::sync::Arc::ptr_eq(previous, set)) => {}
                DependencyState::Known(set) => {
                    footprint = footprint.union(set);
                    previous = Some(std::sync::Arc::clone(set));
                }
            }
        }
        Some(footprint)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assignment_footprints_preserve_interleaved_store_prefixes() {
        let source = [
            LinearOp::LoadY { dst: 10, index: 77 },
            LinearOp::LoadY { dst: 0, index: 5 },
            LinearOp::Binary {
                dst: 11,
                op: crate::BinaryOp::Sub,
                lhs: 10,
                rhs: 0,
            },
            LinearOp::StoreOutput { src: 11 },
            LinearOp::LoadY { dst: 2, index: 9 },
            LinearOp::Binary {
                dst: 12,
                op: crate::BinaryOp::Sub,
                lhs: 10,
                rhs: 2,
            },
            LinearOp::StoreOutput { src: 12 },
        ];
        let shapes = [0, 1, 0].map(|output| {
            crate::derive_target_assignment_shape_for_output(&source, output, 77).unwrap()
        });
        let mut queries = AssignmentDependencies::default();
        let id = crate::RefreshScalarProgramSource::checked(0, 0).unwrap();
        assert_eq!(
            &*queries.for_shapes(id, &source, &shapes),
            &[
                IndexIntervals::singleton(5),
                IndexIntervals::singleton(9),
                IndexIntervals::singleton(5)
            ]
        );
        assert_eq!(queries.sources.len(), 1);
        assert_eq!(
            queries.sources[&id].loaded.as_ref().unwrap().0,
            shapes[2].expr_eval_len()
        );
    }

    #[test]
    fn loaded_tensor_coordinates_remain_compact_with_original_saturation() {
        let tensor = |start, count| LinearOp::TensorLoad {
            dst_start: 0,
            input: crate::TensorInputKind::Y,
            input_start: start,
            count,
            seed_start: None,
            lanes: 1,
        };
        let wide = y_load_intervals(&[tensor(100, 7_000_000)]);
        assert_eq!(wide, IndexIntervals::range(100, 7_000_100));
        assert_eq!(wide.intervals().count(), 1);
        assert!(y_load_intervals(&[tensor(usize::MAX, 1)]).is_empty());
        assert_eq!(
            y_load_intervals(&[tensor(usize::MAX - 2, 4)]),
            IndexIntervals::range(usize::MAX - 2, usize::MAX)
        );
        assert_eq!(
            y_load_indices(&[tensor(4, 3), tensor(5, 0)]),
            BTreeSet::from([4, 5, 6])
        );
    }

    #[test]
    fn a_register_range_depends_on_exactly_its_registers_targets() {
        let program = [
            LinearOp::LoadY { dst: 0, index: 3 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::LoadY { dst: 2, index: 5 },
        ];
        let dependencies = ScalarProgramYDependency::new(&program);
        assert!(dependencies.range_depends_on(0, 2, 3));
        assert!(!dependencies.range_depends_on(0, 2, 5));
        assert!(!dependencies.range_depends_on(1, 1, 3));
        assert!(dependencies.range_depends_on(1, 2, 5));
        // A repeated query is answered from the same footprint.
        assert!(dependencies.range_depends_on(0, 2, 3));
    }

    #[test]
    fn full_capacity_load_keeps_singletons_inline_and_gaps_unknown() {
        let program = [
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::Y,
                input_start: 200,
                count: 14400,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::Const {
                dst: 14402,
                value: 1.0,
            },
        ];
        let dependencies = ScalarProgramYDependency::new(&program);
        for register in 0..14400 {
            assert!(matches!(
                dependencies.register_state(register).as_deref(),
                Some(DependencyState::Singleton(index)) if *index == 200 + register as usize
            ));
            assert!(dependencies.depends_on(register, 200 + register as usize));
            assert!(!dependencies.depends_on(register, 201 + register as usize));
        }
        assert_eq!(
            dependencies
                .footprint(0..14400)
                .unwrap()
                .intervals()
                .collect::<Vec<_>>(),
            [200..=14599]
        );
        assert!(matches!(
            dependencies.register_dependencies(0),
            Some(Cow::Owned(_))
        ));
        assert!(dependencies.register_dependencies(14400).is_none());
        assert!(dependencies.depends_on(14400, 200));
        assert!(dependencies.footprint([0, 14400]).is_none());
        assert!(
            dependencies
                .register_dependencies(14402)
                .unwrap()
                .is_empty()
        );
        assert!(!dependencies.depends_on(14402, 200));
        assert!(dependencies.footprint([14402]).unwrap().is_empty());
    }

    #[test]
    fn shared_inventory_reuse_retains_intervening_singleton_reads() {
        let program = [
            LinearOp::LoadY { dst: 0, index: 3 },
            LinearOp::LoadY { dst: 1, index: 4 },
            LinearOp::Binary {
                dst: 2,
                op: crate::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Move { dst: 3, src: 2 },
            LinearOp::LoadY { dst: 4, index: 9 },
            LinearOp::Const { dst: 5, value: 1.0 },
        ];
        let dependencies = ScalarProgramYDependency::new(&program);
        let Some(Cow::Borrowed(left)) = dependencies.register_dependencies(2) else {
            panic!("whole inventory must be borrowed");
        };
        let Some(Cow::Borrowed(right)) = dependencies.register_dependencies(3) else {
            panic!("moved inventory must be borrowed");
        };
        assert!(std::ptr::eq(left, right));
        assert_eq!(
            dependencies.footprint([2, 5, 4, 3]).unwrap().to_set(),
            BTreeSet::from([3, 4, 9])
        );
    }
}
