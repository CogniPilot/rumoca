use std::collections::{BTreeMap, BTreeSet};
use std::sync::Mutex;

use crate::{LinearOp, Reg, TargetAssignmentShape};

pub(super) fn assignment_y_dependencies_for_shapes(
    source_program: &[LinearOp],
    shapes: &[TargetAssignmentShape],
) -> Box<[Box<[usize]>]> {
    let mut prefix_dependencies = BTreeMap::new();
    shapes
        .iter()
        .map(|shape| {
            let prefix_len = shape.expr_eval_len();
            let (indices, dependency) =
                prefix_dependencies.entry(prefix_len).or_insert_with(|| {
                    let prefix = source_program.get(..prefix_len).unwrap_or(source_program);
                    (
                        y_load_indices(prefix),
                        ScalarProgramYDependency::new(prefix),
                    )
                });
            let footprint = dependency.footprint(shape.value_registers());
            indices
                .iter()
                .copied()
                .filter(|index| {
                    footprint
                        .as_ref()
                        .is_none_or(|footprint| footprint.contains(*index))
                })
                .collect::<Vec<_>>()
                .into_boxed_slice()
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn y_load_indices(program: &[LinearOp]) -> BTreeSet<usize> {
    let mut indices = BTreeSet::new();
    collect_y_load_indices(program, &mut indices);
    indices
}

fn collect_y_load_indices(program: &[LinearOp], indices: &mut BTreeSet<usize>) {
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
            } => indices.extend(*input_start..input_start.saturating_add(*count)),
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                for region in program.regions() {
                    collect_y_load_indices(region, indices);
                }
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    collect_y_load_indices(&arm.condition, indices);
                    collect_y_load_indices(&arm.result, indices);
                }
                collect_y_load_indices(&program.fallback, indices);
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
    dependencies: Option<Vec<Option<std::sync::Arc<BTreeSet<usize>>>>>,
    footprints: Mutex<BTreeMap<(Reg, usize), Option<YFootprint>>>,
    program: std::marker::PhantomData<&'a [LinearOp]>,
}

impl<'a> ScalarProgramYDependency<'a> {
    pub fn new(program: &'a [LinearOp]) -> Self {
        Self {
            dependencies: crate::structural_pattern::program_register_y_dependencies(program).ok(),
            footprints: Mutex::new(BTreeMap::new()),
            program: std::marker::PhantomData,
        }
    }

    /// The exact solver-Y dependencies of `register`, or `None` when the
    /// analysis cannot bound them and [`Self::depends_on`] answers `true` for
    /// every target.
    pub fn register_dependencies(&self, register: u32) -> Option<&BTreeSet<usize>> {
        self.register_set(register)
    }

    fn register_set(&self, register: u32) -> Option<&BTreeSet<usize>> {
        self.dependencies
            .as_ref()
            .and_then(|dependencies| dependencies.get(register as usize))
            .and_then(Option::as_deref)
    }

    pub fn depends_on(&self, register: u32, target: usize) -> bool {
        self.register_set(register)
            .is_none_or(|dependencies| dependencies.contains(&target))
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
                let end = start.checked_add(u32::try_from(count).ok()?)?;
                self.footprint(start..end)
            })
            .as_ref()
            .is_none_or(|footprint| footprint.contains(target))
    }

    /// The union of the dependencies of `registers`, or `None` when one of
    /// them is unbounded.
    pub fn footprint(&self, registers: impl IntoIterator<Item = Reg>) -> Option<YFootprint> {
        let mut indices = BTreeSet::new();
        for register in registers {
            indices.extend(self.register_set(register)?);
        }
        Some(YFootprint::of(indices))
    }
}

/// A set of solver-Y indices as sorted, disjoint, non-adjacent half-open
/// intervals.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct YFootprint(Box<[(usize, usize)]>);

impl YFootprint {
    fn of(indices: BTreeSet<usize>) -> Self {
        let mut intervals: Vec<(usize, usize)> = Vec::new();
        for index in indices {
            match intervals.last_mut() {
                Some((_, end)) if *end == index => *end += 1,
                _ => intervals.push((index, index + 1)),
            }
        }
        Self(intervals.into_boxed_slice())
    }

    /// The indices of the set, in ascending order.
    pub fn indices(&self) -> impl Iterator<Item = usize> + '_ {
        self.0.iter().flat_map(|&(start, end)| start..end)
    }

    pub fn contains(&self, index: usize) -> bool {
        let after = self.0.partition_point(|&(start, _)| start <= index);
        after
            .checked_sub(1)
            .is_some_and(|position| index < self.0[position].1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_footprint_coalesces_adjacent_indices_and_answers_membership() {
        let footprint = YFootprint::of(BTreeSet::from([1, 2, 3, 7]));
        assert_eq!(&*footprint.0, &[(1, 4), (7, 8)]);
        for index in [1, 2, 3, 7] {
            assert!(footprint.contains(index), "{index}");
        }
        for index in [0, 4, 6, 8] {
            assert!(!footprint.contains(index), "{index}");
        }
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
}
