//! How each root-condition output takes part in continuous root search
//! (SPEC_0044 ME-EVENT-005).
//!
//! The classification is a pure function of the checked root rows and the
//! root refresh owner's static causality. Every executor reads it: the linked
//! runtime decides which rows it evaluates while integrating, the FMI
//! event-indicator inventory exposes exactly the searched rows, and generated
//! code renders the same split. No executor classifies roots itself.

use std::collections::BTreeSet;

use std::ops::Range;

use crate::{BinaryOp, LinearOp, Reg, ScalarProgramBlock, SolveProblem, StructuralPattern};

/// Which side of `time` a direct time root subtracts.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TimeRootSign {
    /// `p - time`: positive before the instant.
    ParamMinusTime,
    /// `time - p`: positive after the instant.
    TimeMinusParam,
}

/// The search role of one root-condition output.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RootSearchRole {
    /// The value can change during integration: evaluate and search it.
    Search,
    /// `p - time` or `time - p`: the time schedule announces the instant, so
    /// the root is evaluated in Event Mode but never searched.
    AnnouncedTime {
        param_index: usize,
        sign: TimeRootSign,
    },
    /// Reads only parameters and statically fixed coordinates: it cannot
    /// change within an accepted interval. Evaluated in Event Mode only.
    Static,
}

impl RootSearchRole {
    pub const fn is_searched(self) -> bool {
        matches!(self, Self::Search)
    }
}

/// The search role of every root-condition output, in output order.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RootSearchPlan {
    roles: Box<[RootSearchRole]>,
}

impl RootSearchPlan {
    /// Classify the problem's root-condition outputs.
    ///
    /// A block whose programs do not write consecutive outputs is searched
    /// whole: no row can be attributed to an output without that layout.
    pub fn derive(problem: &SolveProblem) -> Self {
        let roots = &problem.events.root_conditions;
        let static_y = problem
            .continuous
            .refresh_owners
            .root()
            .static_causal_rows()
            .iter()
            .map(|row| row.target_index())
            .collect::<BTreeSet<_>>();
        let layout = &problem.solve_layout;
        let inputs =
            layout.parameter_count..layout.parameter_count + layout.input_scalar_names.len();
        let roles = classify_block(roots, &static_y, &inputs)
            .unwrap_or_else(|| vec![RootSearchRole::Search; roots.output_count()]);
        Self {
            roles: roles.into_boxed_slice(),
        }
    }

    pub fn roles(&self) -> &[RootSearchRole] {
        &self.roles
    }

    pub fn len(&self) -> usize {
        self.roles.len()
    }

    pub fn is_empty(&self) -> bool {
        self.roles.is_empty()
    }

    /// Outputs searched during integration, ascending.
    pub fn searched(&self) -> impl Iterator<Item = usize> + '_ {
        self.roles
            .iter()
            .enumerate()
            .filter_map(|(index, role)| role.is_searched().then_some(index))
    }
}

/// The relation neighborhood of every root-condition output: the outputs,
/// itself included, that read a solver coordinate it reads. An event that
/// cycles between sides of one relation searches its neighborhood jointly
/// for a consistent mode (SPEC_0044 ME-EVENT-008).
pub fn root_neighborhoods(
    roots: &ScalarProgramBlock,
) -> Result<Vec<Vec<usize>>, crate::StructuralPatternError> {
    let mut reads = Vec::new();
    for program in roots.programs() {
        reads.extend(StructuralPattern::derive_output_y_dependencies(
            program, None,
        )?);
    }
    Ok(reads
        .iter()
        .enumerate()
        .map(|(this, read): (usize, &BTreeSet<usize>)| {
            reads
                .iter()
                .enumerate()
                .filter(|(index, other)| *index == this || !read.is_disjoint(other))
                .map(|(index, _)| index)
                .collect()
        })
        .collect())
}

fn classify_block(
    roots: &ScalarProgramBlock,
    static_y: &BTreeSet<usize>,
    inputs: &Range<usize>,
) -> Option<Vec<RootSearchRole>> {
    if !roots.uses_local_contiguous_output_indices() {
        return None;
    }
    let mut roles = Vec::with_capacity(roots.output_count());
    for row in roots.programs() {
        let output_count = ScalarProgramBlock::program_output_count(row);
        if output_count == 0 {
            return None;
        }
        let role = if output_count == 1
            && let Some((param_index, sign)) = direct_time_root(row)
        {
            RootSearchRole::AnnouncedTime { param_index, sign }
        } else if static_root(row, static_y, inputs) {
            RootSearchRole::Static
        } else {
            RootSearchRole::Search
        };
        roles.extend(std::iter::repeat_n(role, output_count));
    }
    (roles.len() == roots.output_count()).then_some(roles)
}

fn direct_time_root(row: &[LinearOp]) -> Option<(usize, TimeRootSign)> {
    let [
        first_load,
        second_load,
        LinearOp::Binary {
            dst,
            op: BinaryOp::Sub,
            lhs,
            rhs,
        },
        LinearOp::StoreOutput { src },
    ] = row
    else {
        return None;
    };
    if dst != src {
        return None;
    }
    let (time_reg, param_reg, param_index) = time_and_param_loads(first_load, second_load)?;
    if *lhs == param_reg && *rhs == time_reg {
        return Some((param_index, TimeRootSign::ParamMinusTime));
    }
    if *lhs == time_reg && *rhs == param_reg {
        return Some((param_index, TimeRootSign::TimeMinusParam));
    }
    None
}

fn time_and_param_loads(first: &LinearOp, second: &LinearOp) -> Option<(Reg, Reg, usize)> {
    match (first, second) {
        (
            LinearOp::LoadTime { dst: time_reg },
            LinearOp::LoadP {
                dst: param_reg,
                index,
            },
        )
        | (
            LinearOp::LoadP {
                dst: param_reg,
                index,
            },
            LinearOp::LoadTime { dst: time_reg },
        ) => Some((*time_reg, *param_reg, *index)),
        _ => None,
    }
}

/// Every P slot but an external input is fixed during one accepted interval, as
/// is a statically fixed `Y` coordinate: a declared parameter never changes,
/// and a discrete value, a `pre` slot, a condition memory, or a clock lane
/// changes only at an event, where the host refreshes the indicators. An
/// external input is not: the importer changes it between calls, and a
/// relation that reads one must be searched like a relation of a state
/// (`inputs` is the range of input slots). A row reading only fixed values,
/// and no time, seed, or effect, keeps its Event Mode value but offers no
/// surface to the continuous root finder.
fn static_root(row: &[LinearOp], static_y: &BTreeSet<usize>, inputs: &Range<usize>) -> bool {
    let Ok(y_dependencies) = StructuralPattern::derive_output_y_dependencies(row, None) else {
        return false;
    };
    if y_dependencies
        .iter()
        .any(|dependencies| !dependencies.is_subset(static_y))
    {
        return false;
    }
    StructuralPattern::derive_output_p_dependencies(row, None).is_ok_and(|dependencies| {
        dependencies
            .iter()
            .flatten()
            .all(|index| !inputs.contains(index))
    }) && none_depend(StructuralPattern::derive_output_time_dependencies(
        row, None,
    )) && none_depend(StructuralPattern::derive_output_seed_dependencies(
        row, None,
    )) && none_depend(StructuralPattern::derive_output_effect_dependencies(
        row, None,
    ))
}

fn none_depend<E>(dependencies: Result<Vec<bool>, E>) -> bool {
    dependencies.is_ok_and(|dependencies| dependencies.iter().all(|depends| !depends))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Slot 0 and 1 are declared parameters, 2 and 3 external inputs; slots 4
    /// and up are discrete values, `pre` slots, condition memories, and clock
    /// lanes.
    const INPUTS: Range<usize> = 2..4;

    #[test]
    fn a_plan_without_roles_is_empty() {
        let plan = RootSearchPlan {
            roles: Box::default(),
        };
        assert!(plan.is_empty());
        assert_eq!(plan.len(), 0);
    }

    #[test]
    fn continuous_static_root_uses_certified_tensor_dependency_flow() {
        let parameter_tensor_root = vec![
            crate::LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::P,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            crate::LinearOp::MatrixMultiply {
                dst_start: 2,
                lhs_start: 0,
                rhs_start: 0,
                rows: 1,
                inner: 2,
                columns: 1,
                lanes: 1,
            },
            crate::LinearOp::StoreOutput { src: 2 },
        ];
        assert!(static_root(
            &parameter_tensor_root,
            &BTreeSet::new(),
            &INPUTS
        ));

        let state_tensor_root = vec![
            crate::LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::Y,
                input_start: 3,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            crate::LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ];
        assert!(!static_root(
            &state_tensor_root,
            &BTreeSet::from([3]),
            &INPUTS
        ));
        assert!(static_root(
            &state_tensor_root,
            &BTreeSet::from([3, 4]),
            &INPUTS
        ));
    }

    #[test]
    fn continuous_static_root_rejects_time_dependency_through_register_flow() {
        let time_root = vec![
            crate::LinearOp::LoadTime { dst: 0 },
            crate::LinearOp::Const { dst: 1, value: 2.0 },
            crate::LinearOp::Binary {
                dst: 2,
                op: crate::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            crate::LinearOp::StoreOutput { src: 2 },
        ];
        assert!(!static_root(&time_root, &BTreeSet::new(), &INPUTS));
    }

    #[test]
    fn continuous_static_root_rejects_seed_dependency() {
        let seed_root = vec![
            crate::LinearOp::LoadSeed { dst: 0, index: 3 },
            crate::LinearOp::StoreOutput { src: 0 },
        ];
        assert!(!static_root(&seed_root, &BTreeSet::new(), &INPUTS));
    }

    #[test]
    fn a_root_reading_an_external_input_is_searched_and_every_other_slot_is_static() {
        let load = |index| {
            vec![
                crate::LinearOp::LoadP { dst: 0, index },
                crate::LinearOp::StoreOutput { src: 0 },
            ]
        };
        // Declared parameters, then the discrete, `pre`, condition-memory, and
        // clock-lane slots behind the inputs, stay fixed within an interval.
        for fixed in [0, 1, 4, 5, 9] {
            assert!(
                static_root(&load(fixed), &BTreeSet::new(), &INPUTS),
                "{fixed}"
            );
        }
        for input in [2, 3] {
            assert!(
                !static_root(&load(input), &BTreeSet::new(), &INPUTS),
                "{input}"
            );
        }
    }
}
