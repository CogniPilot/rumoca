//! Algebraic refresh targets that are static during continuous-time mode.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_solve as solve;
use rumoca_ir_solve::RefreshPlan;

use super::RefreshProgramAccess;
use super::static_domain::ContinuousStaticParameters;

pub(super) fn parameter_static_refresh_targets<A: RefreshProgramAccess + ?Sized>(
    plan: &RefreshPlan,
    block: &A,
    state_count: usize,
    continuous_static_parameters: ContinuousStaticParameters,
) -> BTreeSet<usize> {
    // Begin with the complete algebraic candidate set and remove every target
    // whose dependency closure reaches time, a state, or a non-candidate.
    // This greatest-fixed-point direction is load-bearing for parameter-only
    // algebraic loops: a closed simultaneous block can be static even though
    // none of its members is independently orderable from parameters first.
    // The rows of one program share its dependencies, derived once.
    let mut programs: BTreeMap<(usize, usize), Option<ParameterStaticDependencies>> =
        BTreeMap::new();
    let candidates = plan
        .causal_rows()
        .iter()
        .map(|refresh_row| {
            let program = block.source_program(refresh_row.source()).map(|program| {
                let key = (program.as_ptr() as usize, program.len());
                programs
                    .entry(key)
                    .or_insert_with(|| ParameterStaticDependencies::derive(program));
                key
            });
            (refresh_row.target_index(), program)
        })
        .collect::<Vec<_>>();
    let mut static_targets = candidates
        .iter()
        .map(|(target, _)| *target)
        .collect::<BTreeSet<_>>();
    loop {
        let blockers = programs
            .iter()
            .map(|(key, dependencies)| {
                let blockers = dependencies.as_ref().and_then(|dependencies| {
                    dependencies
                        .independent(continuous_static_parameters)
                        .then(|| dependencies.blockers(state_count, &static_targets))
                });
                (*key, blockers)
            })
            .collect::<BTreeMap<_, _>>();
        let rejected = candidates
            .iter()
            .filter(|(target, _)| static_targets.contains(target))
            .filter(|(target, program)| {
                !program
                    .and_then(|key| blockers.get(&key)?.as_ref())
                    .is_some_and(|blockers| blockers.iter().all(|index| index == target))
            })
            .map(|(target, _)| *target)
            .collect::<Vec<_>>();
        if rejected.is_empty() {
            return static_targets;
        }
        for target in rejected {
            static_targets.remove(&target);
        }
    }
}

/// The dependencies of one refresh program, as unions over its outputs.
struct ParameterStaticDependencies {
    y: BTreeSet<usize>,
    parameters: BTreeSet<usize>,
    time: bool,
    seed: bool,
    effect: bool,
}

impl ParameterStaticDependencies {
    fn derive(program: &[solve::LinearOp]) -> Option<Self> {
        let any = |flags: Vec<bool>| flags.into_iter().any(|flag| flag);
        Some(Self {
            y: solve::StructuralPattern::derive_y_dependency_union(program, None).ok()?,
            parameters: solve::StructuralPattern::derive_p_dependency_union(program, None).ok()?,
            time: any(
                solve::StructuralPattern::derive_output_time_dependencies(program, None).ok()?,
            ),
            seed: any(
                solve::StructuralPattern::derive_output_seed_dependencies(program, None).ok()?,
            ),
            effect: any(
                solve::StructuralPattern::derive_output_effect_dependencies(program, None).ok()?,
            ),
        })
    }

    /// Whether the program reads only static parameters and no time, seed, or
    /// effect.
    fn independent(&self, continuous_static_parameters: ContinuousStaticParameters) -> bool {
        self.parameters
            .iter()
            .all(|index| continuous_static_parameters.contains(*index))
            && !self.time
            && !self.seed
            && !self.effect
    }

    /// Up to two solver values the program reads that are not static
    /// algebraic targets; a row is static when the only one is its own target.
    fn blockers(&self, state_count: usize, static_targets: &BTreeSet<usize>) -> Vec<usize> {
        self.y
            .iter()
            .copied()
            .filter(|index| !(*index >= state_count && static_targets.contains(index)))
            .take(2)
            .collect()
    }

    #[cfg(test)]
    fn is_parameter_static(
        &self,
        target_index: usize,
        state_count: usize,
        static_targets: &BTreeSet<usize>,
        continuous_static_parameters: ContinuousStaticParameters,
    ) -> bool {
        self.independent(continuous_static_parameters)
            && self
                .blockers(state_count, static_targets)
                .iter()
                .all(|index| {
                    parameter_static_y_index(*index, target_index, state_count, static_targets)
                })
    }
}

#[cfg(test)]
pub(super) fn parameter_static_refresh_program(
    program: &[solve::LinearOp],
    target_index: usize,
    state_count: usize,
    static_targets: &BTreeSet<usize>,
    continuous_static_parameters: ContinuousStaticParameters,
) -> bool {
    ParameterStaticDependencies::derive(program).is_some_and(|dependencies| {
        dependencies.is_parameter_static(
            target_index,
            state_count,
            static_targets,
            continuous_static_parameters,
        )
    })
}

#[cfg(test)]
fn parameter_static_y_index(
    index: usize,
    target_index: usize,
    state_count: usize,
    static_targets: &BTreeSet<usize>,
) -> bool {
    index == target_index || (index >= state_count && static_targets.contains(&index))
}
