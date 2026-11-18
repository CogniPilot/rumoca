//! The kinematic partition as it was derived before the closure held
//! intervals and each output carried its program's obligations apart: every
//! output's reads joined with the obligations, every column visited on its
//! own. The interval derivation must give each state the same reason.

use std::collections::BTreeSet;

use super::super::closure_oracle_tests::{ScalarClosure, execution_rows, oracle_fixtures};
use super::*;

/// What one output row reads, joined with its program's obligations.
struct ScalarReads {
    columns: Vec<usize>,
    taint: Option<NonKinematicReason>,
    copy_of: Option<usize>,
}

fn scalar_reads(programs: &ScalarProgramBlock) -> Vec<ScalarReads> {
    programs
        .programs()
        .iter()
        .flat_map(|program| {
            let reads =
                StructuralPattern::derive_output_execution_y_dependency_ranges(program, None)
                    .expect("fixture reads are derivable");
            let effect = StructuralPattern::execution_has_effect(program, None)
                .expect("fixture effects are derivable");
            let taint =
                relation_or_solve(program).or(effect.then_some(NonKinematicReason::Unanalysable));
            let copies = output_copies(program);
            reads
                .into_iter()
                .enumerate()
                .map(move |(position, columns)| ScalarReads {
                    columns: columns.iter().collect(),
                    taint,
                    copy_of: copies.get(position).copied().flatten(),
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn scalar_owners(
    columns: &[usize],
    closure: &ScalarClosure,
    (state_count, solver_count): (usize, usize),
) -> BTreeSet<usize> {
    columns
        .iter()
        .filter(|&&column| column >= state_count && column < solver_count)
        .map(|&column| {
            closure
                .owner_block(column)
                .expect("fixture algebraic has a block")
        })
        .collect()
}

fn scalar_block_taint(
    blocks: &[AlgebraicProjectionBlock],
    implicit: &[ScalarReads],
    closure: &ScalarClosure,
    solver_count: usize,
) -> Vec<Option<NonKinematicReason>> {
    let outside = |columns: &[usize]| columns.last().is_some_and(|&last| last >= solver_count);
    let mut taint = blocks
        .iter()
        .map(|block| {
            let simultaneous = (block.y_indices.len() > 1 || block.tearing.is_some())
                .then_some(NonKinematicReason::SimultaneousBlock);
            block.rows.iter().fold(simultaneous, |taint, &row| {
                taint
                    .or(implicit[row].taint)
                    .or(outside(&implicit[row].columns).then_some(NonKinematicReason::Unanalysable))
            })
        })
        .collect::<Vec<_>>();
    let mut changed = true;
    while changed {
        changed = false;
        for (index, block) in blocks.iter().enumerate() {
            if taint[index].is_some() {
                continue;
            }
            let reached = block
                .rows
                .iter()
                .flat_map(|&row| implicit[row].columns.iter())
                .find_map(|&column| {
                    closure
                        .owner_block(column)
                        .filter(|&owner| owner != index)
                        .and_then(|owner| taint[owner])
                });
            if reached.is_some() {
                taint[index] = reached;
                changed = true;
            }
        }
    }
    taint
}

/// The reasons the scalar derivation gives each state.
fn scalar_reasons(
    derivative: &ScalarProgramBlock,
    implicit: &ScalarProgramBlock,
    plan: &AlgebraicProjectionPlan,
    (state_count, solver_count): (usize, usize),
) -> Vec<Option<NonKinematicReason>> {
    let derivative = scalar_reads(derivative);
    let implicit = scalar_reads(implicit);
    let relation = implicit
        .iter()
        .map(|output| output.columns.clone())
        .collect::<Vec<_>>();
    let closure = ScalarClosure::derive(&relation, plan, (state_count, solver_count));
    let block_taint = scalar_block_taint(
        plan.blocks.scalar_blocks(),
        &implicit,
        &closure,
        solver_count,
    );
    let mut reasons = vec![None; state_count];
    let mut read_blocks = Vec::with_capacity(state_count);
    for (state, row) in derivative.iter().enumerate() {
        let owners = scalar_owners(&row.columns, &closure, (state_count, solver_count));
        let outside = row
            .columns
            .last()
            .is_some_and(|&last| last >= solver_count)
            .then_some(NonKinematicReason::Unanalysable);
        reasons[state] = reasons[state]
            .or(row.taint)
            .or(outside)
            .or_else(|| owners.iter().find_map(|&block| block_taint[block]));
        if let Some(velocity) = row
            .copy_of
            .filter(|&slot| slot < state_count && slot != state)
        {
            reasons[velocity] =
                reasons[velocity].or(Some(NonKinematicReason::Velocity { of: state }));
        }
        read_blocks.push(owners);
    }
    let mut changed = true;
    while changed {
        changed = false;
        for state in 0..state_count {
            if reasons[state].is_some() {
                continue;
            }
            let through = read_blocks[state].iter().find_map(|&block| {
                closure.block_states[block]
                    .iter()
                    .copied()
                    .find(|&state| reasons.get(state).is_none_or(Option::is_some))
            });
            if let Some(through) = through {
                reasons[state] = Some(NonKinematicReason::Closure { through });
                changed = true;
            }
        }
    }
    reasons
}

#[test]
fn the_interval_closure_matches_the_scalar_closure() {
    for (name, system) in oracle_fixtures() {
        let implicit = output_reads(&system.implicit, system.implicit.output_count())
            .expect("fixture reads are derivable");
        let counts = (system.states, system.solver);
        let closure = AlgebraicClosure::derive(
            Some(&ImplicitRelation(&implicit)),
            &system.plan,
            counts,
            None,
        )
        .expect("fixture closure");
        let oracle = ScalarClosure::derive(&execution_rows(&system.implicit), &system.plan, counts);
        for (block, states) in oracle.block_states.iter().enumerate() {
            let actual = closure
                .block_states(block)
                .expect("every block has states")
                .iter()
                .collect::<BTreeSet<_>>();
            assert_eq!(&actual, states, "{name}: block {block}");
        }
    }
}

#[test]
fn the_kinematic_reasons_match_the_scalar_derivation() {
    for (name, system) in oracle_fixtures() {
        let partition = system.partition();
        let oracle = scalar_reasons(
            &system.derivative,
            &system.implicit,
            &system.plan,
            (system.states, system.solver),
        );
        let actual = (0..system.states)
            .map(|state| partition.reason(state))
            .collect::<Vec<_>>();
        assert_eq!(actual, oracle, "{name}");
    }
}
