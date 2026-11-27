//! Typed, scalar regions for one tick of a construction-ordered clock partition.
use super::*;
use crate::codegen::render_solve::SolveRowValue;

fn row(program: &[solve::LinearOp]) -> Value {
    Value::from_object(SolveRowValue::new(Arc::new(vec![program.to_vec()]), 0))
}

fn scalar_assignment(update: &solve::GuardedAssignmentProgram) -> Option<Value> {
    use solve::LinearOp::{FunctionConditional, StoreOutputRange};
    let [
        FunctionConditional {
            dst_start, program, ..
        },
        StoreOutputRange {
            start, count: 1, ..
        },
    ] = update.program()
    else {
        return None;
    };
    let [range] = update.target_ranges() else {
        return None;
    };
    let solve::ScalarSlot::P { index, .. } = range.base() else {
        return None;
    };
    if dst_start != start
        || program.capture_count != 0
        || program.result_count != 1
        || range.count() != 1
    {
        return None;
    }
    let arms: Vec<_> = program
        .arms
        .iter()
        .map(|arm| {
            minijinja::context! {
                condition => row(&arm.condition), result => row(&arm.result)
            }
        })
        .collect();
    Some(minijinja::context! { target => index, arms => arms, fallback => row(&program.fallback) })
}

pub(super) fn assignments(problem: &solve::SolveProblem) -> Value {
    problem
        .discrete
        .clock_partition_order
        .iter()
        .map(|step| {
            let solve::ClockPartitionStep::GuardedAssignment { program_index } = step else {
                return Value::UNDEFINED;
            };
            scalar_assignment(&problem.discrete.guarded_assignments[*program_index])
                .unwrap_or(Value::UNDEFINED)
        })
        .collect()
}

pub(super) fn pulse_memories(problem: &solve::SolveProblem) -> Value {
    let discrete = &problem.discrete;
    discrete
        .rhs
        .programs()
        .iter()
        .zip(&discrete.update_targets)
        .map(|(program, target)| {
            minijinja::context! { program => row(program), target => target }
        })
        .collect()
}
