//! Discrete-partition refusals whose shapes the checked Solve construction
//! also constrains; they are proved on the admission itself.

use super::*;
use crate::{LinearOp, ScalarProgramBlock};

fn rows(programs: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_static_partition.mo"),
        0,
        1,
    );
    ScalarProgramBlock::with_source_span(
        programs,
        span.require_provenance("static partition fixture").unwrap(),
    )
    .unwrap()
}

/// A two-parameter kernel with one condition memory and two discrete outputs
/// of one program, writing `p[0]` and `p[1]` with the given roles.
fn partition(roles: [DiscreteRowRole; 2], targets: [ScalarSlot; 2]) -> SolveModel {
    let mut model = SolveModel::default();
    model.problem.layout = crate::VarLayout::from_parts(Default::default(), 1, 2);
    model.problem.events.condition_memory_parameter_indices = vec![0];
    let discrete = &mut model.problem.discrete;
    discrete.rhs = rows(vec![vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    discrete.update_targets = targets.to_vec();
    discrete.row_roles = roles.to_vec();
    model
}

fn p(index: usize) -> ScalarSlot {
    ScalarSlot::P {
        index,
        byte_offset: 8 * index,
    }
}

#[test]
fn a_program_mixing_equations_and_condition_memories_is_refused() {
    let model = partition(
        [DiscreteRowRole::Equation, DiscreteRowRole::ConditionMemory],
        [p(0), p(1)],
    );
    assert_eq!(
        validate(&model).map(|_| ()),
        Err("a discrete program mixes equations and condition memories")
    );
}

#[test]
fn a_discrete_row_writing_solver_storage_is_refused() {
    let model = partition(
        [DiscreteRowRole::Equation, DiscreteRowRole::Equation],
        [
            p(0),
            ScalarSlot::Y {
                index: 0,
                byte_offset: 0,
            },
        ],
    );
    let refused = validate(&model).map(|_| ()).unwrap_err();
    assert!(
        refused.starts_with("the C profile executes only parameter-determined discrete equations"),
        "{refused}"
    );
}

#[test]
fn equations_and_memories_admit_in_their_own_orders() {
    let model = partition(
        [
            DiscreteRowRole::ConditionMemory,
            DiscreteRowRole::ConditionMemory,
        ],
        [p(0), p(1)],
    );
    let order = validate(&model).expect("constant condition memories are static");
    assert!(order.equations.is_empty());
    assert_eq!(order.memories, vec![0]);
}

const EVENT_ITERATION: &str = "the C profile executes only parameter-determined discrete equations";

fn assert_event_iteration(mutate: impl FnOnce(&mut crate::DiscreteSolveSystem)) {
    let mut model = partition(
        [
            DiscreteRowRole::ConditionMemory,
            DiscreteRowRole::ConditionMemory,
        ],
        [p(0), p(1)],
    );
    mutate(&mut model.problem.discrete);
    let refused = validate(&model).map(|_| ()).unwrap_err();
    assert!(refused.starts_with(EVENT_ITERATION), "{refused}");
}

#[test]
fn runtime_post_commit_and_structured_updates_need_event_iteration() {
    let row = || {
        rows(vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]])
    };
    assert_event_iteration(|discrete| discrete.runtime_assignment_rhs = row());
    assert_event_iteration(|discrete| discrete.post_commit_assignment_rhs = row());
    assert_event_iteration(|discrete| {
        discrete.structured_rhs = crate::ComputeBlock::from_scalar_program_block(row());
    });
}

/// Whole-tensor kernels settle exactly when every operand element settles; a
/// state operand anywhere in a range keeps the result unsettled.
#[test]
fn tensor_kernels_settle_from_their_complete_operand_ranges() {
    use crate::{TensorConcatenateSource, TensorInputKind};
    let program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::P,
            input_start: 0,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::LoadP { dst: 3, index: 3 },
        LinearOp::LoadY { dst: 4, index: 0 },
        LinearOp::LoadP { dst: 5, index: 4 },
        LinearOp::TensorCross {
            dst_start: 6,
            lhs_start: 0,
            rhs_start: 3,
            lanes: 1,
        },
        LinearOp::TensorCross {
            dst_start: 9,
            lhs_start: 0,
            rhs_start: 0,
            lanes: 1,
        },
        LinearOp::MatrixMultiply {
            dst_start: 12,
            lhs_start: 0,
            rhs_start: 0,
            rows: 1,
            inner: 3,
            columns: 1,
            lanes: 1,
        },
        LinearOp::TensorConcatenate {
            dst_start: 13,
            sources: vec![
                TensorConcatenateSource {
                    start: 0,
                    dimensions: vec![3].into(),
                },
                TensorConcatenateSource {
                    start: 3,
                    dimensions: vec![3].into(),
                },
            ]
            .into(),
            dimensions: vec![6].into(),
            axis: 0,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 6 },
        LinearOp::StoreOutput { src: 9 },
        LinearOp::StoreOutput { src: 12 },
        LinearOp::StoreOutput { src: 13 },
    ];
    let settled = scalar_dependencies::program_outputs(
        &crate::SolvePureCallTable::default(),
        &program,
        &[false],
        &[true; 5],
    );
    assert_eq!(settled, Some(vec![false, true, true, false]));
}
