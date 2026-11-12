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
        validate(&model).map(|_| ()).map_err(StaticRefusal::message),
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
    let refused = validate(&model).map(|_| ()).unwrap_err().message();
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
    let refused = validate(&model).map(|_| ()).unwrap_err().message();
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

/// An owner returning its first input after checking its second is finite
/// (or not, without `guard`), and a program that calls it on `(1, y[0])`
/// and stores a constant: the call's result is unused.
fn unused_call(guard: bool) -> (crate::SolvePureCallTable, Vec<LinearOp>) {
    let profile = crate::SolvePureCallTable::default().arithmetic();
    let real = crate::SolveValueType::scalar(crate::SolveScalarType::real(profile));
    let span = crate::source_span_from_offsets(820, 1, 2);
    let table = crate::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            crate::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(820).unwrap()),
            vec![real.clone(), real.clone()],
            vec![crate::SolvePureCallOutput::result(real)],
            span,
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span)?;
                let q = b.load(inputs[1], span)?;
                if guard {
                    b.check_finite(q, crate::SolveFiniteCheck::Derivative, span)?;
                }
                b.store(outputs[0], x, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let program = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: table.owners()[0].call_site(),
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    (table, program)
}

#[test]
fn an_unused_guard_on_an_unsettled_value_unsettles_its_program() {
    for guard in [false, true] {
        let (table, program) = unused_call(guard);
        // The stored value is a constant either way.
        assert_eq!(
            scalar_dependencies::program_outputs(&table, &program, &[false], &[]),
            Some(vec![!guard]),
            "guard={guard}"
        );
        assert_eq!(
            scalar_dependencies::program_outputs(&table, &program, &[true], &[]),
            Some(vec![true]),
            "guard={guard} over a settled value"
        );
    }
}

#[test]
fn an_unused_failable_operation_on_an_unsettled_value_unsettles_its_program() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::LinearSolveComponent {
            dst: 2,
            matrix_start: 0,
            rhs_start: 1,
            n: 1,
            component: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let table = crate::SolvePureCallTable::default();
    assert_eq!(
        scalar_dependencies::program_outputs(&table, &program, &[false], &[]),
        Some(vec![false])
    );
    assert_eq!(
        scalar_dependencies::program_outputs(&table, &program, &[true], &[]),
        Some(vec![true])
    );
}
