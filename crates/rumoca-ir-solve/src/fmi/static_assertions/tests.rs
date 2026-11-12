//! Discrete-partition refusals whose shapes the checked Solve construction
//! also constrains; they are proved on the admission itself.

use super::*;
use crate::{LinearOp, ScalarProgramBlock, TypedProgram};

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

/// What the early-exit predicate of `counted_fold` reads each pass.
#[derive(Clone, Copy)]
enum ExitTest {
    /// The counter against the captured input.
    CounterBelowInput,
    /// The counter against a constant.
    CounterBelowConstant,
}

/// How `counted_fold` updates its second carry each pass.
#[derive(Clone, Copy)]
enum Counter {
    /// `p := p + 1`.
    Steps,
    /// `p := p + x` for the captured input `x`.
    AddsInput,
}

/// `a := a + 1` with a second carry `p` over `1:3`, tested before each pass
/// by `test`; the output is `a` and the one input is the capture `x`.
fn counted_fold(test: ExitTest, counter: Counter) -> TypedProgram {
    use crate::{
        SolveArithmeticProfile, SolveBinaryOperator, SolveCompareOperator, SolveIntegerDomain,
        SolveRealFormat, SolveScalarType, SolveSlotAccess, SolveStorageClass, SolveValue,
        SolveValueType,
    };
    use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
    let span = |start| {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fmi_static_fold.mo"),
            start,
            start + 1,
        )
    };
    let arithmetic =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    TypedProgram::construct(arithmetic, |builder| {
        let input = builder.declare_slot(
            real.clone(),
            SolveStorageClass::Input,
            SolveSlotAccess::ReadOnly,
            span(0),
        )?;
        let output = builder.declare_slot(
            real.clone(),
            SolveStorageClass::Output,
            SolveSlotAccess::ReadWrite,
            span(1),
        )?;
        let x = builder.load(input, span(2))?;
        let a = builder.constant(SolveValue::real(arithmetic, 0.0), span(3))?;
        let p = builder.constant(SolveValue::real(arithmetic, 0.0), span(3))?;
        let results = builder.fold_while(
            domain.clone(),
            &[a, p],
            &[x],
            span(4),
            |region, carried, captures, _, outputs| {
                let counter = region.load(carried[1], span(5))?;
                let bound = match test {
                    ExitTest::CounterBelowInput => region.load(captures[0], span(6))?,
                    ExitTest::CounterBelowConstant => {
                        region.constant(SolveValue::real(arithmetic, 3.0), span(6))?
                    }
                };
                let below = region.compare(SolveCompareOperator::Less, counter, bound, span(7))?;
                region.store(outputs[0], below, span(7))
            },
            |region, carried, captures, _, outputs| {
                let a = region.load(carried[0], span(8))?;
                let p = region.load(carried[1], span(8))?;
                let one = region.constant(SolveValue::real(arithmetic, 1.0), span(9))?;
                let step = match counter {
                    Counter::Steps => one,
                    Counter::AddsInput => region.load(captures[0], span(9))?,
                };
                let a = region.binary(SolveBinaryOperator::Add, a, one, span(10))?;
                let p = region.binary(SolveBinaryOperator::Add, p, step, span(11))?;
                region.store(outputs[0], a, span(10))?;
                region.store(outputs[1], p, span(11))
            },
        )?;
        builder.store(output, results[0], span(12))
    })
    .expect("a bounded fold with an early-exit predicate constructs")
}

/// Whether the output of `counted_fold(test, counter)` is stable when the
/// captured input is `stable_input`.
fn counted_fold_is_stable(test: ExitTest, counter: Counter, stable_input: bool) -> bool {
    let program = counted_fold(test, counter);
    let stable = typed_dependencies::outputs(
        &crate::SolvePureCallTable::default(),
        &program,
        &[stable_input],
    );
    assert_eq!(stable.len(), 1, "one output");
    stable[0]
}

/// The stop count decides the value of `a`, so a predicate that follows an
/// input which can change makes `a` follow it, although the transition reads
/// no input at all.
#[test]
fn a_fold_whose_exit_test_reads_a_changing_input_is_not_stable() {
    let test = ExitTest::CounterBelowInput;
    assert!(!counted_fold_is_stable(test, Counter::Steps, false));
    assert!(counted_fold_is_stable(test, Counter::Steps, true));
}

#[test]
fn a_fold_whose_exit_test_reads_only_stable_values_stays_stable() {
    let test = ExitTest::CounterBelowConstant;
    // A changing input the passes and the exit test never read stays out of it.
    assert!(counted_fold_is_stable(test, Counter::Steps, false));
    assert!(counted_fold_is_stable(test, Counter::Steps, true));
}

/// The exit test reads only the counter, which the passes update from the
/// input: the input still decides the stop count.
#[test]
fn a_fold_whose_exit_test_reads_a_carry_updated_from_a_changing_input_is_not_stable() {
    let test = ExitTest::CounterBelowConstant;
    assert!(!counted_fold_is_stable(test, Counter::AddsInput, false));
    assert!(counted_fold_is_stable(test, Counter::AddsInput, true));
}
