use super::*;
use crate::{
    ComputeBlock, FunctionConditionalProgram, ScalarProgramBlock, SolveArithmeticProfile,
    SolveAssertionActionProjection, SolveEventAction, SolveEventMessage, SolveEventMessagePart,
    SolveIntegerDomain, SolveModel, SolvePureCallIdentity, SolvePureCallOutput, SolveRealFormat,
    SolveScalarType, SolveValue, SolveValueType,
};
use rumoca_core::{SourceId, Span};
use std::{num::NonZeroU64, sync::Arc};

fn span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("assigned-read-coverage.mo"),
        1,
        2,
    )
}

fn table() -> SolvePureCallTable {
    let profile =
        SolveArithmeticProfile::construct(SolveRealFormat::Binary64, SolveIntegerDomain::FULL);
    SolvePureCallTable::construct(profile, |table| {
        for id in 1..=2 {
            table.add_owner(
                SolvePureCallIdentity::issued(NonZeroU64::new(id).unwrap()),
                vec![],
                vec![
                    SolvePureCallOutput::result(SolveValueType::scalar(SolveScalarType::real(
                        profile,
                    ))),
                    SolvePureCallOutput::assertion_predicate(),
                ],
                span(),
                |body, _, outputs| {
                    let value = body.constant(SolveValue::real(profile, 7.0), span())?;
                    let state = body.present_assigned_state(value, span())?;
                    let assertion = body.assertion_output(1, span())?;
                    let (condition, projected) =
                        body.require_assigned_states(assertion, &[state], span())?;
                    body.store(outputs[0], projected[0], span())?;
                    body.store(outputs[1], condition, span())
                },
            )?;
        }
        Ok(())
    })
    .unwrap()
}

fn problem(table: &SolvePureCallTable) -> SolveProblem {
    let mut problem = SolveProblem::default();
    problem.events.actions = table
        .owners()
        .iter()
        .map(|owner| SolveEventAction {
            kind: SolveEventActionKind::Assert,
            message: SolveEventMessage {
                parts: vec![SolveEventMessagePart::Text("original read error".into())],
            },
            span: span(),
            origin: "source assigned read".into(),
            clock_owner: None,
            assertion_projection: SolveAssertionActionProjection::new(owner.call_site(), 1),
        })
        .collect();
    problem
}

fn call(table: &SolvePureCallTable, owner: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::PureCall {
            dst_start: 0,
            input_starts: vec![].into(),
            site: table.owners()[owner].call_site(),
        },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn stage(program: Vec<LinearOp>) -> NativeRefreshAssignmentStage {
    NativeRefreshAssignmentStage {
        integer_bindings: Default::default(),
        source: super::super::NativeStageSource::Continuous { node: 0 },
        targets: super::super::coverage::Coverage::dense(0..1),
        value_kernel: ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_program_spans(vec![program], vec![span()]).unwrap(),
        ),
    }
}

#[test]
fn model_assigned_read_coverage_requires_every_original_action_in_value_stages() {
    let table = table();
    let problem = problem(&table);
    let guards = AssignedReadActions::classify(&problem, &table).unwrap();
    let stages = [stage(call(&table, 0)), stage(call(&table, 1))];
    assert_eq!(guards.check_stage_coverage(&stages), Ok(()));
    assert_eq!(
        guards.check_stage_coverage(&stages[..1]),
        Err(NativeEvaluationRefusal::EventActions)
    );
    assert_eq!(
        guards.check_stage_coverage(&[]),
        Err(NativeEvaluationRefusal::EventActions)
    );
    assert!(!guards.belongs_to(&problem.clone()));

    let program = call(&table, 0);
    let invocation =
        CheckedAssertionInvocation::new(&table, &program, 0, &problem.events.actions).unwrap();
    assert!(std::ptr::eq(invocation.table(), &table));
    assert!(std::ptr::eq(invocation.program(), program.as_slice()));
    assert!(std::ptr::eq(
        invocation.actions(),
        problem.events.actions.as_slice()
    ));
    assert!(std::ptr::eq(
        invocation.projections()[0].action(),
        &problem.events.actions[0]
    ));
    let mut unbound = problem.clone();
    unbound.events.actions[0].assertion_projection = None;
    assert!(AssignedReadActions::classify(&unbound, &table).is_none());
    let mut warning = problem.clone();
    warning.events.actions[0].kind = SolveEventActionKind::Warning;
    assert!(AssignedReadActions::classify(&warning, &table).is_none());
}

#[test]
fn model_assigned_read_coverage_visits_selected_and_unselected_nested_sources() {
    let table = table();
    let problem = problem(&table);
    let guards = AssignedReadActions::classify(&problem, &table).unwrap();
    let conditional = FunctionConditionalProgram::checked(
        0,
        vec![1],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            call(&table, 0),
        )],
        call(&table, 1),
    )
    .unwrap();
    let stage = stage(vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: Arc::new(conditional),
        },
        LinearOp::StoreOutput { src: 0 },
    ]);
    assert_eq!(guards.check_stage_coverage(&[stage]), Ok(()));
}

#[test]
fn native_schedule_table_binding_distinguishes_shared_and_independent_replay_owners() {
    let mut model = SolveModel {
        pure_calls: table(),
        ..SolveModel::default()
    };
    model.problem.layout = crate::VarLayout::from_parts(Default::default(), 1, 0);
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 7.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: crate::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            vec![span()],
        )
        .unwrap(),
    );
    model.problem.continuous.implicit_row_targets = vec![Some(crate::scalar_slot_y(0))].into();
    let schedule = super::super::derive_for_model(&model).unwrap();
    assert_eq!(schedule.stages().len(), 1);
    assert_eq!(schedule.stages()[0].target_range(), Some(0..1));
    assert!(schedule.matches_call_table(&model.pure_calls));
    assert!(schedule.matches_call_table(&model.pure_calls.clone()));
    let replay: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&model.pure_calls).unwrap()).unwrap();
    assert_eq!(replay, model.pure_calls);
    assert!(!schedule.matches_call_table(&replay));
}
