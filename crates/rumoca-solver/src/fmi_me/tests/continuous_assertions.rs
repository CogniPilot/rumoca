use super::{block, fixture_instance_config, refresh_owned, strict_root_relation_memory};
use crate::fmi_me::{MeError, MeModelSource, MeTime, SolveMeKernel};
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve as solve;

#[test]
fn a_crossed_indicator_defers_the_assertion_at_the_completed_point() {
    let mut model = strict_root_relation_memory();
    model.problem.events.action_conditions = block(
        vec![vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::Const {
                dst: 1,
                value: 0.25,
            },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        "completed_step_assertion.mo",
    );
    model.problem.events.actions = vec![solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Assert,
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Text(
                "accepted violation".into(),
            )],
        },
        span: Span::from_offsets(
            SourceId::from_source_name("completed_step_assertion.mo"),
            1,
            2,
        ),
        origin: "Fixture.assertion".into(),
        clock_owner: None,
        assertion_projection: None,
    }];
    let model = refresh_owned(model);
    let complete = |state| {
        let config = fixture_instance_config().importer_driven();
        let mut kernel = SolveMeKernel::instantiate(MeModelSource::fixture(&model), &config)?;
        kernel.enter_initialization_mode()?;
        kernel.exit_initialization_mode()?;
        while kernel.update_discrete_states()?.discrete_states_need_update {}
        kernel.enter_continuous_time_mode()?;
        kernel.set_time(MeTime::at(0.5))?;
        kernel.set_continuous_states(&[state])?;
        kernel.completed_integrator_step(false)
    };

    // The root crosses as x moves from -1 to +0.5, so event processing
    // owns the assertion check. The same time without a crossing fails here.
    assert!(complete(0.5).unwrap().enter_event_mode);
    match complete(-0.5) {
        Err(MeError::Assertion { time, message }) => {
            assert_eq!(time.to_bits(), 0.5_f64.to_bits());
            assert_eq!(message, "accepted violation");
        }
        other => panic!("expected a completed-step assertion failure, got {other:?}"),
    }
}
