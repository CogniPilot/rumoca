use super::*;
use crate::{LinearOp, ScalarProgramBlock, ScalarSlot, SolveDelayPartition};

fn component(operation: LinearOp) -> FmiComponent {
    let (mut model, input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_parameter_profile.mo"),
        0,
        1,
    );
    model.problem.initialization.update_rhs = ScalarProgramBlock::with_source_span(
        vec![vec![operation, LinearOp::StoreOutput { src: 0 }]],
        span.require_provenance("parameter binding fixture")
            .unwrap(),
    )
    .unwrap();
    model.problem.initialization.update_targets = vec![ScalarSlot::P {
        index: 1,
        byte_offset: 8,
    }];
    FmiComponent::construct(model, vec![input]).expect("valid initialization assignment")
}

#[test]
fn initialization_owner_determines_calculated_parameter_metadata() {
    let component = component(LinearOp::Const { dst: 0, value: 2.0 });
    assert_eq!(
        component.variables()[0].causality(),
        FmiCausality::CalculatedParameter
    );
    component
        .into_codegen_view()
        .try_c()
        .expect("constant parameter update is executable");
}

#[test]
fn c_profile_rejects_unsettled_parameter_bindings_on_both_entry_paths() {
    for operation in [
        LinearOp::LoadTime { dst: 0 },
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::LoadP { dst: 0, index: 0 },
    ] {
        let error = component(operation.clone())
            .into_codegen_view()
            .try_c()
            .expect_err("time, a binding cycle, and non-parameter storage are not settled");
        assert!(error.to_string().contains("unsettled or non-parameter"));
        let event_free = component(operation)
            .into_codegen_view()
            .try_event_free()
            .unwrap();
        assert!(FmiCCodegenView::try_from(event_free).is_err());
    }
}

#[test]
fn parameter_settlement_follows_issued_output_indices() {
    let (mut model, mut input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = SolveDelayPartition::default();
    model.problem.solve_layout.variable_storage_runs[0].base = ScalarSlot::P {
        index: 0,
        byte_offset: 0,
    };
    model.problem.solve_layout.variable_storage_runs[0].scalar_count = 2;
    input.dimensions = vec![2];
    input.scalar_names = vec!["p[1]".into(), "p[2]".into()];
    input.start = vec![0.0, 0.0];
    let span = input.declaration;
    model.problem.initialization.update_rhs = ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        ],
        vec![span; 2],
        vec![1, 0],
    )
    .unwrap();
    model.problem.initialization.update_targets = vec![
        ScalarSlot::P {
            index: 0,
            byte_offset: 0,
        },
        ScalarSlot::P {
            index: 1,
            byte_offset: 8,
        },
    ];
    let component = FmiComponent::construct(model, vec![input]).unwrap();
    let error = component
        .into_codegen_view()
        .try_c()
        .expect_err("the first output settles p[2]; p[1] still reads its own unsettled value");
    assert!(error.to_string().contains("unsettled or non-parameter"));
}
