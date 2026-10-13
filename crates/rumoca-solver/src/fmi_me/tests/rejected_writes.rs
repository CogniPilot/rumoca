//! A rejected parameter write leaves the component on its previous values.

use rumoca_ir_solve as solve;

use super::{block, fixture_instance_config, refresh_owned};
use crate::fmi_me::{MeModelSource, SolveMeKernel};

/// One Float64 input `u` (parameter 0) that is the delay time of a channel
/// whose delayed value lands in parameter 1.
fn input_delayed_model() -> solve::SolveModel {
    let constant_row = |value, source| {
        block(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            source,
        )
    };
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.problem.solve_layout.input_scalar_names = vec!["u".into()].into();
    model.parameters = vec![0.2, 0.0].into();
    model.problem.layout = solve::VarLayout::from_parts(
        [
            ("u".into(), solve::scalar_slot_p(0)),
            ("delayed".into(), solve::scalar_slot_p(1)),
        ]
        .into(),
        0,
        2,
    );
    model.problem.events.delays = solve::SolveDelayPartition {
        source_rhs: constant_row(3.0, "fmi_me_input_delay_source.mo"),
        delay_time_rhs: block(
            vec![vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_input_delay_time.mo",
        ),
        delay_max_rhs: constant_row(1.0, "fmi_me_input_delay_max.mo"),
        value_parameter_indices: vec![1],
        source_is_discrete: vec![false],
    };
    refresh_owned(model)
}

#[test]
fn an_input_the_delay_rejects_leaves_the_previous_input_in_place() {
    let model = input_delayed_model();
    let mut kernel =
        SolveMeKernel::instantiate(MeModelSource::fixture(&model), &fixture_instance_config())
            .expect("the delayed-input component instantiates");
    kernel.enter_initialization_mode().unwrap();
    kernel.exit_initialization_mode().unwrap();
    kernel.update_discrete_states().unwrap();
    kernel.enter_continuous_time_mode().unwrap();
    let input = kernel.value_reference("u").expect("the input is declared");

    let error = kernel
        .set_float64(std::slice::from_ref(&input), &[-1.0])
        .expect_err("a negative delay time is refused");
    assert!(
        !error.to_string().is_empty(),
        "the refusal reports why the write failed"
    );
    let mut value = [f64::NAN];
    kernel
        .get_float64(std::slice::from_ref(&input), &mut value)
        .unwrap();
    assert_eq!(value, [0.2], "the rejected write is rolled back");

    kernel
        .set_float64(std::slice::from_ref(&input), &[0.4])
        .expect("an admissible delay time is accepted");
    kernel
        .get_float64(std::slice::from_ref(&input), &mut value)
        .unwrap();
    assert_eq!(value, [0.4]);
}
