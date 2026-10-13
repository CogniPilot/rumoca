use super::*;

#[test]
fn host_driven_preparation_preserves_source_tensor_start_owner() {
    let dae = compile(
        "model HostTensor input Real u[2,60](each start=-0.0); Real x(start=0, fixed=true); equation der(x)=sum(u); end HostTensor;",
        "HostTensor",
    );
    for prepare in [
        super::super::entry::lower_dae_for_gpu_preparation,
        super::super::entry::lower_dae_for_native_preparation,
    ] {
        let model = prepare(&dae, &SimOptions::default()).unwrap();
        assert_tensor_start(&model.parameters);
    }
    #[cfg(feature = "fmi")]
    {
        let component = super::super::fmi::lower_fmi_component(&dae).unwrap();
        assert_tensor_start(&component.runtime_view().model().parameters);
    }
    assert!(lower_dae_for_simulation(&dae, &SimOptions::default()).is_err());
}

fn assert_tensor_start(owner: &rumoca_ir_solve::SolveInitialValues) {
    assert_eq!(owner.len(), 120);
    assert!(!owner.has_dense_view());
    assert_eq!(owner.run_count(), 1);
    let run = owner.runs().next().unwrap();
    assert_eq!(run.count(), 120);
    assert_eq!(run.repeated_bits(), Some((-0.0f64).to_bits()));
}

#[test]
fn host_driven_preparation_applies_parameter_and_partial_input_overrides_to_actual_starts() {
    let dae = compile(
        "model HostOverrides parameter Real offset=.5; input Real u[2,3](each start=offset); Real x(start=0,fixed=true); equation der(x)=sum(u); end HostOverrides;",
        "HostOverrides",
    );
    let opts = SimOptions {
        param_overrides: vec![("offset".into(), 0.75)],
        initial_inputs: vec![("u[1,2]".into(), 1.25)],
        start_overrides: vec![("x".into(), 9.0)],
        ..SimOptions::default()
    };
    for prepare in [
        super::super::entry::lower_dae_for_gpu_preparation,
        super::super::entry::lower_dae_for_native_preparation,
    ] {
        let model = prepare(&dae, &opts).unwrap();
        assert!(!model.parameters.has_dense_view());
        assert_slot_value(&model, "offset", 0.75);
        assert_slot_value(&model, "u[1,1]", 0.75);
        assert_slot_value(&model, "u[1,2]", 1.25);
        assert_slot_value(&model, "u[2,3]", 0.75);
        assert_eq!(model.initial_y.value(0), Some(9.0));
    }
}

#[test]
fn host_driven_preparation_keeps_binding_over_start_and_checks_actual_overrides() {
    let dae = compile(
        "model HostBound input Real u[2,3](each start=9)=fill(.5,2,3); Real x(start=0,fixed=true); equation der(x)=sum(u); end HostBound;",
        "HostBound",
    );
    let opts = SimOptions {
        initial_inputs: vec![("u[1,2]".into(), 1.25)],
        ..SimOptions::default()
    };
    let model = super::super::entry::lower_dae_for_native_preparation(&dae, &opts).unwrap();
    assert_slot_value(&model, "u[1,1]", 0.5);
    assert_slot_value(&model, "u[1,2]", 1.25);
    assert_slot_value(&model, "u[2,3]", 0.5);
    assert!(lower_dae_for_simulation(&dae, &opts).is_ok());
    for value in [f64::INFINITY, f64::NAN] {
        let opts = SimOptions {
            initial_inputs: vec![("u[1,2]".into(), value)],
            ..SimOptions::default()
        };
        assert!(super::super::entry::lower_dae_for_native_preparation(&dae, &opts).is_err());
    }
}

#[test]
fn host_driven_preparation_propagates_policy_into_formal_state_trial_points() {
    let dae = compile(
        "model HostCircle input Real speed(start=.25); Real x(start=1); Real y(start=0); Real lambda(start=0); equation x*x+y*y=1; der(x)=-speed*y+lambda*x; der(y)=speed*x+lambda*y; end HostCircle;",
        "HostCircle",
    );
    let formal = rumoca_phase_structural::construct_formal_derivatives(&dae).unwrap();
    assert!(formal.inspect(|view| view.formal_dimension()) < 2);
    let model = super::super::entry::lower_dae_for_native_preparation(&dae, &SimOptions::default())
        .unwrap();
    assert_slot_value(&model, "speed", 0.25);
    assert!(lower_dae_for_simulation(&dae, &SimOptions::default()).is_err());
}

#[cfg(feature = "fmi")]
#[test]
fn host_string_input_binding_precedes_start_without_changing_parameter_text_starts() {
    let dae = compile(
        "model HostText input String label[2](each start=\"input-start\")=fill(\"input-binding\",2); parameter String tag(start=\"parameter-start\")=\"parameter-binding\"; Real x(start=0,fixed=true); equation der(x)=0; end HostText;",
        "HostText",
    );
    let component = super::super::fmi::lower_fmi_component(&dae).unwrap();
    for (name, expected) in [
        ("label", vec!["input-binding".to_string(); 2]),
        ("tag", vec!["parameter-start".to_string()]),
    ] {
        let variable = component
            .variables()
            .iter()
            .find(|variable| variable.name() == name)
            .unwrap();
        assert_eq!(variable.text_start(), Some(expected.as_slice()));
    }
}

fn assert_slot_value(model: &rumoca_ir_solve::SolveModel, name: &str, expected: f64) {
    let Some(rumoca_ir_solve::ScalarSlot::P { index, .. }) = model.problem.layout.binding(name)
    else {
        panic!("input/parameter must retain P storage")
    };
    assert_eq!(
        model.parameters.value(index).unwrap().to_bits(),
        expected.to_bits()
    );
}
