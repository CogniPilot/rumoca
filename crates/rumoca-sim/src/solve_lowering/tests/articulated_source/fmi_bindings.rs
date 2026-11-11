//! Semantic channel correspondence comes from the real constructor, not names.
use super::super::compile;
use rumoca_ir_solve::fmi::{FmiArticulatedReduction, FmiComponent};
use serde::de::IntoDeserializer;

fn source() -> String {
    let function = super::SOURCE.split("model Case").next().unwrap();
    format!(
        r#"{function}
model Case
 parameter Real inertia[6,6]=identity(6);
 parameter Real subspace[6]={{1,0,0,0,0,0}};
 parameter Real bias[6]=zeros(6), convective[6]=zeros(6);
 Real signal(start=0.25,fixed=true);
 output Real U[6]; output Real D; output Real u; output Real Ia[6,6]; output Real pa[6];
equation
 der(signal)=-signal;
 (U,D,u,Ia,pa)=K.articulatedJointReduction(inertia,subspace,1,0,signal,{{-bias[1],-bias[2],-bias[3],-bias[4],-bias[5],-bias[6]}},convective);
end Case;
"#
    )
}

fn component(source: &str) -> FmiComponent {
    let dae = compile(source, "Case");
    let lowered =
        crate::lower_correlated_for_simulation_with_overrides(&dae, &Default::default()).unwrap();
    crate::finish_fmi_component(lowered).unwrap()
}

fn check_channels(component: &FmiComponent, factor: &FmiArticulatedReduction) {
    assert_eq!(
        factor.inputs().iter().map(|v| v.len()).collect::<Vec<_>>(),
        [36, 6, 1, 1, 1, 6, 6]
    );
    assert_eq!(factor.outputs().len(), 50);
    assert_eq!(factor.inputs()[3][0].literal().unwrap().to_bits(), 0);
    for channel in factor.inputs().iter().flatten().chain(factor.outputs()) {
        if let Some((reference, element, _)) = channel.variable() {
            let variable = component
                .variables()
                .iter()
                .find(|v| v.value_reference_fmi3() == reference)
                .unwrap();
            assert_eq!(variable.fmi3_type(), "Float64");
            assert!(element < variable.scalar_count());
        } else {
            assert!(channel.literal().unwrap().is_finite());
        }
    }
    assert!(component.problem().mechanical_step.is_none());
}

#[test]
fn articulated_fmi_binding_joins_complete_inputs_outputs_and_negative_flow() {
    let component = component(&source());
    let factors = component.metadata().articulated_reductions();
    assert_eq!(factors.len(), 1);
    let factor = &factors[0];
    check_channels(&component, factor);
    let bias = component
        .variables()
        .iter()
        .find(|v| v.name() == "bias")
        .unwrap()
        .value_reference_fmi3();
    for (element, channel) in factor.inputs()[5].iter().enumerate() {
        assert_eq!(channel.variable(), Some((bias, element, true)));
    }
    let output = component
        .variables()
        .iter()
        .find(|v| v.name() == "U")
        .unwrap()
        .value_reference_fmi3();
    for (element, channel) in factor.outputs()[..6].iter().enumerate() {
        assert_eq!(channel.variable(), Some((output, element, false)));
    }
}

#[test]
fn articulated_fmi_binding_survives_renaming_and_storage_changes() {
    let source = source()
        .replace("articulatedJointReduction", "ordinaryFunction")
        .replace("inertia", "massLike")
        .replace("bias", "incoming")
        .replace(
            " Real signal(start=0.25,fixed=true);",
            " Real preceding=17; Real signal(start=0.25,fixed=true);",
        );
    let component = component(&source);
    let factors = component.metadata().articulated_reductions();
    assert_eq!(factors.len(), 1);
    check_channels(&component, &factors[0]);
}

#[test]
fn articulated_fmi_binding_declines_changed_formula_increment_and_unsupported_input() {
    for changed in [
        source().replace("+factorIncrement", "-factorIncrement"),
        source().replace("subspace,1,0,signal", "subspace,1,0.005,signal"),
        source().replace("subspace,1,0,signal", "subspace,1,-0.0,signal"),
        source().replace("subspace,1,0,signal", "subspace,1,0,2*signal"),
    ] {
        let component = component(&changed);
        assert!(component.metadata().articulated_reductions().is_empty());
    }
}

#[test]
fn articulated_fmi_binding_replay_rederives_without_a_wire_claim() {
    let original = component(&source());
    let root = original.runtime_view().retain_assertion_root();
    let wire = serde_json::to_value(
        rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root).unwrap(),
    )
    .unwrap();
    assert!(wire.get("articulated_reductions").is_none());
    let replay =
        rumoca_eval_solve::fmi_wire::deserialize_fmi_component(wire.into_deserializer()).unwrap();
    assert_eq!(replay.metadata().articulated_reductions().len(), 1);
    check_channels(&replay, &replay.metadata().articulated_reductions()[0]);
}

#[test]
fn articulated_fmi_binding_keeps_two_actual_call_contexts() {
    let source=source().replace("equation\n der(signal)",
        "output Real U2[6]; output Real D2; output Real u2; output Real Ia2[6,6]; output Real pa2[6];\nequation\n der(signal)")
        .replace("end Case;", "(U2,D2,u2,Ia2,pa2)=K.articulatedJointReduction(inertia,subspace,1,0,signal,bias,convective); end Case;");
    let component = component(&source);
    let factors = component.metadata().articulated_reductions();
    assert_eq!(factors.len(), 2);
    for factor in factors {
        check_channels(&component, factor);
    }
    let a = factors[0]
        .outputs()
        .iter()
        .map(|v| v.variable())
        .collect::<Vec<_>>();
    let b = factors[1]
        .outputs()
        .iter()
        .map(|v| v.variable())
        .collect::<Vec<_>>();
    assert!(a.iter().all(|v| !b.contains(v)));
}

#[test]
fn articulated_fmi_binding_joins_proofs_split_between_refresh_schedules() {
    let source = source().replace("der(signal)=-signal;", "der(signal)=u;");
    let component = component(&source);
    let factors = component.metadata().articulated_reductions();
    assert_eq!(factors.len(), 1);
    check_channels(&component, &factors[0]);
}
