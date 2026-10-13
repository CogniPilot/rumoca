//! Current FMI capsule replays one correlated retained source through its sole
//! existing checked component reader. No instance-time execution is serialized.
use super::*;
use rumoca_ir_solve::{SolveDeclarationStart, SolveExactInitialRun};
use serde::de::IntoDeserializer;

fn replay(
    wire: serde_json::Value,
) -> Result<rumoca_ir_solve::fmi::FmiComponent, serde_json::Error> {
    rumoca_sim::deserialize_fmi_component(wire.into_deserializer())
}

fn assert_original_inputs_replay(
    component: &rumoca_ir_solve::fmi::FmiComponent,
) -> serde_json::Value {
    let root = component.runtime_view().retain_assertion_root();
    assert_eq!(root.resolved_start_time(), None);
    assert!(std::ptr::eq(root.metadata(), component.metadata()));
    let clone = root.clone();
    assert!(clone.shares_root_owner(&root));
    assert!(std::ptr::eq(clone.metadata(), root.metadata()));
    let original = serde_json::to_value(root.fmi_declaration_inputs().collect::<Vec<_>>()).unwrap();
    let wire =
        serde_json::to_value(rumoca_sim::retained_fmi_component_wire(&root).unwrap()).unwrap();
    assert_eq!(wire["schema_version"], 7);
    assert_eq!(wire["variables"], original);
    let checked = replay(wire.clone()).unwrap();
    let reread = checked.runtime_view().retain_assertion_root();
    assert_eq!(reread.resolved_start_time(), None);
    assert_eq!(
        serde_json::to_value(reread.fmi_declaration_inputs().collect::<Vec<_>>()).unwrap(),
        original
    );
    assert_eq!(checked.variables().len(), component.variables().len());
    for (before, after) in component.variables().iter().zip(checked.variables()) {
        assert_eq!(before.name(), after.name());
        assert_eq!(before.causality(), after.causality());
        assert_eq!(before.variability(), after.variability());
        assert_eq!(before.initial(), after.initial());
        assert_eq!(before.storage(), after.storage());
        assert_eq!(before.fmi3_type(), after.fmi3_type());
        assert_eq!(
            before.fmi2_integer_admitted(),
            after.fmi2_integer_admitted()
        );
    }
    wire
}

#[test]
fn retained_fmi_capsule_replays_exact_source_runs_and_refuses_tampering() {
    let _lock = session_test_guard();
    let mut lengths = Vec::new();
    for count in [8, 65_536] {
        let source = format!(
            r#"
model Capsule
  input Integer repeated[{count}](start=fill(9223372036854775807,{count})) "exact source broadcast";
  input Integer literal[2](start={{9007199254740993,9223372036854775807}});
  input Boolean disabled[{count}](start=fill(false,{count}));
  input Integer missing[{count}];
  input Real zero[{count}](each start=-0.0, each min=-1.0, each max=1.0, each nominal=0.5, each unit="m");
  parameter Real gain=2.0 "independent tunable value";
  output Integer result;
equation
  result=repeated[1];
end Capsule;
"#
        );
        with_source_component(&source, "Capsule", |component| {
            let root = component.runtime_view().retain_assertion_root();
            let inputs = root.fmi_declaration_inputs().collect::<Vec<_>>();
            let lookup = |name| inputs.iter().find(|input| input.name == name).unwrap();
            let SolveDeclarationStart::Integer(values) = &lookup("repeated").start else { panic!("Integer") };
            assert_eq!(values.len(), count);
            assert!(matches!(values.runs().next(), Some(SolveExactInitialRun::Repeat { value: i64::MAX, count: extent, .. }) if *extent == count));
            assert_eq!(values.runs().count(), 1);
            let original_values = root.metadata().variables().iter().find(|variable| variable.name() == "repeated").unwrap();
            let SolveDeclarationStart::Integer(original_values) = original_values.declaration_start() else { panic!("Integer") };
            assert!(std::ptr::eq(values.value(0).unwrap(), original_values.value(0).unwrap()), "projected source run shares the exact original allocation");
            let SolveDeclarationStart::Integer(literal) = &lookup("literal").start else { panic!("Integer") };
            assert!(matches!(literal.runs().next(), Some(SolveExactInitialRun::Literal { values, .. }) if values == &[9_007_199_254_740_993,i64::MAX]));
            let SolveDeclarationStart::Boolean(values) = &lookup("disabled").start else { panic!("Boolean") };
            assert!(matches!(values.runs().next(), Some(SolveExactInitialRun::Repeat { value: false, count: extent, .. }) if *extent == count));
            assert!(matches!(lookup("missing").start, SolveDeclarationStart::Missing { count: extent, .. } if extent == count));
            let SolveDeclarationStart::Real(values) = &lookup("zero").start else { panic!("Real") };
            assert_eq!(values.value(count-1).unwrap().to_bits(), (-0.0f64).to_bits());
            assert_eq!(values.run_count(), 1);
            assert!(!values.has_dense_view());
            let wire = assert_original_inputs_replay(&component);
            lengths.push(serde_json::to_vec(&wire["variables"]).unwrap().len());
            let index = wire["variables"].as_array().unwrap().iter().position(|variable| variable["name"] == "repeated").unwrap();
            let mut wrong_wire = wire.clone();
            wrong_wire["schema_version"] = 5.into();
            assert!(replay(wrong_wire).is_err());
            let mut wrong_solve_wire = wire.clone();
            wrong_solve_wire["solve_model"]["schema_version"] = 0.into();
            assert!(replay(wrong_solve_wire).is_err());
            let mut wrong_type = wire.clone();
            wrong_type["variables"][index]["start"] = serde_json::to_value(&lookup("disabled").start).unwrap();
            assert!(replay(wrong_type).err().unwrap().to_string().contains("does not match its Solve declaration or storage role"));
            let mut wrong_extent = wire.clone();
            wrong_extent["variables"][index]["dimensions"] = serde_json::json!([count+1]);
            let extent_error = replay(wrong_extent).err().unwrap().to_string();
            assert_eq!(extent_error, format!("FMI variable `repeated` has {count} scalars but its shape requires {}", count + 1));
            let mut wrong_run = wire;
            wrong_run["variables"][index]["start"]["values"]["count"] = (count+1).into();
            assert!(replay(wrong_run).is_err());
            Ok(String::new())
        }).unwrap();
    }
    assert!(
        lengths[1] <= lengths[0] + 256,
        "compact declaration wire expanded with tensor extent: {lengths:?}"
    );
}

#[test]
fn retained_fmi_capsule_replays_original_function_record_starts() {
    let _lock = session_test_guard();
    for (source, model) in [
        (typed_inputs::SOURCE, "Step"),
        (source_functions::SLAM_DEFAULT_POLICY, "DefaultPolicyProbe"),
    ] {
        with_source_component(source, model, |component| {
            let original = component.runtime_view().retain_assertion_root();
            assert!(
                original
                    .metadata()
                    .variables()
                    .iter()
                    .any(|variable| matches!(
                        variable.declaration_start(),
                        SolveDeclarationStart::Integer(_) | SolveDeclarationStart::Boolean(_)
                    ))
            );
            assert_original_inputs_replay(&component);
            Ok(String::new())
        })
        .unwrap();
    }
}
