//! The parameter bindings of a Solve target without an FMI component are the
//! FMI C profile's (SPEC_0044 ME-PROJ-001): same programs, same order, same
//! lifecycle rows, on models lowered from source.

use rumoca::Compiler;
use rumoca_ir_solve::fmi::SolveBindingExecution;

/// A dependent parameter beside an MLS §8.5 numeric event memory (a Real
/// `mod`) whose seed is a binding program that runs only while initializing.
const QUOTIENT_SEED: &str = "model QuotientSeed
  parameter Real gain = 2.5;
  parameter Real scaled = 3*gain;
  Real x(start=0, fixed=true);
  Real y;
equation
  der(x) = scaled;
  y = mod(x + gain, 7);
end QuotientSeed;";

const CHAIN: &str = "model BindingChain
  parameter Real a = 2;
  parameter Real b = 3*a;
  parameter Real c = b + a;
  Real x(start=c, fixed=true);
equation
  der(x) = -b*x;
end BindingChain;";

fn executions(name: &str, source: &str) -> (serde_json::Value, serde_json::Value, usize, usize) {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(source, &format!("{name}.mo"))
        .expect("compile the fixture");
    let component =
        rumoca_sim::lower_fmi_c_export_component(&compiled.dae).expect("lower the FMI C component");
    let lanes = SolveBindingExecution::checked(component.runtime_view().model())
        .expect("the model the C profile admits is admitted without its component");
    let component = component
        .into_codegen_view()
        .try_c()
        .expect("admit the C profile");
    (
        serde_json::to_value(lanes.programs()).expect("programs serialize"),
        serde_json::to_value(component.parameter_binding_execution()).expect("programs serialize"),
        lanes.levels(),
        component.parameter_binding_levels(),
    )
}

#[test]
fn lowered_models_bind_as_their_fmi_c_component() {
    for (name, source) in [("BindingChain", CHAIN), ("QuotientSeed", QUOTIENT_SEED)] {
        let (lanes, component, lane_levels, component_levels) = executions(name, source);
        assert_eq!(lanes, component, "{name}");
        assert_eq!(lane_levels, component_levels, "{name}");
    }
}

#[test]
fn a_quotient_seed_runs_only_while_initializing() {
    let (lanes, ..) = executions("QuotientSeed", QUOTIENT_SEED);
    let seeds: Vec<_> = lanes
        .as_array()
        .expect("programs are a sequence")
        .iter()
        .filter(|program| program["quotient_seed_rows"] != serde_json::json!([]))
        .collect();
    assert!(
        !seeds.is_empty(),
        "the fixture has a quotient seed: {lanes}"
    );
    for seed in seeds {
        assert_eq!(seed["initialization_only"], serde_json::json!(true));
        assert_eq!(seed["parameter_rows"], serde_json::json!([]));
    }
}
