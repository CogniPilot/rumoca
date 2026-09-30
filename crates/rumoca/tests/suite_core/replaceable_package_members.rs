//! Members of a replaceable package reached through inheritance (MLS 3.7 §7.3).
//!
//! `Modelica.Fluid` declares `replaceable package Medium` in a partial base
//! and types its components `Medium.MassFlowRate` with attribute modifiers
//! (`each min = ...`). A model that extends the base must resolve
//! `Medium.MassFlowRate` to the predefined type it aliases, so the modifiers
//! are the predefined attributes of `Real` rather than unknown names.

use rumoca::Compiler;

const MODELS: &str = r#"
package PM
  type MassFlowRate = Real(quantity = "MassFlowRate", min = -1e5, max = 1e5);
end PM;

partial connector FluidPort
  replaceable package Medium = PM;
  Medium.MassFlowRate m_flow;
  flow Medium.MassFlowRate m_flow_flow;
end FluidPort;

connector FluidPort_a
  extends FluidPort;
end FluidPort_a;

partial model PipeBase
  replaceable package Medium = PM;
  FluidPort_a port_a(redeclare package Medium = Medium);
end PipeBase;

model Sink
  parameter Real x = 0;
end Sink;

model PortReader
  extends PipeBase;
  Sink sink(x = port_a.m_flow);
equation
  port_a.m_flow = 1.0;
end PortReader;

partial model Base
  replaceable package Medium = PM;
  Medium.MassFlowRate[2] m_flows(each min = 0, each start = 1.0);
equation
  m_flows = {1.0, 2.0};
end Base;

model Extending
  extends Base;
end Extending;
"#;

fn compile(model: &str) -> Result<(), String> {
    match Compiler::new()
        .model(model)
        .compile_str(MODELS, "Members.mo")
    {
        Ok(_) => Ok(()),
        Err(error) => Err(format!("{error:?}")),
    }
}

#[test]
fn an_inherited_replaceable_package_type_accepts_predefined_attributes() {
    if let Err(error) = compile("Extending") {
        panic!("Extending compiles: {error}");
    }
}

#[test]
fn a_directly_declared_component_of_the_same_type_compiles() {
    if let Err(error) = compile("Base") {
        panic!("Base compiles: {error}");
    }
}

#[test]
fn members_typed_through_a_package_alias_are_members_of_an_inherited_connector() {
    if let Err(error) = compile("PortReader") {
        panic!("PortReader compiles: {error}");
    }
}
