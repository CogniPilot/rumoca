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

const INHERITED_MEDIUM: &str = r#"
partial package PM
  replaceable partial model BP
    Real p;
  end BP;
end PM;

package Conc
  extends PM;
  redeclare model extends BP
  equation
    p = 1;
  end BP;
end Conc;

model Comp
  replaceable package Medium = PM;
  Medium.BP bp;
end Comp;

partial model Base
  replaceable package Medium = Conc;
  Comp c(redeclare package Medium = Medium);
end Base;

model Ext
  extends Base;
end Ext;
"#;

#[test]
fn an_inherited_replaceable_package_can_be_forwarded_to_a_component() {
    match Compiler::new()
        .model("Ext")
        .compile_str(INHERITED_MEDIUM, "Inherited.mo")
    {
        Ok(_) => {}
        Err(error) => panic!("Ext compiles: {error:?}"),
    }
}

const TWO_BASES_DECLARE_MEDIUM: &str = r#"
partial package PM
  replaceable partial model BP
    Real p;
  end BP;
end PM;

package Conc
  extends PM;
  redeclare model extends BP
  equation
    p = 1;
  end BP;
end Conc;

partial model B1
  replaceable package Medium = PM;
end B1;

partial model B2
  replaceable package Medium = PM;
  Medium.BP[2] bps;
end B2;

model Both
  extends B1;
  extends B2;
end Both;

model Direct
  Both c(redeclare package Medium = Conc);
end Direct;

model Forwarded
  replaceable package Medium = Conc;
  Both c(redeclare package Medium = Medium);
end Forwarded;
"#;

#[test]
fn a_redeclare_reaches_every_inherited_declaration_of_the_package() {
    for model in ["Direct", "Forwarded"] {
        let result = Compiler::new()
            .model(model)
            .compile_str(TWO_BASES_DECLARE_MEDIUM, "TwoBases.mo");
        if let Err(error) = result {
            panic!("{model} compiles: {error:?}");
        }
    }
}

/// A modifier written in `Vol` names `medium.state`, whose record type is the
/// redeclared `Simple.TS`. It must be typed by the enclosing instance, not by
/// the nominal `PM.TS`, and `medium.state.p` must be a member of it.
const LATE_BOUND_RECORD_MEMBER: &str = r#"
partial package PM
  replaceable partial record TS end TS;
  replaceable partial model BP
    TS state;
  end BP;
end PM;

package Simple
  extends PM;
  redeclare record extends TS
    Real p;
  end TS;
  redeclare model extends BP
    Real x;
  equation
    state.p = x;
  end BP;
end Simple;

model HT
  replaceable package Medium = PM;
  parameter Integer n = 1;
  input Medium.TS states[n];
  Real q = states[1].p;
end HT;

model Vol
  replaceable package Medium = PM;
  Medium.BP medium;
  HT ht(redeclare package Medium = Medium, final n = 1, final states = {medium.state});
equation
  medium.x = 2;
end Vol;

model Top
  Vol v(redeclare package Medium = Simple);
end Top;
"#;

#[test]
fn a_modifier_reads_record_members_typed_by_the_enclosing_instances_package() {
    let result = Compiler::new()
        .model("Top")
        .compile_str(LATE_BOUND_RECORD_MEMBER, "LateBound.mo");
    if let Err(error) = result {
        panic!("Top compiles: {error:?}");
    }
}

const DEFERRED_REFERENCE_INTO_REDECLARED_RECORD: &str = r#"
partial package PM
  replaceable partial record TS end TS;
  replaceable partial model BP
    TS state;
  end BP;
end PM;

package Simple
  extends PM;
  redeclare record extends TS
    Real p;
  end TS;
  redeclare model extends BP
    Real x;
  equation
    state.p = x;
  end BP;
end Simple;

model Vol
  replaceable package Medium = PM;
  Medium.BP medium;
  Real y = medium.state.p;
equation
  medium.x = 2;
end Vol;

model Top
  Vol v(redeclare package Medium = Simple);
end Top;
"#;

#[test]
fn a_reference_reaches_members_of_a_record_redeclared_by_the_selected_package() {
    let result = Compiler::new()
        .model("Top")
        .compile_str(DEFERRED_REFERENCE_INTO_REDECLARED_RECORD, "Deferred.mo");
    if let Err(error) = result {
        panic!("Top compiles: {error:?}");
    }
}
