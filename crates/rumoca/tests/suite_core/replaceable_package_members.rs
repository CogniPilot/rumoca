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

/// `Medium.f` names a function of a replaceable package. The instance whose
/// `Medium` is redeclared as `Simple` calls the redeclared `Simple.f`.
const FUNCTION_OF_REDECLARED_PACKAGE: &str = r#"
partial package PM
  replaceable partial function f
    input Real x;
    output Real y;
  end f;
end PM;

package Simple
  extends PM;
  redeclare function f
    input Real x;
    output Real y;
  algorithm
    y := x + 1;
  end f;
end Simple;

model HT
  replaceable package Medium = PM;
  Real y = Medium.f(2.0);
end HT;

model Top
  HT ht(redeclare package Medium = Simple);
end Top;
"#;

#[test]
fn a_call_through_a_package_alias_selects_the_redeclared_packages_function() {
    let result = Compiler::new()
        .model("Top")
        .compile_str(FUNCTION_OF_REDECLARED_PACKAGE, "Function.mo");
    if let Err(error) = result {
        panic!("Top compiles: {error:?}");
    }
}

/// A function calls a short-class alias of another package's function, both
/// as a package member and as a local declaration of the calling function.
const FUNCTION_ALIAS_CALLS: &str = r#"
package Lib
  function base
    input Real x;
    output Real y;
  algorithm
    y := x + 1;
  end base;
end Lib;

package Pkg
  function member = Lib.base;
  function viaMember
    input Real x;
    output Real y;
  algorithm
    y := member(x) * 2;
  end viaMember;
  function viaLocal
    input Real x;
    output Real y;
  protected
    function local1 = Lib.base;
  algorithm
    y := local1(x) * 3;
  end viaLocal;
end Pkg;

model Top
  Real a = Pkg.viaMember(time);
  Real b = Pkg.viaLocal(time);
end Top;
"#;

#[test]
fn a_function_calls_a_short_class_alias_of_another_packages_function() {
    let result = Compiler::new()
        .model("Top")
        .compile_str(FUNCTION_ALIAS_CALLS, "FunctionAlias.mo");
    if let Err(error) = result {
        panic!("Top compiles: {error:?}");
    }
}

/// A whole record array is passed where the function takes one record, so the
/// call maps over the elements (MLS 12.4.6).
const RECORD_ARRAY_CALL: &str = r#"
record R
  Real p;
  Real q;
end R;

function f
  input R s;
  output Real T;
algorithm
  T := s.p + s.q;
end f;

model Top
  R states[2](each p = 2, each q = 3);
  Real Ts[2];
equation
  Ts = f(states);
end Top;
"#;

#[test]
fn a_record_array_argument_maps_the_function_over_its_elements() {
    let result = Compiler::new()
        .model("Top")
        .compile_str(RECORD_ARRAY_CALL, "RecordArrayCall.mo");
    if let Err(error) = result {
        panic!("Top compiles: {error:?}");
    }
}

/// `inStream(ports[i].h_outflow)` on an array of connectors joined to a scalar
/// connector reads the scalar peer's stream, not an element of it.
const INSTREAM_ARRAY_PORT_TO_SCALAR_PORT: &str = r#"
connector Port
  Real p;
  flow Real m_flow;
  stream Real h_outflow;
end Port;

model Vol
  Port ports[1];
  Real hin[1];
equation
  for i in 1:1 loop
    hin[i] = inStream(ports[i].h_outflow);
    ports[i].h_outflow = 1.0;
    ports[i].m_flow = 0.5 * (ports[i].p - 1);
  end for;
end Vol;

model Pipe
  Port port_a;
  Port port_b;
equation
  port_a.m_flow + port_b.m_flow = 0;
  port_a.p = port_b.p;
  port_a.h_outflow = inStream(port_b.h_outflow);
  port_b.h_outflow = inStream(port_a.h_outflow);
end Pipe;

model Top
  Vol v1;
  Vol v2;
  Pipe pipe1;
equation
  connect(v1.ports[1], pipe1.port_a);
  connect(pipe1.port_b, v2.ports[1]);
end Top;
"#;

#[test]
fn instream_of_an_array_port_reads_the_scalar_peer_stream() {
    let result = Compiler::new()
        .model("Top")
        .compile_str(INSTREAM_ARRAY_PORT_TO_SCALAR_PORT, "InStream.mo");
    if let Err(error) = result {
        panic!("Top compiles: {error:?}");
    }
}

/// A for-equation index inside a call through a redeclared package is the
/// loop's own binder, never a member of the instance's package.
const LOOP_INDEX_IN_PACKAGE_CALL: &str = r#"
partial package PSM
  constant Real cp_const;
  constant Real T0 = 273.15;
  record State
    Real p;
    Real T;
  end State;
  replaceable function setState
    input Real p;
    input Real h;
    output State s;
  algorithm
    s := State(p = p, T = T0 + h/cp_const);
  end setState;
  replaceable function density
    input State s;
    output Real d;
  algorithm
    d := s.p/(287*s.T);
  end density;
end PSM;

package W
  extends PSM(cp_const = 4184);
end W;

model M
  replaceable package Medium = PSM;
  parameter Integer n = 2;
  Real ps[n] = {1e5 + time, 2e5};
  Real hs[n] = {1e4, 2e4 + time};
  Real d[n];
equation
  for i in 1:n loop
    d[i] = Medium.density(Medium.setState(ps[i], hs[i]));
  end for;
end M;

model Top
  M m(redeclare package Medium = W);
end Top;
"#;

#[test]
fn a_loop_index_in_a_package_call_stays_the_loop_binder() {
    let compiled = Compiler::new()
        .model("Top")
        .compile_str(LOOP_INDEX_IN_PACKAGE_CALL, "LoopIndex.mo")
        .unwrap_or_else(|error| panic!("Top compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            ..rumoca_sim::SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Top simulates: {error}"));
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        *result.data[index].last().expect("samples")
    };
    // d = p / (287 (T0 + h / cp)) at t = 1.
    let expected_1 = 100_001.0 / (287.0 * (273.15 + 10_000.0 / 4184.0));
    let expected_2 = 200_000.0 / (287.0 * (273.15 + 20_001.0 / 4184.0));
    assert!((column("m.d[1]") - expected_1).abs() < 1e-9);
    assert!((column("m.d[2]") - expected_2).abs() < 1e-9);
}

/// A package that modifies an inherited constant (`extends PSM(T0 = 273.15)`,
/// as `ConstantPropertyLiquidWater` sets `T0`) and is selected by a redeclare:
/// the nested model reached through the package slot, a function it calls
/// unqualified, and a call through the slot all see the modified value.
const MODIFIED_PACKAGE_CONSTANT: &str = r#"
package PM
  constant Real reference_T = 298.15;
end PM;

partial package PSM
  extends PM;
  constant Real T0 = reference_T;
  function hf
    input Real T;
    output Real h;
  algorithm
    h := 2*(T - T0);
  end hf;
  model BP
    Real T = 300;
    Real h;
    Real h2;
  equation
    h = 2*(T - T0);
    h2 = hf(T);
  end BP;
end PSM;

package M
  extends PSM(T0 = 273.15);
end M;

model Vessel
  replaceable package Medium = PSM;
  Medium.BP medium;
  Real h3 = Medium.hf(300);
end Vessel;

model Top
  Vessel v(redeclare package Medium = M);
end Top;
"#;

#[test]
fn a_selected_package_modifies_the_constants_its_nested_models_and_functions_read() {
    let compiled = Compiler::new()
        .model("Top")
        .compile_str(MODIFIED_PACKAGE_CONSTANT, "Modified.mo")
        .unwrap_or_else(|error| panic!("Top compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .expect("Top simulates");
    for name in ["v.medium.h", "v.medium.h2", "v.h3"] {
        let column = result.names.iter().position(|n| n == name).unwrap();
        for value in &result.data[column] {
            assert!((value - 53.7).abs() < 1e-9, "{name} = {value}");
        }
    }
}

/// A package constant whose binding calls a function (`h_default =
/// f(p_default)`, as `PartialMedium.h_default` calls `specificEnthalpy_pTX`)
/// is inherited by the selected package, so the call names that package's `f`,
/// whose body calls the `g` the package redeclares (MLS §7.1).
const INHERITED_CONSTANT_CALL: &str = r#"
partial package PM
  constant Real p_default = 1;
  constant Real h_default = f(p_default);
  replaceable partial function g
    input Real p;
    output Real y;
  end g;
  replaceable function f
    input Real p;
    output Real h;
  algorithm
    h := g(p);
  end f;
end PM;

package M
  extends PM(p_default = 2);
  redeclare function extends g
  algorithm
    y := 3*p;
  end g;
end M;

model Sensor
  replaceable package Medium = PM;
  Real h;
equation
  h = Medium.h_default;
end Sensor;

model Top
  Sensor s(redeclare package Medium = M);
end Top;

model Direct
  Real h = M.h_default;
end Direct;
"#;

#[test]
fn an_inherited_constant_calls_the_selected_packages_functions() {
    for (model, name) in [("Direct", "h"), ("Top", "s.h")] {
        let compiled = Compiler::new()
            .model(model)
            .compile_str(INHERITED_CONSTANT_CALL, "InheritedCall.mo")
            .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
        let result = rumoca_sim::simulate_dae_with_diagnostics(
            &compiled.dae,
            &rumoca_sim::SimOptions {
                t_end: 0.1,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{model} simulates: {error}"));
        let column = result.names.iter().position(|n| n == name).unwrap();
        for value in &result.data[column] {
            assert!((value - 6.0).abs() < 1e-12, "{model}: {name} = {value}");
        }
    }
}

/// A model that declares its medium package as a local alias
/// (`replaceable package Medium = ConstantPropertyLiquidWater`) and types a
/// component `Medium.BaseProperties`: the nested model's unqualified call and
/// its constants, one without a declaration default, are the selected
/// package's (MLS §4.5.1, §7.1).
const ROOT_PACKAGE_ALIAS: &str = r#"
partial package PM
  constant Real cp_const;
  constant Real T0 = 298.15;
  function hf
    input Real T;
    output Real h;
  algorithm
    h := cp_const*(T - T0);
  end hf;
  model BP
    Real T = 300;
    Real h;
    Real u;
  equation
    h = hf(T);
    u = cp_const*(T - T0);
  end BP;
end PM;

package M
  extends PM(cp_const = 2, T0 = 273.15);
end M;

model Root
  replaceable package Medium = M;
  Medium.BP medium;
end Root;
"#;

#[test]
fn a_local_package_alias_selects_the_package_of_its_nested_models() {
    let compiled = Compiler::new()
        .model("Root")
        .compile_str(ROOT_PACKAGE_ALIAS, "RootAlias.mo")
        .unwrap_or_else(|error| panic!("Root compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .expect("Root simulates");
    for name in ["medium.h", "medium.u"] {
        let column = result.names.iter().position(|n| n == name).unwrap();
        for value in &result.data[column] {
            assert!((value - 53.7).abs() < 1e-9, "{name} = {value}");
        }
    }
}

/// A package redeclares an inherited record in its `extends` modification
/// (`extends PM(redeclare record State = SR)`, as `Media.Incompressible.TableBased`
/// does for `ThermodynamicState`). The record constructor called by the
/// package's own function, and by its `function extends` of an inherited
/// function, is the redeclared `SR` (MLS §7.3), also through a further
/// extending package selected as a replaceable package.
const EXTENDS_MODIFIER_RECORD_REDECLARE: &str = r#"
record R0
end R0;

partial package PM
  replaceable record State
    extends R0;
  end State;
  replaceable partial function g
    input Real p;
    input Real T;
    output State s;
  end g;
end PM;

record SR
  extends R0;
  Real T;
  Real p;
end SR;

package P
  extends PM(redeclare record State = SR);
  function f
    input Real p;
    input Real T;
    output State s;
  algorithm
    s := State(p = p, T = T);
  end f;
  redeclare function extends g
  algorithm
    s := State(p = 10*p, T = 10*T);
  end g;
end P;

package G
  extends P;
end G;

model Top
  replaceable package Medium = G;
  Medium.State a = Medium.f(1, 2 + time);
  Medium.State b = Medium.g(1, 2 + time);
end Top;
"#;

#[test]
fn an_extends_modifier_record_redeclaration_is_the_record_of_the_derived_package() {
    let compiled = Compiler::new()
        .model("Top")
        .compile_str(EXTENDS_MODIFIER_RECORD_REDECLARE, "ExtendsRecord.mo")
        .unwrap_or_else(|error| panic!("Top compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .expect("Top simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (index, time) in result.times.iter().enumerate() {
        assert!((column("a.p")[index] - 1.0).abs() < 1e-12);
        assert!((column("a.T")[index] - (2.0 + time)).abs() < 1e-9);
        assert!((column("b.p")[index] - 10.0).abs() < 1e-12);
        assert!((column("b.T")[index] - 10.0 * (2.0 + time)).abs() < 1e-8);
    }
}
