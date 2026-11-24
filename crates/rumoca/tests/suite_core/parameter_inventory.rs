//! MLS §§5.3, 10.1, and 12.4: component parameter scopes and function formals.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"package ParameterInventoryProbe
  function bump
    input Integer n;
    output Integer y;
  algorithm
    y := n + 1;
  end bump;
  model Component
    parameter Integer n;
    parameter Real gain;
    parameter Boolean enabled = true;
    parameter Real values[:,2] = fill(gain, n, 2);
    final parameter Integer count = size(values, 1);
    Real x(start=0, fixed=true);
  equation
    der(x) = if enabled then bump(count)*gain else 0;
  end Component;
  model Root
    parameter Integer n=99;
    Component left(n=2, gain=0.5);
    Component right(n=5, gain=2);
    Real y=left.x+right.x;
  end Root;
end ParameterInventoryProbe;
"#;

#[test]
fn parameter_inventory_preserves_sibling_shapes_and_function_formals() {
    let compiled = Compiler::new()
        .model("ParameterInventoryProbe.Root")
        .compile_str(SOURCE, "parameter_inventory.mo")
        .unwrap();
    for (component, rows) in [("left", 2), ("right", 5)] {
        let name = rumoca_core::VarName::new(format!("{component}.values"));
        assert_eq!(compiled.flat.variables[&name].dims, vec![rows, 2]);
    }
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap();
        for (name, rate) in [("left.x", 1.5), ("right.x", 12.0), ("y", 13.5)] {
            let channel = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap();
            assert!(!result.times.is_empty());
            for (&time, &actual) in result.times.iter().zip(&result.data[channel]) {
                assert!(
                    (actual - rate * time).abs() < 1e-7,
                    "{solver_mode:?}: {name}({time})={actual}"
                );
            }
        }
    }
}

#[test]
fn input_component_keeps_declared_parameter_and_constant_roles() {
    use rumoca_ir_dae::VariableRole;

    let compiled = Compiler::new()
        .model("InputComponentParameters")
        .compile_str(
            include_str!("../fixtures/attribute_semantics/InputComponentParameters.mo"),
            "InputComponentParameters.mo",
        )
        .unwrap();
    compiled.dae.inspect(|view| {
        for (name, expected) in [
            ("signal.offset", VariableRole::Constant),
            ("signal.gain", VariableRole::Parameter),
            ("signal.u", VariableRole::Input),
        ] {
            let (_, variable) = view
                .variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .unwrap_or_else(|| panic!("missing declaration {name}"));
            assert_eq!(variable.role(), expected, "{name}");
        }
    });
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .unwrap();
    let output = result.names.iter().position(|name| name == "y").unwrap();
    let samples = &result.data[output];
    assert!(!samples.is_empty());
    assert!(samples.iter().all(|&value| value == 7.0));
}

#[test]
fn nested_parameter_modifier_keeps_its_enclosing_runtime_binding() {
    use rumoca_compile::compile::{Session, SessionConfig, StructuralOverride};

    let source = r#"model Leaf
  parameter Real startValue = 0;
  output Real y;
initial equation
  y = startValue;
equation
  der(y) = 0;
end Leaf;
model Probe
  parameter Real startValue = 60;
  Leaf leaf(startValue = startValue);
end Probe;"#;
    let mut session = Session::new(SessionConfig::default());
    session.add_document("nested_modifier.mo", source).unwrap();
    session.set_structural_overrides(&[("startValue".into(), StructuralOverride::Real(60.0))]);
    let compiled = session.compile_model("Probe").unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            param_overrides: vec![("startValue".into(), 4.0)],
            ..Default::default()
        },
    )
    .unwrap();
    let y = result
        .names
        .iter()
        .position(|name| name == "leaf.y")
        .unwrap();
    assert_eq!(result.data[y].first().copied(), Some(4.0));
}

/// MLS §7.2.4: `m(r = d.r)` binds the whole record, so it overrides the field
/// modifier `r(w = ...)` of the extends clause it modifies, in `m.r` and in
/// every component `m` forwards `r` to. The field value is once an Integer
/// literal and once not: either is overridden.
#[test]
fn a_record_binding_overrides_the_field_modifiers_of_the_declaration_it_modifies() {
    for inner_field_value in ["5", "5.5"] {
        let source = format!(
            r#"record R
  parameter Real a = 0;
  parameter Real w = 1;
  final parameter Real t = a/w;
end R;
record Data
  parameter R r(a = 0, w = 7);
end Data;
model Part
  parameter R r;
  Real torque = r.t*r.w;
  Real speed = r.w;
end Part;
model Base
  parameter R r;
  Part part(final r = r);
end Base;
model Mid
  extends Base(r(w = {inner_field_value}));
end Mid;
model Top
  parameter Data d(r(a = 180, w = 3));
  Mid m(r = d.r);
  Real own = m.r.w;
  Real x(start = 0, fixed = true);
equation
  der(x) = m.part.torque;
end Top;"#
        );
        let compiled = Compiler::new()
            .model("Top")
            .compile_str(&source, "record_binding_over_field_modifier.mo")
            .unwrap();
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.1),
                ..Default::default()
            },
        )
        .unwrap();
        for (name, expected) in [
            ("own", 3.0),
            ("m.part.speed", 3.0),
            ("m.part.torque", 180.0),
        ] {
            let channel = result
                .names
                .iter()
                .position(|channel| channel == name)
                .unwrap_or_else(|| panic!("missing {name}"));
            assert_eq!(
                result.data[channel].first().copied(),
                Some(expected),
                "{name} with the inner field modifier w = {inner_field_value}"
            );
        }
    }
}

/// MLS §12.9: an external body is foreign code, which numeric evaluation
/// cannot run. A parameter bound to an external "C" call is refused with the
/// function and its symbol, not given a plausible value.
#[test]
fn a_parameter_bound_to_an_external_c_call_is_refused_with_its_symbol() {
    let source = r#"model ExternalParameter
  function f
    input Real x;
    output Real y;
    external "C" y = no_such_symbol(x);
  end f;
  parameter Real p = f(1);
  Real x(start = 0, fixed = true);
equation
  der(x) = p;
end ExternalParameter;"#;
    let compiled = Compiler::new()
        .model("ExternalParameter")
        .compile_str(source, "external_parameter.mo")
        .unwrap();
    let error = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect_err("an external C body cannot be evaluated");
    assert!(
        error.to_string().contains(
            "external C function `f` calls `no_such_symbol`, which this runtime cannot execute"
        ),
        "{error:#}"
    );
}
