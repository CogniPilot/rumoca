//! Predefined-type attributes inherited through short type definitions and
//! `extends` of a type (MLS §4.8.1, §4.9), merged with the component's own
//! modifiers and outer modifications by MLS §7.2 precedence.

use rumoca::{CompilationResult, Compiler};
use rumoca_core::{Expression, Literal, StateSelect, VarName};
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package TypeAttributes
  constant Real scale = 4;
  type Temp = Real(start = 288.15, nominal = 300, min = 0);
  type Base = Real(start = 2, max = 1000, unit = "K",
    quantity = "ThermodynamicTemperature", displayUnit = "degC");
  type Narrowed = Base(min = 1, start = 3);
  type Extended
    extends Narrowed(nominal = 5);
  end Extended;
  type Scaled = Real(start = scale, fixed = true, stateSelect = StateSelect.prefer);
  type Locked = Real(final start = 1);
  type Relocked = Locked(start = 2);
  type Triple = Real[3](start = {1, 2, 3});
  type Filled = Real[3](each start = 5);
  type Level = enumeration(low, mid, high);
  type MidLevel = Level(start = Level.mid);

  model Minimal
    Temp t;
  equation
    der(t) = -t;
  end Minimal;

  model Chain
    Base b;
    Narrowed n;
    Extended e;
    Extended own(start = 7, min = 0.5, displayUnit = "K");
    Scaled s;
  equation
    der(b) = -b;
    der(n) = -n;
    der(e) = -e;
    der(own) = -own;
    der(s) = -s;
  end Chain;

  model Holder
    Extended e;
  equation
    der(e) = -e;
  end Holder;

  model Outer
    Holder h(e(start = 9));
  end Outer;

  model OwnFinal
    Locked x(start = 2);
  equation
    der(x) = -x;
  end OwnFinal;

  model ChainFinal
    Relocked x;
  equation
    der(x) = -x;
  end ChainFinal;

  model Arrays
    Triple v;
    Triple w[2];
    Filled u;
    Temp t[2];
  equation
    der(v) = -v;
    for i in 1:2 loop
      der(w[i]) = -w[i];
    end for;
    der(u) = -u;
    der(t) = -t;
  end Arrays;

  model Discrete
    MidLevel level;
    parameter Temp unbound;
    parameter Temp bound = 300;
  equation
    when time > 0.5 then
      level = Level.high;
    end when;
  end Discrete;
end TypeAttributes;
"#;

fn compile(model: &str) -> CompilationResult {
    match Compiler::new()
        .model(model)
        .compile_str(SOURCE, "TypeAttributes.mo")
    {
        Ok(compiled) => compiled,
        Err(err) => panic!("{model} should compile: {err:?}"),
    }
}

fn number(expr: Option<&Expression>, what: &str) -> f64 {
    match expr {
        Some(Expression::Literal {
            value: Literal::Real(value),
            ..
        }) => *value,
        Some(Expression::Literal {
            value: Literal::Integer(value),
            ..
        }) => *value as f64,
        other => panic!("{what}: expected a numeric literal, got {other:?}"),
    }
}

fn flat_var<'a>(compiled: &'a CompilationResult, name: &str) -> &'a rumoca_ir_flat::Variable {
    let Some(variable) = compiled.flat.variables.get(&VarName::new(name)) else {
        panic!("missing flat variable {name}");
    };
    variable
}

fn trace_at(compiled: &CompilationResult, name: &str, t_end: f64) -> (f64, f64) {
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end,
            dt: Some(t_end / 4.0),
            ..Default::default()
        },
    )
    .expect("simulation should succeed");
    let Some(index) = result.names.iter().position(|n| n == name) else {
        panic!("missing trace {name}");
    };
    let trace = &result.data[index];
    (trace[0], trace[trace.len() - 1])
}

#[test]
fn short_type_definition_attributes_reach_the_variable() {
    let compiled = compile("TypeAttributes.Minimal");
    let t = flat_var(&compiled, "t");
    assert_eq!(number(t.start.as_ref(), "t.start"), 288.15);
    assert_eq!(number(t.nominal.as_ref(), "t.nominal"), 300.0);
    assert_eq!(number(t.min.as_ref(), "t.min"), 0.0);

    let (initial, last) = trace_at(&compiled, "t", 1.0);
    assert!((initial - 288.15).abs() < 1e-9, "t(0) = {initial}");
    let expected = 288.15 * (-1.0f64).exp();
    assert!(
        (last - expected).abs() < 1e-4 * expected,
        "t(1) = {last}, expected {expected}"
    );
}

#[test]
fn innermost_type_in_the_chain_wins_and_component_modifiers_win_over_it() {
    let compiled = compile("TypeAttributes.Chain");

    let b = flat_var(&compiled, "b");
    assert_eq!(number(b.start.as_ref(), "b.start"), 2.0);
    assert_eq!(number(b.max.as_ref(), "b.max"), 1000.0);
    assert!(b.min.is_none());
    assert_eq!(b.unit.as_deref(), Some("K"));

    let n = flat_var(&compiled, "n");
    assert_eq!(number(n.start.as_ref(), "n.start"), 3.0);
    assert_eq!(number(n.min.as_ref(), "n.min"), 1.0);
    assert_eq!(number(n.max.as_ref(), "n.max"), 1000.0);

    let e = flat_var(&compiled, "e");
    assert_eq!(number(e.start.as_ref(), "e.start"), 3.0);
    assert_eq!(number(e.min.as_ref(), "e.min"), 1.0);
    assert_eq!(number(e.max.as_ref(), "e.max"), 1000.0);
    assert_eq!(number(e.nominal.as_ref(), "e.nominal"), 5.0);
    assert_eq!(e.quantity.as_deref(), Some("ThermodynamicTemperature"));
    assert_eq!(e.display_unit.as_deref(), Some("degC"));

    let own = flat_var(&compiled, "own");
    assert_eq!(number(own.start.as_ref(), "own.start"), 7.0);
    assert_eq!(number(own.min.as_ref(), "own.min"), 0.5);
    assert_eq!(number(own.nominal.as_ref(), "own.nominal"), 5.0);
    assert_eq!(own.display_unit.as_deref(), Some("K"));

    let s = flat_var(&compiled, "s");
    assert_eq!(s.fixed.as_deref(), Some(&[true][..]));
    assert_eq!(s.state_select, StateSelect::Prefer);
    let (initial, _) = trace_at(&compiled, "s", 0.5);
    assert!((initial - 4.0).abs() < 1e-9, "s(0) = {initial}");
}

#[test]
fn outer_modification_wins_over_the_type_chain() {
    let compiled = compile("TypeAttributes.Outer");
    let e = flat_var(&compiled, "h.e");
    assert_eq!(number(e.start.as_ref(), "h.e.start"), 9.0);
    assert_eq!(number(e.min.as_ref(), "h.e.min"), 1.0);
    assert_eq!(number(e.nominal.as_ref(), "h.e.nominal"), 5.0);
}

#[test]
fn declaration_modifier_of_a_final_type_attribute_is_rejected() {
    let err = Compiler::new()
        .model("TypeAttributes.OwnFinal")
        .compile_str(SOURCE, "TypeAttributes.mo")
        .expect_err("MLS §7.2.6: a final type attribute cannot be modified");
    let message = format!("{err:?}");
    assert!(message.contains("final"), "{message}");
}

#[test]
fn derived_type_modifying_a_final_base_attribute_is_rejected() {
    let err = Compiler::new()
        .model("TypeAttributes.ChainFinal")
        .compile_str(SOURCE, "TypeAttributes.mo")
        .expect_err("MLS §7.2.6: a derived type cannot modify a final base attribute");
    let message = format!("{err:?}");
    assert!(message.contains("final"), "{message}");
}

#[test]
fn array_type_attributes_repeat_over_the_component_dimensions() {
    let compiled = compile("TypeAttributes.Arrays");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            dt: Some(0.25),
            ..Default::default()
        },
    )
    .expect("simulation should succeed");
    let initial = |name: &str| {
        let Some(index) = result.names.iter().position(|n| n == name) else {
            panic!("missing trace {name} in {:?}", result.names);
        };
        result.data[index][0]
    };
    for (name, expected) in [
        ("v[1]", 1.0),
        ("v[3]", 3.0),
        ("w[1,1]", 1.0),
        ("w[1,3]", 3.0),
        ("w[2,1]", 1.0),
        ("w[2,2]", 2.0),
        ("u[2]", 5.0),
        ("t[2]", 288.15),
    ] {
        let value = initial(name);
        assert!((value - expected).abs() < 1e-9, "{name}(0) = {value}");
    }
}

#[test]
fn enumeration_and_parameter_types_supply_their_start() {
    let compiled = compile("TypeAttributes.Discrete");
    let level = flat_var(&compiled, "level");
    let start = format!("{:?}", level.start.as_ref().expect("level.start"));
    assert!(start.contains("mid"), "{start}");

    let unbound = flat_var(&compiled, "unbound");
    assert_eq!(number(unbound.start.as_ref(), "unbound.start"), 288.15);
    assert_eq!(number(unbound.nominal.as_ref(), "unbound.nominal"), 300.0);
    let bound = flat_var(&compiled, "bound");
    assert_eq!(number(bound.binding.as_ref(), "bound.binding"), 300.0);
    assert_eq!(number(bound.min.as_ref(), "bound.min"), 0.0);
}
