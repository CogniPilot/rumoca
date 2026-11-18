//! A modifier value is found in the context in which the modifier occurs
//! (MLS 3.7 §7.2, SPEC_0022 INST-001).
//!
//! `tank(s = s)` binds the component's `s` to the enclosing `s`. Translation
//! time evaluation that selects the component's `if s then` branch must read
//! the enclosing value, not the component's own declaration default: the
//! `Modelica.Fluid` AST_BatchPlant tanks pass `stiffCharacteristicForEmptyPort`
//! down this way.

use rumoca::{CompilationResult, Compiler, CompilerError};
use rumoca_core::{Expression, Literal, VarName};
use rumoca_sim::{SimOptions, SimulationSession, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package SameName
  model Tank
    parameter Boolean s = false annotation(Evaluate = true);
    parameter Real k = 1;
    parameter Integer n = 0;
    Real y[n];
    Real z[n];
    Real w;
  equation
    for i in 1:n loop
      if s then
        z[i] = 1 + (if y[i] > 0 then 0 else 10);
        y[i] = time;
      else
        y[i] = 2;
        z[i] = 0;
      end if;
    end for;
    w = k;
  end Tank;
  model Test
    parameter Boolean s = true;
    parameter Real k = 3;
    parameter Integer n = 2;
    Tank tank(s = s, k = k, n = n);
  end Test;
end SameName;
"#;

#[test]
fn a_same_named_modifier_value_is_read_from_the_enclosing_scope() {
    let compiled = Compiler::new()
        .model("SameName.Test")
        .compile_str(SOURCE, "SameName.mo")
        .unwrap_or_else(|error| panic!("SameName.Test compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("SameName.Test simulates: {error}"));
    let last = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        *result.data[index].last().expect("samples")
    };
    // The enclosing `s = true` selects the first branch for both elements.
    for element in 1..=2 {
        assert!((last(&format!("tank.y[{element}]")) - 1.0).abs() < 1e-9);
        assert!((last(&format!("tank.z[{element}]")) - 1.0).abs() < 1e-9);
    }
    assert!((last("tank.w") - 3.0).abs() < 1e-12);
}

fn compile_redeclaration(source: &str) -> Result<CompilationResult, CompilerError> {
    Compiler::new()
        .model("Redecl")
        .compile_str(source, "Redecl.mo")
}

fn assert_redeclared_output(compiled: &CompilationResult, expected: f64) {
    let mut simulation = SimulationSession::new(&compiled.dae, SimOptions::default())
        .expect("initialize the redeclared model");
    simulation
        .advance_to(0.1)
        .expect("advance the redeclared model");
    let value = simulation
        .get("viaMod.m.y")
        .expect("read the selected value")
        .expect("the selected value is visible");
    assert_eq!(value.to_bits(), expected.to_bits());
}

fn assert_redeclared_start(compiled: &CompilationResult) {
    let start = &compiled.flat.variables[&VarName::new("viaMod.m.a")].start;
    match start {
        Some(Expression::Literal {
            value: Literal::Integer(3),
            ..
        }) => {}
        Some(Expression::Literal {
            value: Literal::Real(value),
            ..
        }) => {
            assert_eq!(value.to_bits(), 3.0_f64.to_bits());
        }
        other => panic!("expected to preserve start = 3, got {other:?}"),
    }
}

#[test]
fn a_redeclare_final_attribute_survives_a_separate_outer_value_binding() {
    let compiled = compile_redeclaration(include_str!(
        "../fixtures/redeclare_modifiers/FinalAttributeDisjoint.mo"
    ))
    .expect("a value binding may coexist with an inherited final attribute");
    assert_redeclared_output(&compiled, 6.0);
    assert_redeclared_start(&compiled);
}

fn assert_final_redeclare_error(source: &str, path: &str) {
    let Err(error) = compile_redeclaration(source) else {
        panic!("cannot change a final element");
    };
    let CompilerError::CompileDiagnosticsError { failures, .. } = error else {
        panic!("expected the instantiation diagnostic, got {error:?}");
    };
    let target = format!("`{path}`");
    assert!(
        failures.iter().any(|failure| {
            failure.error_code.as_deref() == Some("EI028") && failure.error.contains(&target)
        }),
        "{failures:?}"
    );
}

#[test]
fn an_outer_start_modifier_cannot_change_a_redeclare_final_attribute() {
    assert_final_redeclare_error(
        include_str!("../fixtures/redeclare_modifiers/FinalAttributeConflict.mo"),
        "a.start",
    );
}

#[test]
fn an_unbound_final_modifier_still_prevents_overriding_the_member() {
    assert_final_redeclare_error(
        include_str!("../fixtures/redeclare_modifiers/FinalBareMember.mo"),
        "a",
    );
}

#[test]
fn a_single_redeclare_modifier_can_set_both_the_binding_and_an_attribute() {
    let compiled = compile_redeclaration(include_str!(
        "../fixtures/redeclare_modifiers/CombinedNestedAndBinding.mo"
    ))
    .expect("compile the combined binding and attribute modification");
    assert_redeclared_output(&compiled, 4.0);
    assert_redeclared_start(&compiled);
}

#[test]
fn an_outer_whole_record_binding_supersedes_redeclare_field_defaults() {
    let compiled = compile_redeclaration(include_str!(
        "../fixtures/redeclare_modifiers/OuterRecordBinding.mo"
    ))
    .expect("compile the record-valued parameter");
    assert_redeclared_output(&compiled, 6.0);
}

#[test]
fn distinct_redeclare_aliases_with_the_same_name_keep_their_own_bindings() {
    let compiled = compile_redeclaration(include_str!(
        "../fixtures/redeclare_modifiers/SameNamedClassSelections.mo"
    ))
    .expect("compile distinct alias selections");
    assert_redeclared_output(&compiled, 6.0);
}

#[test]
fn a_bare_final_target_cannot_capture_an_enclosing_binding() {
    for source in [
        include_str!("../fixtures/redeclare_modifiers/OrdinaryBareFinalScope.mo"),
        include_str!("../fixtures/redeclare_modifiers/OrdinaryNoFinalScope.mo"),
        include_str!("../fixtures/redeclare_modifiers/InheritedBareFinalScope.mo"),
    ] {
        let compiled = compile_redeclaration(source).expect("compile the ordinary nested scope");
        let mut simulation = SimulationSession::new(&compiled.dae, SimOptions::default())
            .expect("initialize the ordinary nested scope");
        simulation.advance_to(0.1).expect("advance the model");
        for name in ["plain.y", "viaShort.m.y", "viaMod.m.y"] {
            let value = simulation
                .get(name)
                .expect("read the symbol")
                .expect("the symbol is visible");
            assert_eq!(value.to_bits(), 1.0_f64.to_bits(), "{name}");
        }
    }
}

#[test]
fn an_outer_modifier_cannot_change_an_ordinary_bare_final_target() {
    assert_final_redeclare_error(
        include_str!("../fixtures/redeclare_modifiers/OrdinaryBareFinalOverride.mo"),
        "m.a",
    );
}

#[test]
fn a_nested_class_is_forwarded_only_by_a_redeclaration() {
    for (source, expected) in [
        (
            include_str!("../fixtures/redeclare_modifiers/BareFinalNestedClass.mo"),
            1.0,
        ),
        (
            include_str!("../fixtures/redeclare_modifiers/NoModifierNestedClass.mo"),
            1.0,
        ),
        (
            include_str!("../fixtures/redeclare_modifiers/ForwardedNestedClass.mo"),
            6.0,
        ),
        (
            include_str!("../fixtures/redeclare_modifiers/ConstrainedBySelectedOuterAlias.mo"),
            6.0,
        ),
        (
            include_str!("../fixtures/redeclare_modifiers/ConstrainedByBareFinalOuterAlias.mo"),
            1.0,
        ),
    ] {
        let compiled = compile_redeclaration(source).expect("compile the nested class selection");
        assert_redeclared_output(&compiled, expected);
    }
}
