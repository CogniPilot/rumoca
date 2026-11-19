//! MLS §7.3: a call through a class's replaceable package selects the package
//! that class instance selects. A class's own default is such a selection, so
//! two components whose defaults differ call two different functions.

use rumoca_core::{Expression, ExpressionVisitor, Reference};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

const SOURCE_NAME: &str = "<package_call_own_default>";
const SOURCE: &str = r#"
package P
  partial package PartialMedium
    constant Real k = 1;
    function scale
      input Real x;
      output Real y;
    algorithm
      y := k*x;
    end scale;
    replaceable record ThermodynamicState
    end ThermodynamicState;
    replaceable partial function density
      input ThermodynamicState state;
      output Real d;
    end density;
  end PartialMedium;
  package Gas
    extends PartialMedium(k = 2);
    redeclare record extends ThermodynamicState
      Real p;
      Real T;
    end ThermodynamicState;
    redeclare function extends density
    algorithm
      d := state.p/(287*state.T);
    end density;
  end Gas;
  package Liquid
    extends PartialMedium(k = 3);
    redeclare record extends ThermodynamicState
      Real p;
      Real T;
    end ThermodynamicState;
    redeclare function extends density
    algorithm
      d := 1000;
    end density;
  end Liquid;
  model GasMachine
    replaceable package Medium = Gas constrainedby PartialMedium;
    Medium.ThermodynamicState state(p = 1e5 + time, T = 300);
    Real d = Medium.density(state);
    Real e;
    Real x(start = 0);
  equation
    e = Medium.density(state);
    der(x) = -x;
  initial equation
    x = Medium.density(state);
  end GasMachine;
  model LiquidMachine
    replaceable package Medium = Liquid constrainedby PartialMedium;
    Medium.ThermodynamicState state(p = 1e5 + time, T = 300);
    Real d = Medium.density(state);
    Real e;
    Real x(start = 0);
  equation
    e = Medium.density(state);
    der(x) = -x;
  initial equation
    x = Medium.density(state);
  end LiquidMachine;
  model Plant
    GasMachine gas;
    LiquidMachine liquid;
  end Plant;
  model PlantReversed
    LiquidMachine liquid;
    GasMachine gas;
  end PlantReversed;
  model Redeclared
    GasMachine first;
    GasMachine second(redeclare package Medium = Liquid);
  end Redeclared;
  model RootAndComponent
    replaceable package Medium = Gas constrainedby PartialMedium;
    Medium.ThermodynamicState state(p = 1e5 + time, T = 300);
    Real d = Medium.density(state);
    LiquidMachine liquid;
  end RootAndComponent;
  partial model PartialTwoPort
    replaceable package Medium = PartialMedium;
    Medium.ThermodynamicState state(p = 1e5 + time, T = 300);
  end PartialTwoPort;
  model Curve
    extends PartialTwoPort(redeclare replaceable package Medium = Gas);
    Real d;
  equation
    d = Medium.density(state);
  end Curve;
  model BoundCurve
    extends PartialTwoPort(redeclare replaceable package Medium = Gas);
    Real d = Medium.density(state);
  end BoundCurve;
  model Machine
    replaceable package Medium = Gas constrainedby PartialMedium;
    Real d = Medium.density(curve.state);
    Curve curve(redeclare package Medium = Medium);
  end Machine;
  model EquationMachine
    replaceable package Medium = Gas constrainedby PartialMedium;
    Real d;
    BoundCurve curve(redeclare package Medium = Medium);
  equation
    d = Medium.density(curve.state);
  end EquationMachine;
  model Vessel
    replaceable package Medium = PartialMedium;
    Real y = Medium.scale(1 + time);
  end Vessel;
  model Column
    replaceable package Medium = Gas constrainedby PartialMedium;
    replaceable package Medium_ST = Gas constrainedby PartialMedium;
    Vessel vessel(redeclare package Medium = Medium);
    Vessel steam(redeclare package Medium = Medium_ST);
  end Column;
  model Still
    Column col(redeclare package Medium_ST = Liquid);
  end Still;
  model Explicit
    replaceable package Medium = Liquid constrainedby PartialMedium;
    Real a = P.Gas.scale(1 + time);
    Real b = Medium.scale(1 + time);
    Real c = Gas.scale(1 + time);
  end Explicit;
  model ExplicitHolder
    Explicit e;
  end ExplicitHolder;
end P;
"#;

fn flatten(model: &str) -> (ast::InstancedTree, flat::Model) {
    let stored = rumoca_phase_parse::parse_to_ast(SOURCE, SOURCE_NAME).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(SOURCE_NAME, SOURCE);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let mut instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("source instantiates");
    rumoca_phase_typecheck::typecheck_instanced(&instanced.tree, &mut instanced.overlay, model)
        .expect("source typechecks");
    let flat = rumoca_phase_flatten::flatten_ref(&instanced.tree, &instanced.overlay, model)
        .expect("source flattens");
    (instanced, flat)
}

#[derive(Default)]
struct Calls(Vec<Reference>);

impl ExpressionVisitor for Calls {
    fn visit_function_call(&mut self, name: &Reference, args: &[Expression], constructor: bool) {
        if !constructor {
            self.0.push(name.clone());
        }
        self.walk_function_call(name, args, constructor);
    }
}

fn calls_in(expression: &Expression) -> Vec<Reference> {
    let mut calls = Calls::default();
    calls.visit_expression(expression);
    calls.0
}

/// Every `density` call of one component: its binding, its equation and its
/// initial equation.
fn component_density_calls(flat: &flat::Model, component: &str) -> Vec<Reference> {
    let prefix = format!("{component}.");
    let mentions_component = |expression: &Expression| {
        let mut names = Vec::new();
        expression.collect_var_refs(&mut names);
        names.iter().any(|name| name.as_str().starts_with(&prefix))
    };
    let mut calls = Vec::new();
    for (name, variable) in &flat.variables {
        if name.as_str().starts_with(&prefix)
            && let Some(binding) = &variable.binding
        {
            calls.extend(calls_in(binding));
        }
    }
    for equation in flat.equations.iter().chain(&flat.initial_equations) {
        if mentions_component(&equation.residual) {
            calls.extend(calls_in(&equation.residual));
        }
    }
    calls
}

fn assert_component_calls(
    instanced: &ast::InstancedTree,
    flat: &flat::Model,
    component: &str,
    package: &str,
) {
    let function = format!("{package}.density");
    let expected = instanced
        .tree
        .get_def_id_by_name(&function)
        .expect("fixture declares the function");
    let calls = component_density_calls(flat, component);
    assert_eq!(
        calls.len(),
        3,
        "{component}: binding, equation and initial equation each call density: {calls:?}"
    );
    for call in &calls {
        assert_eq!(
            call.as_str(),
            function,
            "{component} calls its own package's function"
        );
        assert_eq!(
            call.target_def_id(),
            Some(expected),
            "{component}: {call:?}"
        );
    }
    let collected = flat
        .functions
        .iter()
        .find_map(|(name, collected)| (name.as_str() == function).then_some(collected))
        .unwrap_or_else(|| panic!("{function} is collected"));
    assert_eq!(collected.def_id, Some(expected));
}

#[test]
fn components_with_different_default_packages_call_their_own_function() {
    let (instanced, flat) = flatten("P.Plant");
    assert_component_calls(&instanced, &flat, "gas", "P.Gas");
    assert_component_calls(&instanced, &flat, "liquid", "P.Liquid");
}

#[test]
fn the_selected_function_does_not_depend_on_declaration_order() {
    let (instanced, flat) = flatten("P.PlantReversed");
    assert_component_calls(&instanced, &flat, "gas", "P.Gas");
    assert_component_calls(&instanced, &flat, "liquid", "P.Liquid");
}

#[test]
fn a_default_and_a_redeclare_of_one_class_call_their_own_function() {
    let (instanced, flat) = flatten("P.Redeclared");
    assert_component_calls(&instanced, &flat, "first", "P.Gas");
    assert_component_calls(&instanced, &flat, "second", "P.Liquid");
}

#[test]
fn a_root_call_and_a_component_call_select_their_own_function() {
    let (instanced, flat) = flatten("P.RootAndComponent");
    assert_component_calls(&instanced, &flat, "liquid", "P.Liquid");
    let gas = instanced.tree.get_def_id_by_name("P.Gas.density").unwrap();
    let binding = flat
        .variables
        .iter()
        .find_map(|(name, variable)| (name.as_str() == "d").then_some(variable))
        .and_then(|variable| variable.binding.as_ref())
        .expect("root d has a binding");
    let calls = calls_in(binding);
    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].target_def_id(), Some(gas));
    let collected = flat
        .functions
        .iter()
        .find_map(|(name, collected)| (name.as_str() == calls[0].as_str()).then_some(collected))
        .expect("the root call's function is collected");
    assert_eq!(collected.def_id, Some(gas));
}

/// Every call in the flat model, wherever it is written.
fn all_calls(flat: &flat::Model) -> Vec<Reference> {
    let mut calls = Vec::new();
    for variable in flat.variables.values() {
        if let Some(binding) = &variable.binding {
            calls.extend(calls_in(binding));
        }
    }
    for equation in flat.equations.iter().chain(&flat.initial_equations) {
        calls.extend(calls_in(&equation.residual));
    }
    calls
}

fn assert_every_call_is_gas_density(model: &str) {
    let (instanced, flat) = flatten(model);
    let gas = instanced.tree.get_def_id_by_name("P.Gas.density").unwrap();
    let calls = all_calls(&flat);
    assert_eq!(calls.len(), 2, "{model}: {calls:?}");
    for call in &calls {
        assert_eq!(call.target_def_id(), Some(gas), "{model}: {call:?}");
        let collected = flat
            .functions
            .iter()
            .find_map(|(name, collected)| (name.as_str() == call.as_str()).then_some(collected))
            .unwrap_or_else(|| panic!("{model}: {} is collected", call.as_str()));
        let inputs: Vec<_> = collected
            .inputs
            .iter()
            .map(|input| input.name.as_str())
            .collect();
        assert_eq!(
            inputs,
            ["state_p", "state_T"],
            "{model}: {} takes the selected package's state",
            call.as_str()
        );
    }
}

#[test]
fn a_root_call_and_an_inherited_component_call_share_the_selected_function() {
    assert_every_call_is_gas_density("P.Machine");
    assert_every_call_is_gas_density("P.EquationMachine");
}

#[test]
fn two_selections_of_one_class_call_their_own_function() {
    let (_, flat) = flatten("P.Still");
    for (variable, function) in [
        ("col.vessel.y", "P.Gas.scale"),
        ("col.steam.y", "P.Liquid.scale"),
    ] {
        assert_eq!(
            binding_call(&flat, variable).as_str(),
            function,
            "{variable}"
        );
    }
}

fn binding_call(flat: &flat::Model, variable: &str) -> Reference {
    let binding = flat
        .variables
        .iter()
        .find_map(|(name, candidate)| (name.as_str() == variable).then_some(candidate))
        .and_then(|candidate| candidate.binding.as_ref())
        .unwrap_or_else(|| panic!("{variable} has a binding"));
    let calls = calls_in(binding);
    assert_eq!(calls.len(), 1, "{variable}: {calls:?}");
    calls.into_iter().next().unwrap()
}

/// `scale` is inherited, so each package's exposure is told apart by the
/// package it is called through, which fixes the constant `k` it reads.
#[test]
fn a_call_spelled_through_a_named_package_is_not_reselected() {
    for (model, prefix) in [("P.Explicit", ""), ("P.ExplicitHolder", "e.")] {
        let (_, flat) = flatten(model);
        for (variable, function) in [
            ("a", "P.Gas.scale"),
            ("b", "P.Liquid.scale"),
            ("c", "P.Gas.scale"),
        ] {
            let call = binding_call(&flat, &format!("{prefix}{variable}"));
            assert_eq!(call.as_str(), function, "{model} {variable}");
        }
    }
}
