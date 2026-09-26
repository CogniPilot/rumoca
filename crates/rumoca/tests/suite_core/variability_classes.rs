//! MLS variability through compilation (MLS §4.4.4, §4.5, §8.6): one
//! declaration of each class keeps its class in the DAE, in the FMI 3
//! classification (SPEC_0044 ME-PARAM-001), and in simulation, where a
//! parameter set takes effect and literal folding (SPEC_0043 §4) never
//! reaches a parameter or a discrete variable.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const MODEL: &str = "
model VariabilityClasses
  function twice
    input Real u;
    output Real y;
  algorithm
    y := 2 * u;
  end twice;
  constant Real c = 2;
  parameter Real pe = 3 annotation(Evaluate = true);
  final parameter Real pf = 4;
  parameter Real p = 5;
  parameter Real pc = 2 * p;
  parameter Real unused = 6;
  parameter Real x0 = 1;
  Real a;
  Real x(start = x0, fixed = true);
  Real y;
equation
  a = 7;
  der(x) = -x;
  y = a + c + pe + pf + pc + twice(p) + twice(c);
end VariabilityClasses;";

const DISCRETE: &str = "
model DiscreteClass
  discrete Real d(start = 0, fixed = true);
  discrete Real e(start = 0, fixed = true);
  Real x(start = 1, fixed = true);
equation
  der(x) = -x;
  when time > 0.5 then
    d = 3;
    e = pre(e) + 1;
  end when;
end DiscreteClass;";

fn compile(model: &str, source: &str) -> rumoca::CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile {model}: {error:#}"))
}

/// One declaration's `(role, variability, is_tunable, evaluable)` in the
/// emitted DAE JSON.
fn dae_class(json: &serde_json::Value, name: &str) -> (String, String, bool, bool) {
    let variable = json["storage"]["variables"]
        .as_array()
        .and_then(|variables| variables.iter().find(|variable| variable["name"] == name))
        .unwrap_or_else(|| panic!("{name} is a DAE declaration"));
    let text = |value: &serde_json::Value| value.as_str().unwrap_or_default().to_string();
    (
        text(&variable["role"]),
        text(&variable["variability"]),
        variable["attributes"]["is_tunable"] == true,
        variable["attributes"]["evaluable"] == true,
    )
}

#[test]
fn the_dae_keeps_each_declared_variability_class() {
    let json: serde_json::Value =
        serde_json::from_str(&compile("VariabilityClasses", MODEL).to_json().unwrap()).unwrap();
    for (name, role, variability, tunable, evaluable) in [
        ("c", "constant", "constant", false, false),
        ("pe", "parameter", "parameter", false, true),
        ("pf", "parameter", "parameter", false, true),
        ("p", "parameter", "parameter", true, false),
        ("unused", "parameter", "parameter", true, false),
        ("x0", "parameter", "parameter", true, false),
        ("a", "algebraic", "continuous", false, false),
        ("x", "state", "continuous", false, false),
    ] {
        assert_eq!(
            dae_class(&json, name),
            (role.into(), variability.into(), tunable, evaluable),
            "{name}"
        );
    }
    let discrete: serde_json::Value =
        serde_json::from_str(&compile("DiscreteClass", DISCRETE).to_json().unwrap()).unwrap();
    for name in ["d", "e"] {
        assert_eq!(
            dae_class(&discrete, name),
            ("discrete_real".into(), "discrete".into(), false, false),
            "{name}"
        );
    }
}

/// The `causality`, `variability`, and `initial` of one scalar variable in a
/// modelDescription, and whether it carries a `start`.
fn fmi_class(description: &str, name: &str) -> (String, String, String, bool) {
    let needle = format!(" name=\"{name}\" ");
    let entry = description
        .lines()
        .find(|line| line.contains(&needle))
        .unwrap_or_else(|| panic!("{name} is published"));
    let attribute = |key: &str| {
        let key = format!(" {key}=\"");
        entry
            .find(&key)
            .map(|at| {
                let rest = &entry[at + key.len()..];
                rest[..rest.find('"').unwrap_or(0)].to_string()
            })
            .unwrap_or_default()
    };
    (
        attribute("causality"),
        attribute("variability"),
        attribute("initial"),
        entry.contains(" start=\""),
    )
}

#[test]
fn the_fmi3_description_classifies_each_variability_class() {
    let compiled = compile("VariabilityClasses", MODEL);
    let description = rumoca::render_target_files(&compiled, "VariabilityClasses", "fmi3", None)
        .unwrap_or_else(|error| panic!("render fmi3: {error:#}"))
        .into_iter()
        .find(|file| file.path.ends_with("modelDescription.xml"))
        .expect("an FMI 3 description")
        .content;
    for (name, causality, variability, initial, start) in [
        ("c", "local", "constant", "exact", true),
        ("pe", "calculatedParameter", "fixed", "calculated", false),
        ("pf", "calculatedParameter", "fixed", "calculated", false),
        ("p", "parameter", "tunable", "exact", true),
        ("pc", "calculatedParameter", "tunable", "calculated", false),
        ("unused", "parameter", "tunable", "exact", true),
        ("x0", "parameter", "tunable", "exact", true),
        ("a", "local", "continuous", "calculated", false),
        ("x", "local", "continuous", "calculated", false),
    ] {
        assert_eq!(
            fmi_class(&description, name),
            (causality.into(), variability.into(), initial.into(), start),
            "{name}"
        );
    }
}

fn simulate(model: &str, source: &str, overrides: &[(&str, f64)]) -> rumoca_sim::SimResult {
    simulate_dae_with_diagnostics(
        &compile(model, source).dae,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            param_overrides: overrides
                .iter()
                .map(|(name, value)| ((*name).to_string(), *value))
                .collect(),
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("simulate {model}: {error:#}"))
}

fn column<'a>(result: &'a rumoca_sim::SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|n| n == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    &result.data[index]
}

/// A parameter set takes effect: the literal-bound `p` stays a run-time read
/// through `pc` and the unfolded call `twice(p)`, and the state start `x0` is
/// assigned at initialization, while the constant call `twice(c)` and the
/// literal-bound algebraic `a` fold without losing `a` as a declaration.
#[test]
fn parameter_sets_take_effect_and_folding_stops_at_parameters() {
    let base = simulate("VariabilityClasses", MODEL, &[]);
    let set = simulate("VariabilityClasses", MODEL, &[("p", 6.0), ("x0", 3.0)]);
    let folded = 7.0 + 2.0 + 3.0 + 4.0 + 4.0;
    for (result, p, x0) in [(&base, 5.0, 1.0), (&set, 6.0, 3.0)] {
        for (row, &time) in result.times.iter().enumerate() {
            let expected = [
                ("a", 7.0),
                ("y", folded + 4.0 * p),
                ("x", x0 * (-time).exp()),
            ];
            for (name, value) in expected {
                let actual = column(result, name)[row];
                assert!(
                    (actual - value).abs() < 1e-4 * value.abs().max(1.0),
                    "{name} at {time} with p = {p}, x0 = {x0}: {actual} != {value}"
                );
            }
        }
    }
}

/// A discrete variable assigned a literal in a `when` clause is not read as
/// that literal elsewhere: it holds its start until the event and its
/// memory `pre(e)` counts events.
#[test]
fn a_discrete_literal_assignment_is_not_propagated() {
    let result = simulate("DiscreteClass", DISCRETE, &[]);
    for (row, &time) in result.times.iter().enumerate() {
        let (d, e) = if time > 0.5 + 1e-9 {
            (3.0, 1.0)
        } else {
            (0.0, 0.0)
        };
        if (time - 0.5).abs() < 1e-9 {
            continue;
        }
        assert_eq!(column(&result, "d")[row], d, "d at {time}");
        assert_eq!(column(&result, "e")[row], e, "e at {time}");
    }
}
