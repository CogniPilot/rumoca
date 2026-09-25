//! STRUCT-T10(a): `final` and `Evaluate=true` parameters with evaluable
//! bindings are read as their values, while every declaration stays and
//! ordinary parameters keep their runtime meaning.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::fold_evaluable_parameters;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const FOLDED: &str = "
model Folded
  parameter Real n[3] = {0, 0, 1} annotation(Evaluate = true);
  final parameter Real e[3] = n / sqrt(n * n);
  parameter Real k = 2;
  final parameter Real kk = 2 * k;
  parameter Integer m = 2 annotation(Evaluate = true);
  parameter Real tEvent = 0.45 annotation(Evaluate = true);
  parameter Real w0 = 3 annotation(Evaluate = true);
  parameter Real wFree(fixed = false) annotation(Evaluate = true);
  Real x(start = 1, fixed = true);
  Real y[3];
  Real w;
  discrete Real d(start = 0, fixed = true);
initial equation
  w = w0;
  wFree = w0 + 1;
equation
  der(x) = -e[3] * x * kk / m;
  y = e * x + {0, 0, wFree};
  der(w) = 0;
  when time > tEvent then
    d = m;
  end when;
  assert(e[3] > 0, \"the axis keeps a positive third component\");
end Folded;";

fn compile(source: &str, name: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(name)
        .compile_str(source, "evaluable_parameters.mo")
        .unwrap()
        .dae
}

fn evaluable(model: &dae::Dae) -> Vec<String> {
    model.inspect(|view| {
        view.variables()
            .filter(|(_, variable)| variable.is_evaluable())
            .map(|(_, variable)| variable.name().to_string())
            .collect()
    })
}

/// Names of every parameter an expression coordinate reads.
fn read_parameters(model: &dae::Dae) -> Vec<String> {
    model.inspect(|view| {
        let mut names = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .filter_map(|node| match node.operation() {
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) => {
                    Some(view.variable(id.into()).unwrap().name().to_string())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        names.sort();
        names.dedup();
        names
    })
}

fn declarations(model: &dae::Dae) -> Vec<(String, dae::VariableRole, bool)> {
    model.inspect(|view| {
        view.variables()
            .map(|(_, variable)| {
                (
                    variable.name().to_string(),
                    variable.role(),
                    variable.binding().is_some(),
                )
            })
            .collect()
    })
}

#[test]
fn only_final_and_evaluate_parameters_over_evaluable_bindings_are_marked() {
    let source = compile(FOLDED, "Folded");
    assert_eq!(evaluable(&source), ["n", "e", "m", "tEvent", "w0"]);
}

/// A conditional binding reads every branch: a translation-time condition that
/// would select a literal does not make the ordinary parameter in the other
/// branch disappear, so the parameter stays unmarked and construction accepts
/// the model (the MSL `ThermalAmbientSMPM.constTr` shape).
#[test]
fn a_binding_with_an_ordinary_branch_is_not_marked() {
    let source = compile(
        "model Branches
           final parameter Boolean useFirst = false;
           parameter Real ordinary = 2;
           final parameter Real k = if useFirst then ordinary else 3;
           Real x(start = 1, fixed = true);
         equation
           der(x) = -k * x;
         end Branches;",
        "Branches",
    );
    assert_eq!(evaluable(&source), ["useFirst"]);
}

#[test]
fn folding_replaces_every_evaluable_read_and_keeps_every_declaration() {
    let source = compile(FOLDED, "Folded");
    let folded = fold_evaluable_parameters(&source)
        .unwrap()
        .expect("evaluable parameters are read");
    assert_eq!(declarations(&source), declarations(&folded));
    assert_eq!(evaluable(&source), evaluable(&folded));
    let before = read_parameters(&source);
    for name in ["n", "e", "m", "tEvent", "w0"] {
        assert!(before.contains(&name.to_string()), "{name}: {before:?}");
    }
    assert_eq!(read_parameters(&folded), ["k", "kk", "wFree"]);
}

#[test]
fn folded_values_drive_event_initial_and_continuous_rows() {
    let source = compile(FOLDED, "Folded");
    let result = simulate_dae_with_diagnostics(
        &source,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .unwrap();
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    for (row, &time) in result.times.iter().enumerate() {
        let x = (-2.0 * time).exp();
        let d = if time > 0.45 { 2.0 } else { 0.0 };
        for (name, expected) in [("x", x), ("y[3]", x + 4.0), ("w", 3.0), ("d", d)] {
            // The event instant reports both limits; only its sides are fixed.
            if name == "d" && (time - 0.45).abs() < 1e-6 {
                continue;
            }
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-4,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}

#[test]
fn an_assertion_over_a_folded_parameter_is_still_enforced() {
    let text = FOLDED.replace(
        "assert(e[3] > 0, \"the axis keeps a positive third component\");",
        "assert(m < 2, \"m stays below two\");",
    );
    let source = compile(&text, "Folded");
    let folded = fold_evaluable_parameters(&source).unwrap().unwrap();
    assert!(!read_parameters(&folded).contains(&"m".to_string()));
    let result = simulate_dae_with_diagnostics(
        &source,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    );
    let Err(error) = result else {
        panic!("the folded assertion fails");
    };
    let error = format!("{error:?}");
    assert!(error.contains("m stays below two"), "{error}");
}
