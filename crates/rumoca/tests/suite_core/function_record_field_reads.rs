//! MLS §12.4.4 for a field of a record result: the field is checked where it
//! is read.
//!
//! `Modelica.Media.Water.IF97_Utilities.BaseIF97.Regions.boilingcurve_p`
//! assigns `cv` of its result record in one arm of an `if` and `cp`, `vt`,
//! `vp` in the other, and its callers (`hvl_p(p, boilingcurve_p(p))`,
//! `hvl_dp`, `drhovl_dp`) read only the fields the taken arm assigned. A
//! field the executed path never wrote is returned absent; its assigned
//! state travels with the record through the return and through a
//! whole-record call argument, and reading it fails at that read, on the
//! executed path only. Any other use of the whole record uses every field.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

/// `make` writes one field per arm; `READER` receives the record whole.
const PASSED_ON: &str = r#"
model PassedOn
  record R
    Real a;
    Real b;
  end R;
  function make
    input Real x;
    output R r;
  algorithm
    if x > 0 then
      r.a := x;
    else
      r.b := -x;
    end if;
  end make;
  function pick "reads, in an if expression, the field the same test wrote"
    input Real x;
    input R r;
    output Real y;
  algorithm
    y := if x > 0 then r.a else 10*r.b;
  end pick;
  function pick_statement "the same reads in the arms of an if statement"
    input Real x;
    input R r;
    output Real y;
  algorithm
    if x > 0 then
      y := r.a;
    else
      y := 10*r.b;
    end if;
  end pick_statement;
  function wrong "reads the field the other arm wrote"
    input Real x;
    input R r;
    output Real y;
  algorithm
    y := if x > 0.5 then r.b else r.a;
  end wrong;
  function relay "hands the record on"
    input Real x;
    input R r;
    output Real y;
  algorithm
    y := pick(x, r) + 1;
  end relay;
  Real y = READER(time - 0.25, make(time - 0.25));
end PassedOn;
"#;

/// The whole record bound to a model variable: every field is used.
const BOUND: &str = r#"
model Bound
  record R
    Real a;
    Real b;
  end R;
  function make
    input Real x;
    output R r;
  algorithm
    if x > 0 then
      r.a := x;
    else
      r.b := -x;
    end if;
  end make;
  R r = make(time + 1);
end Bound;
"#;

fn simulate(model: &str, source: &str) -> Result<SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .map_err(|error| error.to_string())
}

/// Until Solve IR carries an assigned state across a call (#686 step 1b),
/// the simulator refuses these models by name, before it lowers a row. It
/// must not run them: a lowering that ignored the carried state would read a
/// value that may not exist. The value and error-at-the-read expectations
/// for the simulator arrive with that step; the DAE tests below already pin
/// them on the checked DAE.
#[test]
fn the_simulator_refuses_a_carried_field_state_it_cannot_lower_yet() {
    let refusal = "Solve IR does not carry that state across a call yet";
    for reader in ["pick", "pick_statement", "relay", "wrong"] {
        let error = simulate("PassedOn", &PASSED_ON.replace("READER", reader))
            .expect_err("the Solve lowering has no call interface for an assigned state");
        assert!(error.contains(refusal), "{reader}: {error}");
        assert!(!error.contains("used without a value"), "{reader}: {error}");
    }
    // No call hands `make`'s fields on, so nothing is carried: binding the
    // record whole keeps main's check at the result, unchanged.
    let error = simulate("Bound", BOUND).expect_err("`r.b` is unwritten for a positive argument");
    assert!(error.contains("`r__b` is used without a value"), "{error}");
    assert!(!error.contains(refusal), "{error}");
}

/// The same functions with a parameter argument, so the checked DAE can be
/// evaluated on its own: the DAE owns where the field state is carried and
/// where it is checked, whatever consumes it afterwards.
fn static_source(reader: &str, argument: f64) -> String {
    PASSED_ON
        .replace("model PassedOn", "model Static")
        .replace("end PassedOn;", "end Static;")
        .replace(
            "Real y = READER(time - 0.25, make(time - 0.25));",
            &format!("parameter Real p = {argument};\n  Real y = {reader}(p, make(p));"),
        )
}

fn compile_dae(model: &str, source: &str) -> std::sync::Arc<rumoca_ir_dae::Dae> {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"))
        .dae
}

/// Evaluate the one call of `reader` in the checked DAE of `Static`.
fn evaluate_reader(reader: &str, argument: f64) -> Result<f64, String> {
    use rumoca_ir_dae as dae;
    compile_dae("Static", &static_source(reader, argument)).inspect(|view| {
        let call = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|&id| {
                matches!(
                    view.expression(id).expect("dense expression").operation(),
                    dae::ExpressionOperation::Call { function, .. }
                        if view
                            .function(function)
                            .expect("checked callee")
                            .name()
                            .as_str()
                            .ends_with(reader)
                )
            })
            .unwrap_or_else(|| panic!("the model calls {reader}"));
        rumoca_eval_dae::NumericEvaluator::new(view)
            .expression_values(call)
            .map(|values| values.materialize()[0])
            .map_err(|error| error.to_string())
    })
}

#[test]
fn the_dae_carries_the_field_states_to_the_reads_and_checks_them_there() {
    use rumoca_ir_dae as dae;
    compile_dae("Static", &static_source("pick", 0.75)).inspect(|view| {
        let function = |name: &str| {
            (0..view.function_count())
                .filter_map(|index| view.function(view.function_id(index)?))
                .find(|function| function.name().as_str().ends_with(name))
                .unwrap_or_else(|| panic!("{name} is constructed"))
        };
        let carried = function("make")
            .statements()
            .filter(|statement| {
                matches!(statement, dae::FunctionStatementView::CarryAssigned { .. })
            })
            .count();
        let checked = function("make")
            .statements()
            .filter(|statement| {
                matches!(
                    statement,
                    dae::FunctionStatementView::RequireAssigned { .. }
                )
            })
            .count();
        assert_eq!((carried, checked), (2, 0), "both field states are carried");
        assert_eq!(function("pick").absent_parameters(), &[1, 2]);
        assert!(function("make").absent_parameters().is_empty());
        let count = |wanted: fn(&dae::ExpressionOperation<'_>) -> bool| {
            (0..view.expression_count())
                .filter_map(|index| view.expression(view.expression_id(index)?))
                .filter(|node| wanted(&node.operation()))
                .count()
        };
        assert_eq!(
            count(|operation| matches!(operation, dae::ExpressionOperation::PassAssigned { .. })),
            2,
            "the call hands both fields on with their state"
        );
        assert_eq!(
            count(|operation| matches!(
                operation,
                dae::ExpressionOperation::AssignedParameter { checked: true, .. }
            )),
            2,
            "pick reads each field through the checked read"
        );
    });
}

#[test]
fn the_dae_evaluator_reads_the_field_the_executed_path_wrote() {
    for (reader, argument, expected) in [
        ("pick", 0.75, 0.75),
        ("pick", -0.5, 5.0),
        ("pick_statement", 0.75, 0.75),
        ("pick_statement", -0.5, 5.0),
        ("relay", 0.75, 1.75),
        ("relay", -0.5, 6.0),
        ("wrong", 0.25, 0.25),
    ] {
        let value = evaluate_reader(reader, argument)
            .unwrap_or_else(|error| panic!("{reader}({argument}): {error}"));
        assert!(
            (value - expected).abs() < 1e-12,
            "{reader}({argument}) = {value}, expected {expected}"
        );
    }
}

#[test]
fn the_dae_evaluator_fails_at_the_read_of_a_field_the_path_never_wrote() {
    // `make` wrote `a` for a positive argument and `b` otherwise; `wrong`
    // reads the other one.
    for (argument, unwritten) in [(0.75, "r_b"), (-0.5, "r_a")] {
        let error = evaluate_reader("wrong", argument)
            .expect_err("the field read was never written on this path");
        assert!(
            error.contains(&format!("`{unwritten}` is used without a value")),
            "wrong({argument}): {error}"
        );
    }
}

#[test]
fn the_template_context_projects_the_carried_field_states() {
    let dae = compile_dae("Static", &static_source("pick", 0.75));
    let context = rumoca_phase_codegen::dae_template_json(&dae).expect("the checked DAE projects");
    let expressions = context["expressions"]
        .as_array()
        .expect("the template context lists every expression");
    let of_kind = |kind: &str| {
        expressions
            .iter()
            .filter(|expression| expression["operation"]["kind"] == kind)
            .collect::<Vec<_>>()
    };
    let passes = of_kind("pass_assigned");
    assert_eq!(passes.len(), 2, "the call forwards both fields");
    assert!(
        passes
            .iter()
            .all(|pass| pass["operation"]["operand"].is_u64()),
        "{passes:?}"
    );
    let reads = of_kind("assigned_parameter");
    assert_eq!(reads.len(), 2, "pick reads each field");
    assert!(
        reads.iter().all(|read| read["operation"]["checked"] == true
            && read["operation"]["function"].is_u64()
            && read["operation"]["ordinal"].is_u64()),
        "{reads:?}"
    );
}

/// A state demoted by a constraint rebuilds the whole DAE, functions
/// included: `make` keeps carrying its two field states unchecked.
const DEMOTED: &str = r#"
model Demoted
  record R
    Real a;
    Real b;
  end R;
  function make
    input Real x;
    output R r;
  algorithm
    if x > 0 then
      r.a := x;
    else
      r.b := -x;
    end if;
  end make;
  function first
    input R r;
    output Real y;
  algorithm
    y := 2*r.a;
  end first;
  parameter Real p = 1.5;
  parameter Real k = first(make(p));
  Real x;
  Real v;
equation
  der(x) = v;
  x = sin(time)*k;
end Demoted;
"#;

fn carried_statements(dae: &rumoca_ir_dae::Dae, name: &str) -> usize {
    use rumoca_ir_dae as dae;
    dae.inspect(|view| {
        (0..view.function_count())
            .filter_map(|index| view.function(view.function_id(index)?))
            .find(|function| function.name().as_str().ends_with(name))
            .unwrap_or_else(|| panic!("{name} is constructed"))
            .statements()
            .filter(|statement| {
                matches!(statement, dae::FunctionStatementView::CarryAssigned { .. })
            })
            .count()
    })
}

#[test]
fn index_reduction_rebuilds_a_function_that_carries_field_states() {
    let dae = compile_dae("Demoted", DEMOTED);
    assert_eq!(carried_statements(&dae, "make"), 2);
    let prepared = rumoca_phase_structural::prepare_for_solve(&dae)
        .unwrap_or_else(|error| panic!("the constraint demotes x: {error}"));
    assert!(
        !std::ptr::eq(prepared.as_dae(), &*dae),
        "the demotion rebuilt the DAE"
    );
    assert_eq!(carried_statements(prepared.as_dae(), "make"), 2);
}

/// Differentiation admits no carried field state, so a constraint whose
/// derivative would pass through one is not reduced: it is refused.
#[test]
fn a_constraint_through_a_carried_field_state_is_not_differentiated() {
    let source = DEMOTED
        .replace("  parameter Real k = first(make(p));\n", "")
        .replace("x = sin(time)*k;", "x = sin(time)*first(make(p));");
    let dae = compile_dae("Demoted", &source);
    let error = match rumoca_phase_structural::prepare_for_solve(&dae) {
        Ok(_) => panic!("the constraint differentiates through a carried field state"),
        Err(error) => error.to_string(),
    };
    assert!(error.contains("structurally singular"), "{error}");
}
