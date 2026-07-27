//! Regression coverage for if-equations whose branches disagree about which
//! variables they differentiate (MLS §8.3.4).
//!
//! `Modelica.Thermal.FluidHeatFlow.BaseClasses.TwoPort` writes its energy
//! balance as
//!
//! ```modelica
//! if m > Modelica.Constants.small then
//!   flowPort_a.H_flow + flowPort_b.H_flow + Q_flow = m*medium.cv*der(T);
//! else
//!   flowPort_a.H_flow + flowPort_b.H_flow + Q_flow = 0;
//! end if;
//! ```
//!
//! Both branches hold exactly one equation, so the count-matching fast path used
//! to merge them into a single residual carrying an `if`-expression. `der(T)`
//! then still appeared syntactically in the residual, so `T` was selected as a
//! state even for components declared with `m = 0` — and the taken branch never
//! assigns `der(T)`, which surfaced at run time as
//! `EX001 non-finite derivative evaluation for state 'pump.T'` across the
//! `Modelica.Thermal.FluidHeatFlow.Examples` family.
//!
//! The condition is a parameter expression, so it must be resolved during
//! flattening and only the taken branch emitted.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

fn flatten_source(source: &str, model: &str) -> flat::Model {
    let file_name = "<parameter_conditional_der_branch>";
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("source parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("source resolves");
    let instanced =
        rumoca_phase_instantiate::instantiate(resolved, model).expect("model instantiates");
    rumoca_phase_flatten::flatten_ref(instanced.inner(), instanced.overlay(), model)
        .expect("model flattens")
}

/// Every equation residual, rendered.
fn residuals(model: &flat::Model) -> Vec<String> {
    model
        .equations
        .iter()
        .map(|eq| format!("{:?}", eq.residual))
        .collect()
}

const SOURCE: &str = r#"
package P
  partial model TwoPort
    parameter Real m = 1 "Mass of medium";
    parameter Real cv = 4000;
    Real T(start = 300);
    Real H_flow;
  equation
    H_flow = 10;
    if m > 1e-60 then
      H_flow = m*cv*der(T);
    else
      H_flow = 0;
    end if;
  end TwoPort;

  model MasslessPort
    extends TwoPort(m = 0);
  end MasslessPort;

  model MassivePort
    extends TwoPort(m = 2);
  end MassivePort;
end P;
"#;

#[test]
fn zero_mass_branch_drops_the_derivative_equation() {
    let model = flatten_source(SOURCE, "P.MasslessPort");
    let residuals = residuals(&model);

    assert!(
        residuals
            .iter()
            .all(|residual| !residual.contains("function: Der")),
        "m = 0 selects the algebraic branch, so no der() may survive: {residuals:?}"
    );
}

#[test]
fn nonzero_mass_branch_keeps_the_derivative_equation() {
    let model = flatten_source(SOURCE, "P.MassivePort");
    let residuals = residuals(&model);

    assert!(
        residuals
            .iter()
            .any(|residual| residual.contains("function: Der")),
        "m = 2 selects the dynamic branch, which must keep der(T): {residuals:?}"
    );
}

/// Branches that differentiate the same variables stay mergeable: nothing here
/// forces the condition to be resolved during flattening.
#[test]
fn matching_der_targets_do_not_force_branch_selection() {
    const SAME_DER: &str = r#"
package Q
  model Both
    parameter Real k = 0;
    Real x(start = 1);
    Real u;
  equation
    u = 3;
    if k > 0.5 then
      der(x) = u;
    else
      der(x) = -u;
    end if;
  end Both;
end Q;
"#;
    let model = flatten_source(SAME_DER, "Q.Both");
    let residuals = residuals(&model);

    assert!(
        residuals
            .iter()
            .any(|residual| residual.contains("function: Der")),
        "der(x) is defined in both branches: {residuals:?}"
    );
}
