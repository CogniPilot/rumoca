//! STRUCT-T10(b): annotated straight-line calls are replaced by their
//! substituted bodies, before index reduction for `Inline`/`LateInline` and
//! after formal-derivative construction for `InlineAfterIndexReduction`, while
//! every call the profile does not admit stays a call.

use rumoca::Compiler;
use rumoca_ir_dae as dae;
use rumoca_phase_structural::inline_annotated_calls;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

fn compile(source: &str, name: &str) -> std::sync::Arc<dae::Dae> {
    Compiler::new()
        .model(name)
        .compile_str(source, "inline_calls.mo")
        .unwrap()
        .dae
}

/// Names of the functions a model-level expression still calls.
fn called_functions(model: &dae::Dae) -> Vec<String> {
    model.inspect(|view| {
        let mut names = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .filter(|node| node.function_scope().is_none())
            .filter_map(|node| match node.operation() {
                dae::ExpressionOperation::Call { function, .. } => {
                    Some(view.function(function).unwrap().name().to_string())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        names.sort();
        names.dedup();
        names
    })
}

const PROFILE: &str = r#"
package InlineProfile
  function axisScale
    input Real e[3];
    input Real s;
    output Real y[3];
  algorithm
    y := e * s;
    annotation(Inline = true);
  end axisScale;
  function late
    input Real x;
    output Real y;
  algorithm
    y := 2 * x;
    annotation(LateInline = true);
  end late;
  function supplied
    input Real x;
    output Real y;
  algorithm
    y := sin(x);
    annotation(Inline = true, derivative = supplied_d);
  end supplied;
  function supplied_d
    input Real x;
    input Real dx;
    output Real dy;
  algorithm
    dy := cos(x) * dx;
  end supplied_d;
  function kinked
    input Real x;
    output Real y;
  algorithm
    y := abs(x);
    annotation(Inline = true);
  end kinked;
  function stepped
    input Real x;
    output Real y;
  protected
    Real z;
  algorithm
    z := x + 1;
    y := 2 * z;
    annotation(Inline = true);
  end stepped;
  function checked
    input Real x;
    output Real y;
  algorithm
    assert(x > -10, "x stays above -10");
    y := x;
    annotation(Inline = true);
  end checked;
  function refused
    input Real x;
    output Real y;
  algorithm
    y := 3 * x;
    annotation(Inline = false);
  end refused;
  model M
    Real x(start = 1, fixed = true);
    Real v[3];
    Real a;
    Real b;
    Real c;
    Real d;
    Real f;
    Real g;
  equation
    v = axisScale({1, 0, 0}, x);
    a = late(x);
    b = supplied(x);
    c = kinked(x);
    d = stepped(x);
    f = checked(x);
    g = refused(x);
    der(x) = -v[1];
  end M;
end InlineProfile;"#;

#[test]
fn only_admitted_annotated_calls_are_inlined_before_index_reduction() {
    let source = compile(PROFILE, "InlineProfile.M");
    let before = called_functions(&source);
    for name in [
        "axisScale",
        "late",
        "supplied",
        "kinked",
        "stepped",
        "checked",
        "refused",
    ] {
        assert!(
            before.iter().any(|called| called.ends_with(name)),
            "{name}: {before:?}"
        );
    }
    let Some(inlined) = inline_annotated_calls(&source).unwrap() else {
        panic!("axisScale and late are admitted");
    };
    let after = called_functions(&inlined);
    for name in ["axisScale", "late"] {
        assert!(
            !after.iter().any(|called| called.ends_with(name)),
            "{name} is inlined: {after:?}"
        );
    }
    for name in ["supplied", "kinked", "stepped", "checked", "refused"] {
        assert!(
            after.iter().any(|called| called.ends_with(name)),
            "{name} stays a call: {after:?}"
        );
    }
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
        let x = (-time).exp();
        for (name, expected) in [
            ("v[1]", x),
            ("v[2]", 0.0),
            ("a", 2.0 * x),
            ("b", x.sin()),
            ("d", 2.0 * (x + 1.0)),
            ("g", 3.0 * x),
        ] {
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-4,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}

const CIRCLE: &str = r#"
package InlineCircle
  function norm2
    input Real q[2];
    output Real y;
  algorithm
    y := q * q;
    annotation(InlineAfterIndexReduction = true);
  end norm2;
  model M
    Real q[2](start = {1, 0});
    Real v[2](start = {0, 1});
    Real lambda;
  initial equation
    q[2] = 0;
    v[2] = 1;
  equation
    der(q) = v;
    der(v) = lambda * q;
    1 = norm2(q);
  end M;
end InlineCircle;"#;

#[test]
fn an_inline_after_index_reduction_call_is_differentiated_before_it_is_inlined() {
    let source = compile(CIRCLE, "InlineCircle.M");
    assert!(
        inline_annotated_calls(&source).unwrap().is_none(),
        "InlineAfterIndexReduction is not inlined before index reduction"
    );
    let lowered =
        rumoca_phase_solve::lower_solve_model(&source, &std::collections::HashMap::new(), |_| {})
            .unwrap();
    let prepared = called_functions(lowered.prepared_dae());
    assert!(
        !prepared.iter().any(|called| called.ends_with("norm2")),
        "the prepared system reads the substituted body: {prepared:?}"
    );
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
        for (name, expected) in [("q[1]", time.cos()), ("q[2]", time.sin())] {
            let actual = column(name)[row];
            assert!(
                (actual - expected).abs() < 1e-3,
                "{name} at {time}: {actual} != {expected}"
            );
        }
    }
}

const MUTUAL: &str = r#"
package InlineMutual
  function even
    input Real x;
    output Real y;
  algorithm
    y := odd(x);
    annotation(Inline = true);
  end even;
  function odd
    input Real x;
    output Real y;
  algorithm
    y := if x > 1 then even(x - 1) else x;
    annotation(Inline = true);
  end odd;
  function plain
    input Real x;
    output Real y;
  algorithm
    y := 2 * x;
    annotation(Inline = true);
  end plain;
  model M
    Real x(start = 1, fixed = true);
    Real a;
    Real b;
  equation
    der(x) = -x;
    a = even(x);
    b = plain(x);
  end M;
end InlineMutual;"#;

#[test]
fn a_mutually_recursive_straight_line_function_is_not_inlined() {
    let source = compile(MUTUAL, "InlineMutual.M");
    let Some(inlined) = inline_annotated_calls(&source).unwrap() else {
        panic!("plain is admitted");
    };
    let after = called_functions(&inlined);
    assert!(
        after.iter().any(|called| called.ends_with("even")),
        "even recurses through odd and stays a call: {after:?}"
    );
    assert!(
        !after.iter().any(|called| called.ends_with("plain")),
        "plain is inlined: {after:?}"
    );
}
