//! Record-modification forwarding through nested components.
//!
//! A record modification applied at the top of a hierarchy has to reach every
//! *dependent* field of every forwarded copy of that record, not just the
//! field that was modified.

use super::*;

#[test]
fn test_record_forwarding_rebinds_dependent_record_fields() {
    let mut session = Session::default();
    session
        .add_document(
            "test.mo",
            r#"
                package P
                  record R
                    parameter Real a = 2;
                    final parameter Real b = a;
                  end R;

                  model Inner
                    parameter R r;
                    parameter Real x = r.b;
                  end Inner;

                  model Mid
                    parameter R r;
                    Inner i(r = r);
                  end Mid;

                  model Top
                    parameter R r(a = 5);
                    Mid mid(r = r);
                  end Top;
                end P;
                "#,
        )
        .unwrap();

    let result = session.compile_model("P.Top").unwrap();

    // `R.b` is `final`, so it keeps its declaration equation `b = a` and takes
    // the modified `a` (MLS 3.7 §7.2.6). `mid(r = r)` and `i(r = r)` are
    // binding equations on a record component, so each forwarded record equals
    // the source record component-wise (MLS 3.7 §7.2, §4.4.2.1). Every
    // dependent field must therefore carry the overridden `a = 5`, not the
    // record default `a = 2`.
    for name in [
        "r.b",
        "mid.r.b",
        "mid.i.r.b",
        // `Inner.x = r.b` reads the dependent field through the forwarded record.
        "mid.i.x",
    ] {
        let value = resolved_parameter_value(&result.dae, name);
        assert!(
            (value - 5.0).abs() <= f64::EPSILON,
            "record forwarding must propagate dependent field b via overridden a: \
             {name} resolved to {value}, expected 5"
        );
    }
}

/// Resolve a DAE parameter binding to its numeric value, following
/// parameter-to-parameter references.
///
/// A parameter binding may be any parameter expression (MLS 3.7 §3.8), and
/// parameter bindings are evaluated in dependency order before the
/// initialization problem is solved (MLS 3.7 §8.6), so a forwarded record field
/// may legitimately stay bound to the source field rather than being folded to
/// a literal. Asserting on the resolved value keeps this test on the language
/// semantics instead of on how far constant folding happened to get.
fn resolved_parameter_value(dae: &dae::Dae, name: &str) -> f64 {
    dae.inspect(|view| {
        let mut current = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == name)
            .map(|(id, _)| id)
            .unwrap_or_else(|| panic!("{name} must exist in checked DAE parameters"));
        for _ in 0..16 {
            let variable = view
                .variable(current)
                .expect("branded parameter identity resolves");
            let binding = variable
                .binding()
                .or_else(|| variable.start())
                .unwrap_or_else(|| panic!("{} must have a binding", variable.name()));
            match view
                .expression(binding)
                .expect("branded binding expression resolves")
                .operation()
            {
                dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => {
                    return *value as f64;
                }
                dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => {
                    return *value;
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) => {
                    current = view
                        .variables()
                        .find(|(variable_id, _)| variable_id.index() == id.index())
                        .map(|(variable_id, _)| variable_id)
                        .expect("parameter coordinate resolves to its declaration");
                }
                _ => panic!(
                    "{} binding is neither a numeric literal nor a scalar parameter reference",
                    variable.name()
                ),
            }
        }
        panic!("parameter binding chain starting at {name} did not terminate")
    })
}
