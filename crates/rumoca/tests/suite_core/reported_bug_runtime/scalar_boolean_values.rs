//! EXPR-042 value semantics; activation conditions retain their separate owner.
use super::*;

fn assert_value(source: &str, model: &str, cases: &[(f64, f64)]) {
    let compiled = compile(source, model);
    assert_dae_value(&compiled.dae, model, cases);
}

fn assert_dae_value(dae: &rumoca_ir_dae::Dae, model: &str, cases: &[(f64, f64)]) {
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        for &(input, expected) in cases {
            let mut opts = options(policy);
            opts.initial_inputs = vec![("k".into(), input)];
            let result = simulate_dae(dae, &opts)
                .unwrap_or_else(|error| panic!("{model}/{policy:?}/k={input}: {error:?}"));
            assert_eq!(last(&result, "y"), expected, "{model}/{policy:?}/k={input}");
        }
    }
}

#[test]
fn scalar_boolean_or_skips_a_dynamic_invalid_index() {
    let source = GUARDED.replace("k <= n and", "k > n or");
    assert_value(&source, "GuardedOperands", &[(4., 1.), (3., 1.), (2., 0.)]);
}

#[test]
fn scalar_boolean_selected_asserting_call_keeps_its_effect() {
    let source = GUARDED.replace("a[k] > 5", "checked(k)").replace(
        "  function guarded",
        "  function checked
    input Integer k;
    output Boolean ok;
  algorithm
    assert(k > 1, \"selected Boolean call\");
    ok := true;
  end checked;
  function guarded",
    );
    assert!(source.contains("function checked"));
    assert_value(&source, "GuardedOperands", &[(4., 0.), (2., 1.)]);
    let compiled = compile(&source, "GuardedOperands");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("k".into(), 1.)];
        let error =
            simulate_dae(&compiled.dae, &opts).expect_err("the selected call assertion executes");
        assert!(
            format!("{error:?}").contains("selected Boolean call"),
            "{error:?}"
        );
    }
}

#[test]
fn scalar_boolean_skipping_does_not_hide_an_independently_demanded_index() {
    let source = GUARDED.replace(
        "if k <= n and a[k] > 5 then 1 else 0",
        "(if k <= n and a[k] > 5 then 1 else 0) + a[k]",
    );
    let compiled = compile(&source, "GuardedOperands");
    for policy in [SimExecutionPolicy::Interpreter, SimExecutionPolicy::Auto] {
        let mut opts = options(policy);
        opts.initial_inputs = vec![("k".into(), 4.)];
        assert_index_fault(
            simulate_dae(&compiled.dae, &opts).expect_err("independent access faults"),
        );
    }
}

#[test]
fn scalar_boolean_index_of_shape_one_remains_a_scalar_value() {
    let source = GUARDED
        .replace("a[3]", "a[1]")
        .replace("{1, 2, 9}", "{9}")
        .replace("k, 3);", "k, 1);");
    assert_value(&source, "GuardedOperands", &[(1., 1.), (2., 0.)]);
}

#[test]
fn ordinary_model_boolean_value_issues_guarded_arms_and_replays() {
    let source = "model ModelBooleanValue
 input Integer k(start=4);
 Real a[3](start={1,2,9}, each fixed=true);
 Real y;
 equation
 der(a)=zeros(3);
 y=if k<=3 and a[k]>5 then 1 else 0;
end ModelBooleanValue;";
    let compiled = compile(source, "ModelBooleanValue");
    let json = serde_json::to_vec(&compiled.dae).unwrap();
    let replayed: rumoca_ir_dae::Dae = serde_json::from_slice(&json).unwrap();
    assert_eq!(json, serde_json::to_vec(&replayed).unwrap());
    for (label, dae) in [
        ("source", compiled.dae.as_ref()),
        ("JSON replay", &replayed),
    ] {
        dae.inspect(|view| {
            assert!(view.model_conditional_count() >= 2);
            assert!(
                view.roots().any(|(_, root)| {
                    let expression = view.relation(root.relation()).unwrap().expression();
                    view.model_activation_guard(expression).is_some()
                }),
                "the actual RHS state relation retains its issued arm"
            );
        });
        assert_dae_value(dae, label, &[(4., 0.), (3., 1.), (2., 0.)]);
    }
}

#[test]
fn scalar_boolean_nested_and_or_keep_both_truth_tables() {
    for (operator, expected) in [("and", [0., 0., 0., 1.]), ("or", [0., 1., 1., 1.])] {
        let source = GUARDED.replace(
            "k <= n and a[k] > 5",
            &format!("(k == 1 or k == 3) {operator} k >= 2"),
        );
        let cases = expected
            .into_iter()
            .enumerate()
            .map(|(input, value)| (input as f64, value))
            .collect::<Vec<_>>();
        assert_value(&source, "GuardedOperands", &cases);
    }
}
