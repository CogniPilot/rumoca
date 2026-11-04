use rumoca::Compiler;
use rumoca_ir_dae as dae;

fn check_branches(declaration: &str, activation: &str, body: &str, expected: &[dae::DaeLiteral]) {
    let source = format!(
        "model NestedBranches\ninput Boolean u; input Boolean v; input Boolean w;\n\
         {declaration}\nalgorithm\nwhen {activation} then\n{body}\nend when;\n\
         end NestedBranches;"
    );
    let compiled = Compiler::new()
        .model("NestedBranches")
        .compile_str(&source, "NestedBranches.mo")
        .unwrap();
    compiled.dae.inspect(|view| {
        let owner = (0..view.discrete_value_owner_count())
            .map(|index| {
                view.discrete_value_owner(view.discrete_value_owner_id(index).unwrap())
                    .unwrap()
            })
            .find(|owner| {
                owner.targets().iter().any(|target| {
                    view.variable(target.variable().into())
                        .unwrap()
                        .name()
                        .as_str()
                        == "x"
                })
            })
            .unwrap();
        assert_eq!(owner.targets().len(), 1);
        let mut actual = Vec::new();
        for (index, branch) in owner.branches().iter().enumerate() {
            let dae::DiscreteBranchActivation::When { trigger, guard } = branch.activation() else {
                panic!("an event branch retains its activation");
            };
            assert_eq!(
                trigger == guard,
                index + 1 == owner.branches().len(),
                "the enclosing default follows every nested override"
            );
            let (value, _) = branch.values().get(0).unwrap();
            let dae::ExpressionOperation::Literal(value) =
                view.expression(value).unwrap().operation()
            else {
                panic!("these assignments have literal final values");
            };
            actual.push(value.clone());
        }
        assert_eq!(actual, expected);
    });
    rumoca_phase_solve::lower_solve_model(
        &compiled.dae,
        &std::collections::HashMap::from([("u".into(), 1.0), ("v".into(), 1.0), ("w".into(), 1.0)]),
        |_| {},
    )
    .unwrap();
}

#[test]
fn nested_integer_override_precedes_its_clocked_and_unclocked_default() {
    for activation in ["sample(0, 1)", "time > 1"] {
        check_branches(
            "discrete Integer x(start=0, fixed=true);",
            activation,
            "x := 0; if u then if v then x := 1; end if; end if;",
            &[dae::DaeLiteral::Integer(1), dae::DaeLiteral::Integer(0)],
        );
    }
}

#[test]
fn nested_else_override_preserves_boolean_values() {
    check_branches(
        "discrete Boolean x(start=false, fixed=true);",
        "time > 1",
        "x := false; if u then else if v then x := true; end if; end if;",
        &[
            dae::DaeLiteral::Boolean(true),
            dae::DaeLiteral::Boolean(false),
        ],
    );
}

#[test]
fn later_enclosing_assignment_overrides_a_nested_definition() {
    check_branches(
        "discrete Integer x(start=0, fixed=true);",
        "time > 1",
        "x := 0; if u then if v then x := 1; end if; end if; x := 2;",
        &[dae::DaeLiteral::Integer(2), dae::DaeLiteral::Integer(2)],
    );
}

#[test]
fn ancestry_crosses_multiple_branches_without_direct_assignments() {
    check_branches(
        "discrete Integer x(start=0, fixed=true);",
        "time > 1",
        "x := 0; if u then if v then if w then x := 3; end if; end if; end if;",
        &[dae::DaeLiteral::Integer(3), dae::DaeLiteral::Integer(0)],
    );
}
