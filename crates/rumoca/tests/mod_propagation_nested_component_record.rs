fn flat_expr_is_numeric_value(expr: &rumoca_core::Expression, expected: i64) -> bool {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => *value == expected,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } => (*value - expected as f64).abs() <= f64::EPSILON,
        _ => false,
    }
}

fn flat_expr_mentions_name(expr: &rumoca_core::Expression, needle: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            name.as_str().contains(needle)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        flat_expr_mentions_name(expr, needle)
                    }
                    _ => false,
                })
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            field.contains(needle) || flat_expr_mentions_name(base, needle)
        }
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            name.as_str().contains(needle)
                || args.iter().any(|arg| flat_expr_mentions_name(arg, needle))
        }
        rumoca_core::Expression::Array { elements, .. } => elements
            .iter()
            .any(|element| flat_expr_mentions_name(element, needle)),
        _ => false,
    }
}

#[test]
fn test_nested_component_record_modifier_resolves_sibling_alias_scope() {
    let source = r#"
        record CoreParameters
            parameter Real p = 1;
        end CoreParameters;

        record Data
            parameter CoreParameters core;
        end Data;

        model Core
            parameter CoreParameters coreParameters;
            parameter Real use = coreParameters.p;
        end Core;

        partial model PartialMachine
            parameter CoreParameters coreParameters;
            Core core(final coreParameters = coreParameters);
        end PartialMachine;

        model Machine
            extends PartialMachine;
        end Machine;

        model Motor
            parameter Data data;
            Machine machine(coreParameters = data.core);
        end Motor;

        model Top
            parameter Data data(core(p = 3));
            Motor motor(data = data);
        end Top;
    "#;

    let compiled = rumoca::Compiler::new()
        .model("Top")
        .compile_str(source, "test.mo")
        .expect("Top should compile");

    let binding = compiled
        .flat
        .variables
        .iter()
        .find(|(name, _)| name.as_str() == "motor.machine.core.coreParameters.p")
        .and_then(|(_, var)| var.binding.as_ref())
        .expect("motor.machine.core.coreParameters.p should have binding");

    assert!(
        !flat_expr_mentions_name(binding, "motor.machine.data.core"),
        "inherited nested record modifier must not be scoped under the machine component; binding={binding:?}"
    );
    assert!(
        !flat_expr_mentions_name(binding, "motor.machine.coreParameters"),
        "inherited nested record modifier must not leave an intermediate record alias for DAE lowering; binding={binding:?}"
    );
    assert!(
        flat_expr_mentions_name(binding, "motor.data.core")
            || flat_expr_mentions_name(binding, "data.core")
            || flat_expr_is_numeric_value(binding, 3),
        "inherited nested record modifier should resolve through the outer sibling alias scope; binding={binding:?}"
    );

    rumoca_phase_dae::to_dae(&compiled.flat).expect("Top should lower to DAE");
}
