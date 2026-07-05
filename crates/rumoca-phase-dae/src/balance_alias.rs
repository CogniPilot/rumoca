use super::{BalanceSymbolSet, eq_binary_var_refs};
use rumoca_ir_dae as dae;

pub(super) fn is_vector_forwarding_alias(
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    output_names: &BalanceSymbolSet,
    component_defined_targets: &BalanceSymbolSet,
) -> bool {
    if eq.scalar_count <= 1 {
        return false;
    }
    if !(eq.origin.starts_with("binding equation for") || eq.origin.starts_with("equation from ")) {
        return false;
    }
    let Some((lhs, rhs)) = eq_binary_lhs_rhs(&eq.rhs) else {
        return false;
    };
    let Some(rhs_name) = forwarded_value_ref(rhs) else {
        return false;
    };
    let rumoca_core::Expression::VarRef { name: lhs_name, .. } = lhs else {
        return false;
    };
    output_names.matches_reference(rhs_name)
        || !continuous_unknowns.matches_reference(lhs_name)
        || component_defined_targets.matches_reference(lhs_name)
}

fn forwarded_value_ref(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => Some(name),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            ..
        } => {
            let rumoca_core::Expression::VarRef { name, .. } = args.first()? else {
                return None;
            };
            Some(name)
        }
        _ => None,
    }
}

pub(super) fn is_absent_lhs_component_alias(
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    input_names: &BalanceSymbolSet,
) -> bool {
    let Some(lhs) = eq_lhs_var_ref(&eq.rhs) else {
        return false;
    };
    !continuous_unknowns.names.contains(lhs.var_name())
        && !input_names.names.contains(lhs.var_name())
}

pub(super) fn is_non_constraining_binding_alias(
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    output_names: &BalanceSymbolSet,
    component_defined_targets: &BalanceSymbolSet,
) -> bool {
    let refs = eq_binary_var_refs(&eq.rhs);
    let [lhs, rhs] = refs.as_slice() else {
        return false;
    };
    let lhs_is_continuous_unknown = continuous_unknowns.matches_reference(lhs);
    let rhs_is_continuous_unknown = continuous_unknowns.matches_reference(rhs);
    let lhs_is_component_defined = component_defined_targets.matches_reference(lhs);
    if eq.scalar_count > 1 && (output_names.matches_reference(lhs) || lhs_is_component_defined) {
        return true;
    }
    if !lhs_is_continuous_unknown {
        return true;
    }
    if !rhs_is_continuous_unknown {
        return lhs_is_component_defined;
    }
    lhs_is_component_defined && component_defined_targets.matches_reference(rhs)
}

pub(super) fn is_input_forwarding_connection_alias(
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    input_names: &BalanceSymbolSet,
) -> bool {
    let refs = eq_binary_var_refs(&eq.rhs);
    let [lhs, rhs] = refs.as_slice() else {
        return false;
    };
    let lhs_is_input = input_names.matches_reference(lhs);
    let rhs_is_input = input_names.matches_reference(rhs);
    let lhs_is_continuous_unknown = continuous_unknowns.matches_reference(lhs);
    let rhs_is_continuous_unknown = continuous_unknowns.matches_reference(rhs);
    (lhs_is_input && !rhs_is_continuous_unknown) || (rhs_is_input && !lhs_is_continuous_unknown)
}

fn eq_lhs_var_ref(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        ..
    } = expr
    else {
        return None;
    };
    let rumoca_core::Expression::VarRef { name, .. } = lhs.as_ref() else {
        return None;
    };
    Some(name)
}

fn eq_binary_lhs_rhs(
    expr: &rumoca_core::Expression,
) -> Option<(&rumoca_core::Expression, &rumoca_core::Expression)> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return None;
    };
    Some((lhs, rhs))
}

#[cfg(test)]
mod tests {
    use super::super::{BalanceResult, balance};
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("balance_alias_fixture.mo"),
            1,
            2,
        )
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: test_span(),
        }
    }

    fn binary_eq(lhs_name: &str, rhs_name: &str, origin: &str) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref(lhs_name)),
                rhs: Box::new(var_ref(rhs_name)),
                span: test_span(),
            },
            span: test_span(),
            origin: origin.to_string(),
            scalar_count: 1,
        }
    }

    fn vector_binary_eq(
        lhs_name: &str,
        rhs_name: &str,
        origin: &str,
        count: usize,
    ) -> dae::Equation {
        dae::Equation {
            scalar_count: count,
            ..binary_eq(lhs_name, rhs_name, origin)
        }
    }

    fn vector_fill_eq(lhs_name: &str, rhs_name: &str, origin: &str, count: usize) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref(lhs_name)),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Fill,
                    args: vec![
                        var_ref(rhs_name),
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Integer(count as i64),
                            span: test_span(),
                        },
                    ],
                    span: test_span(),
                }),
                span: test_span(),
            },
            span: test_span(),
            origin: origin.to_string(),
            scalar_count: count,
        }
    }

    fn scalar_input(name: &str) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            ..rumoca_ir_dae::Variable::empty_with_span(test_span())
        }
    }

    fn algebraic_var(name: &str) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            ..rumoca_ir_dae::Variable::empty_with_span(test_span())
        }
    }

    fn vector_algebraic_var(name: &str, size: i64) -> dae::Variable {
        dae::Variable {
            dims: vec![size],
            ..algebraic_var(name)
        }
    }

    fn vector_output_var(name: &str, size: i64) -> dae::Variable {
        dae::Variable {
            dims: vec![size],
            ..algebraic_var(name)
        }
    }

    fn balance_value(dae: &dae::Dae) -> BalanceResult<i64> {
        balance(dae)
    }

    #[test]
    fn balance_skips_binding_alias_to_absent_public_alias() {
        let mut dae = dae::Dae::default();
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("x"), algebraic_var("x"));
        dae.continuous
            .equations
            .push(binary_eq("x", "source", "component equation"));
        dae.continuous.equations.push(binary_eq(
            "publicAlias",
            "x",
            "binding equation for publicAlias",
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_binding_alias_between_component_defined_targets() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), algebraic_var(name));
            dae.continuous
                .equations
                .push(binary_eq(name, "source", "component equation"));
        }
        dae.continuous
            .equations
            .push(binary_eq("a", "b", "binding equation for a"));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_counts_binding_from_parameter_to_undefined_unknown() {
        let mut dae = dae::Dae::default();
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("secret"), algebraic_var("secret"));
        dae.variables
            .parameters
            .insert(rumoca_core::VarName::new("k"), algebraic_var("k"));
        dae.continuous
            .equations
            .push(binary_eq("secret", "k", "binding equation for secret"));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_binding_alias_from_output_projection_when_lhs_is_defined() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            rumoca_core::VarName::new("driver"),
            vector_output_var("driver", 3),
        );
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("alias"),
            vector_algebraic_var("alias", 3),
        );

        dae.continuous.equations.push(vector_binary_eq(
            "driver",
            "source",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "alias",
            "external",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "alias",
            "driver",
            "binding equation for alias",
            3,
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_vector_binding_alias_to_public_output_projection() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            rumoca_core::VarName::new("public"),
            vector_output_var("public", 3),
        );
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("internal"),
            vector_algebraic_var("internal", 3),
        );

        dae.continuous.equations.push(vector_binary_eq(
            "internal",
            "source",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "public",
            "external",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "public",
            "internal",
            "binding equation for public",
            3,
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_internal_component_input_forwarding_without_continuous_unknown() {
        let mut dae = dae::Dae::default();
        dae.variables.inputs.insert(
            rumoca_core::VarName::new("replicator.u"),
            scalar_input("replicator.u"),
        );
        dae.continuous.equations.push(binary_eq(
            "replicator.y",
            "replicator.u",
            "equation from replicator",
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_connection_input_forwarding_to_missing_alias() {
        let mut dae = dae::Dae::default();
        dae.variables
            .inputs
            .insert(rumoca_core::VarName::new("sink.u"), scalar_input("sink.u"));
        dae.continuous.equations.push(binary_eq(
            "source.y",
            "sink.u",
            "connection equation: source.y = sink.u",
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_vector_forwarding_aliases() {
        let mut dae = dae::Dae::default();
        for name in ["alias", "source", "filled"] {
            dae.variables.algebraics.insert(
                rumoca_core::VarName::new(name),
                vector_algebraic_var(name, 3),
            );
        }
        dae.continuous.equations.push(vector_binary_eq(
            "source",
            "driver",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "alias",
            "aliasDriver",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "filled",
            "filledDriver",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "alias",
            "source",
            "binding equation for alias",
            3,
        ));
        dae.continuous.equations.push(vector_fill_eq(
            "filled",
            "source",
            "equation from replicator",
            3,
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_vector_forwarding_from_output_projection() {
        let mut dae = dae::Dae::default();
        dae.variables.outputs.insert(
            rumoca_core::VarName::new("driver"),
            vector_output_var("driver", 3),
        );
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("alias"),
            vector_algebraic_var("alias", 3),
        );

        dae.continuous.equations.push(vector_binary_eq(
            "driver",
            "source",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "alias",
            "external",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "alias",
            "driver",
            "equation from adaptor",
            3,
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_counts_vector_pass_through_from_non_output_driver() {
        let mut dae = dae::Dae::default();
        dae.variables
            .outputs
            .insert(rumoca_core::VarName::new("y"), vector_output_var("y", 3));
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("u"), vector_algebraic_var("u", 3));

        dae.continuous
            .equations
            .push(vector_binary_eq("u", "source", "component equation", 3));
        dae.continuous
            .equations
            .push(vector_binary_eq("y", "u", "equation from passThrough", 3));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_connection_alias_between_component_defined_vectors() {
        let mut dae = dae::Dae::default();
        for name in ["sensor.v", "adaptor.v"] {
            dae.variables.algebraics.insert(
                rumoca_core::VarName::new(name),
                vector_algebraic_var(name, 3),
            );
        }

        dae.continuous.equations.push(vector_binary_eq(
            "sensor.v",
            "sensorSource",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "adaptor.v",
            "adaptorSource",
            "component equation",
            3,
        ));
        dae.continuous.equations.push(vector_binary_eq(
            "sensor.v",
            "adaptor.v",
            "connection equation: sensor.v = adaptor.v",
            3,
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn component_defined_targets_include_second_binary_ref_when_it_is_the_only_unknown() {
        let mut dae = dae::Dae::default();
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("y"), algebraic_var("y"));
        dae.variables
            .inputs
            .insert(rumoca_core::VarName::new("u"), scalar_input("u"));
        dae.variables.inputs.insert(
            rumoca_core::VarName::new("external"),
            scalar_input("external"),
        );

        dae.continuous
            .equations
            .push(binary_eq("u", "y", "component equation"));
        dae.continuous
            .equations
            .push(binary_eq("y", "external", "connect(y, external)"));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn component_defined_targets_do_not_treat_two_unknown_residual_as_two_definitions() {
        let mut dae = dae::Dae::default();
        for name in ["a", "b"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), algebraic_var(name));
        }
        dae.variables.inputs.insert(
            rumoca_core::VarName::new("external"),
            scalar_input("external"),
        );

        dae.continuous
            .equations
            .push(binary_eq("a", "b", "component equation"));
        dae.continuous
            .equations
            .push(binary_eq("b", "external", "connect(b, external)"));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }
}
