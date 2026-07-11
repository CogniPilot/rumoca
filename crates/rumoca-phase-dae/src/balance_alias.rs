use super::{BalanceSymbolSet, eq_binary_var_refs};
use rumoca_ir_dae as dae;

pub(super) fn is_vector_forwarding_alias(
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    _output_names: &BalanceSymbolSet,
    _component_defined_targets: &BalanceSymbolSet,
) -> bool {
    if eq.scalar_count <= 1 {
        let Some((lhs, rhs)) = eq_binary_lhs_rhs(&eq.rhs) else {
            return false;
        };
        return eq.origin.starts_with("equation from ")
            && (is_unresolved_reference_field_alias(lhs, rhs, continuous_unknowns)
                || is_rooted_two_pin_reference_anchor_alias(eq.origin.as_str(), lhs, rhs));
    }
    if !(eq.origin.starts_with("binding equation for") || eq.origin.starts_with("equation from ")) {
        return false;
    }
    let Some((lhs, rhs)) = eq_binary_lhs_rhs(&eq.rhs) else {
        return false;
    };
    if matches!(lhs, rumoca_core::Expression::FieldAccess { .. }) {
        return !lhs_matches_symbols(lhs, continuous_unknowns);
    }
    if forwarded_value_ref(rhs).is_none() {
        return false;
    }
    if !lhs_is_forwarding_target(lhs) {
        return false;
    }
    !lhs_matches_symbols(lhs, continuous_unknowns)
}

pub(super) fn is_surplus_component_vector_forwarding_alias(
    eq: &dae::Equation,
    continuous_unknowns: &BalanceSymbolSet,
    _output_names: &BalanceSymbolSet,
    _component_defined_targets: &BalanceSymbolSet,
) -> bool {
    if (eq.scalar_count <= 1 && !eq.origin.contains(" [scalarized "))
        || !(eq.origin.starts_with("binding equation for")
            || eq.origin.starts_with("equation from "))
    {
        return false;
    }
    let Some((lhs, rhs)) = eq_binary_lhs_rhs(&eq.rhs) else {
        return false;
    };
    if forwarded_value_ref(rhs).is_none() {
        return false;
    }
    lhs_is_forwarding_target(lhs) && lhs_matches_symbols(lhs, continuous_unknowns)
}

pub(super) fn is_surplus_overconstrained_derivative_alias(eq: &dae::Equation) -> bool {
    if eq.scalar_count != 1 || !eq.origin.starts_with("overconstrained derivative alias:") {
        return false;
    }
    let Some((lhs, rhs)) = eq_binary_lhs_rhs(&eq.rhs) else {
        return false;
    };
    let Some(lhs_name) = derivative_alias_reference_name(lhs) else {
        return false;
    };
    let Some(rhs_name) = derivative_alias_reference_name(rhs) else {
        return false;
    };
    lhs_name.as_str().ends_with(".reference.gamma")
        && rhs_name.as_str().ends_with(".reference.gamma")
}

fn derivative_alias_reference_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    let rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args,
        ..
    } = expr
    else {
        return None;
    };
    let [arg] = args.as_slice() else {
        return None;
    };
    expression_var_name(arg)
}

fn lhs_is_forwarding_target(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef { .. } | rumoca_core::Expression::FieldAccess { .. }
    )
}

fn is_unresolved_reference_field_alias(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    continuous_unknowns: &BalanceSymbolSet,
) -> bool {
    let Some(lhs_name) = field_access_var_name(lhs) else {
        return false;
    };
    let Some(rhs_name) = field_access_var_name(rhs) else {
        return false;
    };
    lhs_name.as_str().ends_with(".reference.gamma")
        && rhs_name.as_str().ends_with(".reference.gamma")
        && !continuous_unknowns.matches_name(&lhs_name)
}

fn is_rooted_two_pin_reference_anchor_alias(
    origin: &str,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
) -> bool {
    let Some(component) = origin.strip_prefix("equation from ") else {
        return false;
    };
    let Some(lhs_name) = expression_var_name(lhs) else {
        return false;
    };
    let Some(rhs_name) = expression_var_name(rhs) else {
        return false;
    };
    let plug_p_reference = format!("{component}.plug_p.reference.gamma");
    let plug_n_reference = format!("{component}.plug_n.reference.gamma");
    let internal_reference = format!("{component}.gamma");
    lhs_name.as_str() == plug_p_reference
        && (rhs_name.as_str() == plug_n_reference || rhs_name.as_str() == internal_reference)
}

fn lhs_matches_symbols(expr: &rumoca_core::Expression, symbols: &BalanceSymbolSet) -> bool {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => symbols.matches_reference(name),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            field_access_var_name(expr).is_some_and(|name| symbols.matches_name(&name))
                || field_access_array_members_match_symbols(base, field, symbols)
                || matches!(
                    base.as_ref(),
                    rumoca_core::Expression::VarRef { name, .. } if symbols.matches_reference(name)
                )
        }
        _ => false,
    }
}

fn field_access_array_members_match_symbols(
    base: &rumoca_core::Expression,
    field: &str,
    symbols: &BalanceSymbolSet,
) -> bool {
    let rumoca_core::Expression::VarRef { name, .. } = base else {
        return false;
    };
    let array_prefix = format!("{}[", name.var_name().as_str());
    let field_marker = format!("].{field}");
    symbols.names.iter().any(|candidate| {
        let candidate = candidate.as_str();
        candidate.starts_with(&array_prefix) && candidate.contains(&field_marker)
    }) || symbols.prefixes.iter().any(|candidate| {
        let candidate = candidate.as_str();
        candidate.starts_with(&array_prefix) && candidate.contains(&field_marker)
    })
}

fn field_access_var_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = expr else {
        return None;
    };
    Some(rumoca_core::VarName::new(format!(
        "{}.{field}",
        indexed_expression_var_name(base)?
    )))
}

fn expression_var_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let suffix = subscript_suffix(subscripts)?;
            Some(rumoca_core::VarName::new(format!(
                "{}{suffix}",
                name.var_name().as_str()
            )))
        }
        rumoca_core::Expression::FieldAccess { .. } => field_access_var_name(expr),
        rumoca_core::Expression::Index { .. } => Some(rumoca_core::VarName::new(
            indexed_expression_var_name(expr)?,
        )),
        _ => None,
    }
}

fn indexed_expression_var_name(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Some(format!(
            "{}{}",
            name.var_name().as_str(),
            subscript_suffix(subscripts)?
        )),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => Some(format!(
            "{}{}",
            indexed_expression_var_name(base)?,
            subscript_suffix(subscripts)?
        )),
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            Some(format!("{}.{field}", indexed_expression_var_name(base)?))
        }
        _ => None,
    }
}

fn subscript_suffix(subscripts: &[rumoca_core::Subscript]) -> Option<String> {
    let mut suffix = String::new();
    for subscript in subscripts {
        let rumoca_core::Subscript::Index { value, .. } = subscript else {
            return None;
        };
        suffix.push('[');
        suffix.push_str(&value.to_string());
        suffix.push(']');
    }
    Some(suffix)
}

fn forwarded_value_ref(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => Some(name),
        rumoca_core::Expression::FieldAccess { base, .. } => {
            let rumoca_core::Expression::VarRef { name, .. } = base.as_ref() else {
                return None;
            };
            Some(name)
        }
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
    _output_names: &BalanceSymbolSet,
    component_defined_targets: &BalanceSymbolSet,
) -> bool {
    let refs = eq_binary_var_refs(&eq.rhs);
    let [lhs, rhs] = refs.as_slice() else {
        return false;
    };
    let lhs_is_continuous_unknown = continuous_unknowns.matches_reference(lhs);
    let rhs_is_continuous_unknown = continuous_unknowns.matches_reference(rhs);
    let lhs_is_component_defined = component_defined_targets.matches_reference(lhs);
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

    fn vector_field_access_eq(
        lhs_base: &str,
        lhs_field: &str,
        rhs_name: &str,
        origin: &str,
        count: usize,
    ) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::FieldAccess {
                    base: Box::new(var_ref(lhs_base)),
                    field: lhs_field.to_string(),
                    span: test_span(),
                }),
                rhs: Box::new(var_ref(rhs_name)),
                span: test_span(),
            },
            span: test_span(),
            origin: origin.to_string(),
            scalar_count: count,
        }
    }

    fn field_access_expr(
        base: rumoca_core::Expression,
        fields: &[&str],
    ) -> rumoca_core::Expression {
        fields
            .iter()
            .fold(base, |base, field| rumoca_core::Expression::FieldAccess {
                base: Box::new(base),
                field: (*field).to_string(),
                span: test_span(),
            })
    }

    fn indexed_field_access_expr(
        base: &str,
        index: i64,
        fields: &[&str],
    ) -> rumoca_core::Expression {
        field_access_expr(
            rumoca_core::Expression::Index {
                base: Box::new(var_ref(base)),
                subscripts: vec![rumoca_core::Subscript::Index {
                    value: index,
                    span: test_span(),
                }],
                span: test_span(),
            },
            fields,
        )
    }

    fn scalar_expr_eq(
        lhs: rumoca_core::Expression,
        rhs: rumoca_core::Expression,
        origin: &str,
    ) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: test_span(),
            },
            span: test_span(),
            origin: origin.to_string(),
            scalar_count: 1,
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
    fn balance_skips_component_defined_output_forwarding_from_input() {
        let mut dae = dae::Dae::default();
        dae.variables
            .outputs
            .insert(rumoca_core::VarName::new("y"), vector_output_var("y", 3));
        dae.variables
            .inputs
            .insert(rumoca_core::VarName::new("u"), vector_algebraic_var("u", 3));

        dae.continuous
            .equations
            .push(vector_binary_eq("y", "source", "component equation", 3));
        dae.continuous
            .equations
            .push(vector_binary_eq("y", "u", "equation from replicator", 3));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_component_defined_field_projection_from_input() {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("pin.v"),
            vector_algebraic_var("pin.v", 3),
        );
        dae.variables
            .inputs
            .insert(rumoca_core::VarName::new("u"), vector_algebraic_var("u", 3));

        dae.continuous
            .equations
            .push(vector_binary_eq("pin.v", "source", "component equation", 3));
        dae.continuous.equations.push(vector_field_access_eq(
            "pin",
            "v",
            "u",
            "equation from plugToPin",
            3,
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_component_defined_array_field_projection_from_input() {
        let mut dae = dae::Dae::default();
        for name in ["pin[1].v.re", "pin[1].v.im"] {
            dae.variables
                .algebraics
                .insert(rumoca_core::VarName::new(name), algebraic_var(name));
        }
        dae.variables
            .inputs
            .insert(rumoca_core::VarName::new("u"), vector_algebraic_var("u", 2));

        dae.continuous
            .equations
            .push(binary_eq("pin[1].v.re", "source_re", "component equation"));
        dae.continuous
            .equations
            .push(binary_eq("pin[1].v.im", "source_im", "component equation"));
        dae.continuous.equations.push(vector_field_access_eq(
            "pin",
            "v",
            "u",
            "equation from plugToPin",
            2,
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_counts_output_projection_when_lhs_is_not_component_defined() {
        let mut dae = dae::Dae::default();
        dae.variables
            .outputs
            .insert(rumoca_core::VarName::new("y"), vector_output_var("y", 3));
        dae.variables
            .algebraics
            .insert(rumoca_core::VarName::new("x"), vector_algebraic_var("x", 3));

        dae.continuous
            .equations
            .push(vector_binary_eq("x", "source", "component equation", 3));
        dae.continuous
            .equations
            .push(vector_binary_eq("y", "x", "equation from plant", 3));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_unresolved_indexed_reference_field_alias() {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("sensor.sensor[1].pin_p.reference.gamma"),
            algebraic_var("sensor.sensor[1].pin_p.reference.gamma"),
        );
        dae.continuous.equations.push(binary_eq(
            "sensor.sensor[1].pin_p.reference.gamma",
            "driver",
            "component equation",
        ));
        dae.continuous.equations.push(scalar_expr_eq(
            indexed_field_access_expr("sensor", 1, &["pin_p", "reference", "gamma"]),
            indexed_field_access_expr("sensor", 1, &["pin_n", "reference", "gamma"]),
            "equation from sensor.sensor[1]",
        ));

        assert_eq!(balance_value(&dae).expect("valid DAE balance fixture"), 0);
    }

    #[test]
    fn balance_skips_rooted_two_pin_reference_anchor_aliases() {
        let mut dae = dae::Dae::default();
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new("source.plug_p.reference.gamma"),
            algebraic_var("source.plug_p.reference.gamma"),
        );
        dae.continuous.equations.push(binary_eq(
            "source.plug_p.reference.gamma",
            "driver",
            "component equation",
        ));
        dae.continuous.equations.push(binary_eq(
            "source.plug_p.reference.gamma",
            "source.gamma",
            "equation from source",
        ));
        dae.continuous.equations.push(binary_eq(
            "source.plug_p.reference.gamma",
            "source.plug_n.reference.gamma",
            "equation from source",
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
