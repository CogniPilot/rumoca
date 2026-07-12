use super::*;

#[test]
fn test_todae_splits_mixed_clocked_tuple_assignment_by_dae_partition() {
    let flat = mixed_clocked_tuple_model();

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("clocked tuple assignment should convert");

    assert_mixed_clocked_tuple_dae(&dae);
}

fn mixed_clocked_tuple_model() -> Model {
    let mut flat = Model::new();
    let span = crate::test_support::test_span();
    add_clocked_tuple_variables(&mut flat, span);
    add_hold_function(&mut flat, span);
    add_clocked_tuple_equation(&mut flat, span);
    flat
}

fn add_clocked_tuple_variables(flat: &mut Model, span: Span) {
    flat.add_variable(
        VarName::new("noise"),
        flat::Variable {
            name: VarName::new("noise"),
            component_ref: Some(make_comp_ref("noise")),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        },
    );
    flat.add_variable(
        VarName::new("seedState"),
        flat::Variable {
            name: VarName::new("seedState"),
            component_ref: Some(make_comp_ref("seedState")),
            dims: vec![3],
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(span)
        },
    );
}

fn add_hold_function(flat: &mut Model, span: Span) {
    let mut hold = rumoca_core::Function::new("hold", span);
    hold.outputs
        .push(rumoca_core::FunctionParam::new("x", "Real", span));
    hold.outputs
        .push(rumoca_core::FunctionParam::new("seedOut", "Integer", span).with_dims(vec![3]));
    flat.add_function(hold);
}

fn add_clocked_tuple_equation(flat: &mut Model, span: Span) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Tuple {
                elements: vec![make_var_ref("noise"), make_var_ref("seedState")],
                span,
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: rumoca_core::Reference::from_component_reference(make_comp_ref("hold")),
                args: vec![Expression::FunctionCall {
                    name: rumoca_core::Reference::from_component_reference(make_comp_ref(
                        "previous",
                    )),
                    args: vec![make_var_ref("seedState")],
                    is_constructor: false,
                    span,
                }],
                is_constructor: false,
                span,
            }),
            span,
        },
        span,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "clocked".to_string(),
        },
        scalar_count: 4,
    });
}

fn assert_mixed_clocked_tuple_dae(dae: &rumoca_ir_dae::Dae) {
    assert!(
        dae.variables
            .discrete_reals
            .contains_key(&rumoca_core::VarName::new("noise"))
    );
    assert!(
        dae.variables
            .discrete_valued
            .contains_key(&rumoca_core::VarName::new("seedState"))
    );
    assert!(
        dae.continuous.equations.is_empty(),
        "clocked tuple assignment should not remain in f_x"
    );
    assert_eq!(
        dae.discrete.real_updates.len(),
        1,
        "Real tuple output should be routed to f_z"
    );
    assert_eq!(dae.discrete.real_updates[0].scalar_count, 1);
    assert_eq!(
        dae.discrete.valued_updates.len(),
        1,
        "Integer tuple output should be routed to f_m"
    );
    assert_eq!(dae.discrete.valued_updates[0].scalar_count, 3);
    assert_eq!(
        selected_call_name(&dae.discrete.real_updates[0].rhs).as_deref(),
        Some("hold.x")
    );
    assert_eq!(
        selected_call_name(&dae.discrete.valued_updates[0].rhs).as_deref(),
        Some("hold.seedOut")
    );
    let selected = selected_call_reference(&dae.discrete.valued_updates[0].rhs)
        .expect("tuple output call reference");
    assert!(selected.has_structure());
    assert_eq!(
        selected
            .parts()
            .iter()
            .map(|part| part.ident.as_str())
            .collect::<Vec<_>>(),
        ["hold", "seedOut"]
    );
}

fn selected_call_name(expr: &rumoca_core::Expression) -> Option<String> {
    selected_call_reference(expr).map(|name| name.as_str().to_string())
}

fn selected_call_reference(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::FunctionCall { name, .. } => Some(name),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            selected_call_reference(lhs).or_else(|| selected_call_reference(rhs))
        }
        _ => None,
    }
}
