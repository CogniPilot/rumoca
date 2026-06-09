use super::*;

#[test]
fn test_todae_splits_mixed_clocked_tuple_assignment_by_dae_partition() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("noise"),
        flat::Variable {
            name: VarName::new("noise"),
            component_ref: Some(make_comp_ref("noise")),
            is_primitive: true,
            ..Default::default()
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
            ..Default::default()
        },
    );

    let mut hold = rumoca_core::Function::new("hold", Default::default());
    hold.outputs
        .push(rumoca_core::FunctionParam::new("x", "Real"));
    hold.outputs
        .push(rumoca_core::FunctionParam::new("seedOut", "Integer").with_dims(vec![3]));
    flat.functions.insert(hold.name.clone(), hold);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Tuple {
                elements: vec![make_var_ref("noise"), make_var_ref("seedState")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("hold").into(),
                args: vec![Expression::FunctionCall {
                    name: VarName::new("previous").into(),
                    args: vec![make_var_ref("seedState")],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "clocked".to_string(),
        },
        scalar_count: 4,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("clocked tuple assignment should convert");

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
}

fn selected_call_name(expr: &rumoca_core::Expression) -> Option<String> {
    match expr {
        rumoca_core::Expression::FunctionCall { name, .. } => Some(name.as_str().to_string()),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            selected_call_name(lhs).or_else(|| selected_call_name(rhs))
        }
        _ => None,
    }
}
