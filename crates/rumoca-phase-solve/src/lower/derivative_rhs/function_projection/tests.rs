use super::*;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_projection_test.mo"),
        0,
        1,
    )
}

fn real(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span: test_span(),
    }
}

fn integer(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Integer(value),
        span: test_span(),
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

fn array(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: test_span(),
    }
}

fn builtin(
    function: rumoca_core::BuiltinFunction,
    args: Vec<rumoca_core::Expression>,
) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function,
        args,
        span: test_span(),
    }
}

fn binary(
    op: rumoca_core::OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn component_reference(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::Reference {
    rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
        local: false,
        span: test_span(),
        parts,
        def_id: None,
    })
}

fn assert_var_ref_name(expr: &rumoca_core::Expression, expected: &str) {
    let rumoca_core::Expression::VarRef { name, .. } = expr else {
        panic!("expected VarRef `{expected}`, got {expr:?}");
    };
    assert_eq!(name.as_str(), expected);
}

#[test]
fn flatten_array_elements_flattens_matrix_rows() -> Result<(), LowerError> {
    let row1 = array(vec![real(1.0), real(2.0)], false);
    let row2 = array(vec![real(3.0), real(4.0)], false);

    let flattened = flatten_array_elements(&[row1, row2], test_span())?;
    let values = flattened
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::Literal {
                value: Literal::Real(value),
                ..
            } => *value,
            other => panic!("expected scalar literal, got {other:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(values, vec![1.0, 2.0, 3.0, 4.0]);
    Ok(())
}

#[test]
fn scalar_flat_index_projects_to_empty_subscripts() -> Result<(), LowerError> {
    let subscripts = required_flat_index_to_subscripts(&[], 0, test_span())?;

    assert!(subscripts.is_empty());
    Ok(())
}

#[test]
fn scalar_flat_index_rejects_nonzero_index() {
    let err = required_flat_index_to_subscripts(&[], 1, test_span())
        .expect_err("scalar flat index one should be out of bounds");

    assert!(
        err.reason()
            .contains("flat index 1 is out of bounds for dimensions []"),
        "{}",
        err.reason()
    );
}

#[test]
fn stream_passthrough_projects_argument_scalars() -> Result<(), LowerError> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope
        .scalars
        .insert("u".to_string(), vec![real(1.0), real(2.0)]);
    scope.dims.insert("u".to_string(), vec![2]);
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("inStream").into(),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("u"),
            subscripts: Vec::new(),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };

    let projected = analysis
        .project_value_scalars(&expr, &[2], &scope, 0, test_span())?
        .expect("stream passthrough should project argument scalars");
    let values = projected
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::Literal {
                value: Literal::Real(value),
                ..
            } => *value,
            other => panic!("expected scalar literal, got {other:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(values, vec![1.0, 2.0]);
    let dims = analysis
        .expr_dims(&expr, &scope, 0, test_span())?
        .expect("stream passthrough should infer argument dimensions");
    assert_eq!(dims, vec![2]);
    Ok(())
}

#[test]
fn cat_projection_concatenates_dynamic_vector_and_computed_tail() -> Result<(), LowerError> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.scalars.insert("u".to_string(), vec![real(0.25)]);
    scope.dims.insert("u".to_string(), vec![1]);
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cat,
        args: vec![
            integer(1),
            var_ref("u"),
            array(
                vec![binary(
                    rumoca_core::OpBinary::Sub,
                    integer(1),
                    rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Sum,
                        args: vec![var_ref("u")],
                        span: test_span(),
                    },
                    test_span(),
                )],
                false,
            ),
        ],
        span: test_span(),
    };

    let dims = analysis
        .expr_dims(&expr, &scope, 0, test_span())?
        .expect("cat dimensions should be inferred");
    assert_eq!(dims, vec![2]);
    let projected = analysis
        .project_value_scalars(&expr, &[2], &scope, 0, test_span())?
        .expect("cat should project to scalar values");

    assert_eq!(projected.len(), 2);
    assert!(matches!(
        &projected[0],
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            ..
        } if (*value - 0.25).abs() < 1e-12
    ));
    assert!(matches!(
        &projected[1],
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            ..
        }
    ));
    Ok(())
}

#[test]
fn full_binding_substitution_rewrites_nested_local_inputs() -> Result<(), LowerError> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("p".to_string(), real(101325.0));
    scope.full.insert(
        "state".to_string(),
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("ThermodynamicState").into(),
            args: vec![rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("__rumoca_named_arg__.p").into(),
                args: vec![rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("p"),
                    subscripts: Vec::new(),
                    span: test_span(),
                }],
                is_constructor: true,
                span: test_span(),
            }],
            is_constructor: true,
            span: test_span(),
        },
    );

    let substituted = analysis.substitute(
        &rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("state"),
            subscripts: Vec::new(),
            span: test_span(),
        },
        &scope,
    )?;

    let rumoca_core::Expression::FunctionCall { args, .. } = substituted else {
        panic!("expected substituted record constructor");
    };
    let rumoca_core::Expression::FunctionCall { args, .. } = &args[0] else {
        panic!("expected named argument constructor");
    };
    assert!(matches!(
        args.as_slice(),
        [rumoca_core::Expression::Literal {
            value: Literal::Real(101325.0),
            ..
        }]
    ));
    Ok(())
}

#[test]
fn named_constructor_field_access_projects_selected_actual() -> Result<(), LowerError> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("p".to_string(), real(101325.0));
    scope.full.insert("T".to_string(), real(295.0));
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("ThermodynamicState").into(),
            args: vec![
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("__rumoca_named_arg__.p").into(),
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("p"),
                        subscripts: Vec::new(),
                        span: test_span(),
                    }],
                    is_constructor: true,
                    span: test_span(),
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("__rumoca_named_arg__.T").into(),
                    args: vec![rumoca_core::Expression::Binary {
                        op: OpBinary::Add,
                        lhs: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::new("p"),
                            subscripts: Vec::new(),
                            span: test_span(),
                        }),
                        rhs: Box::new(rumoca_core::Expression::VarRef {
                            name: rumoca_core::Reference::new("T"),
                            subscripts: Vec::new(),
                            span: test_span(),
                        }),
                        span: test_span(),
                    }],
                    is_constructor: true,
                    span: test_span(),
                },
            ],
            is_constructor: true,
            span: test_span(),
        }),
        field: "p".to_string(),
        span: test_span(),
    };

    let projected = analysis
        .project_value_scalars(&expr, &[], &scope, 0, test_span())?
        .expect("named constructor field should project");

    assert!(matches!(
        projected.as_slice(),
        [rumoca_core::Expression::Literal {
            value: Literal::Real(101325.0),
            ..
        }]
    ));
    Ok(())
}

#[test]
fn function_record_field_access_projects_if_constructor_output() -> Result<(), LowerError> {
    let mut dae_model = dae::Dae::default();
    let mut state_ctor = rumoca_core::Function::new("My.State", test_span());
    state_ctor.is_constructor = true;
    state_ctor.pure = true;
    state_ctor.inputs.push(scalar_function_param("p"));
    state_ctor.inputs.push(scalar_function_param("T"));
    state_ctor
        .outputs
        .push(record_function_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.State"), state_ctor);

    let mut make_state = rumoca_core::Function::new("My.makeState", test_span());
    make_state.pure = true;
    make_state.inputs.push(scalar_function_param("p"));
    make_state.inputs.push(scalar_function_param("T"));
    make_state
        .outputs
        .push(record_function_param("state", "My.State"));
    make_state.body.push(scalar_assignment(
        "state",
        rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::Binary {
                    op: OpBinary::Eq,
                    lhs: Box::new(local_var("p")),
                    rhs: Box::new(local_var("p")),
                    span: test_span(),
                },
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("My.State").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("__rumoca_named_arg__.p").into(),
                            args: vec![local_var("p")],
                            is_constructor: true,
                            span: test_span(),
                        },
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("__rumoca_named_arg__.T").into(),
                            args: vec![rumoca_core::Expression::Binary {
                                op: OpBinary::Add,
                                lhs: Box::new(local_var("p")),
                                rhs: Box::new(local_var("T")),
                                span: test_span(),
                            }],
                            is_constructor: true,
                            span: test_span(),
                        },
                    ],
                    is_constructor: true,
                    span: test_span(),
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("My.State").into(),
                args: vec![local_var("p"), local_var("T")],
                is_constructor: true,
                span: test_span(),
            }),
            span: test_span(),
        },
    ));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.makeState"), make_state);

    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("p".to_string(), real(101325.0));
    scope.full.insert("T".to_string(), real(295.0));
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.makeState").into(),
            args: vec![local_var("p"), local_var("T")],
            is_constructor: false,
            span: test_span(),
        }),
        field: "p".to_string(),
        span: test_span(),
    };

    let projected = analysis
        .project_value_scalars(&expr, &[], &scope, 0, test_span())?
        .expect("function record field should project");

    assert!(matches!(
        projected.as_slice(),
        [rumoca_core::Expression::Literal {
            value: Literal::Real(101325.0),
            ..
        }]
    ));
    Ok(())
}

#[test]
fn flattened_record_inputs_project_positional_record_actual_fields() -> Result<(), LowerError> {
    let mut dae_model = dae::Dae::default();
    let mut state_ctor = rumoca_core::Function::new("My.State", test_span());
    state_ctor.is_constructor = true;
    state_ctor.pure = true;
    state_ctor.inputs.push(scalar_function_param("p"));
    state_ctor.inputs.push(scalar_function_param("T"));
    state_ctor
        .outputs
        .push(record_function_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.State"), state_ctor);

    let mut make_state = rumoca_core::Function::new("My.makeState", test_span());
    make_state.pure = true;
    make_state.inputs.push(scalar_function_param("p"));
    make_state.inputs.push(scalar_function_param("T"));
    make_state
        .outputs
        .push(record_function_param("state", "My.State"));
    make_state.body.push(scalar_assignment(
        "state",
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.State").into(),
            args: vec![local_var("p"), local_var("T")],
            is_constructor: true,
            span: test_span(),
        },
    ));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.makeState"), make_state);

    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("p".to_string(), real(101325.0));
    scope.full.insert("T".to_string(), real(295.0));
    let mut enthalpy = rumoca_core::Function::new("My.specificEnthalpy", test_span());
    enthalpy.inputs.push(scalar_function_param("state_p"));
    enthalpy.inputs.push(scalar_function_param("state_T"));
    let actual = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.makeState").into(),
        args: vec![local_var("p"), local_var("T")],
        is_constructor: false,
        span: test_span(),
    };
    let p_field = rumoca_core::Expression::FieldAccess {
        base: Box::new(actual.clone()),
        field: "p".to_string(),
        span: test_span(),
    };
    let direct_projected = analysis
        .project_value_scalars(&p_field, &[], &scope, 0, test_span())?
        .expect("direct record field projection should return a scalar");
    assert!(matches!(
        direct_projected.as_slice(),
        [rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            ..
        }] if (*value - 101325.0).abs() < 1e-12
    ));

    let projected = analysis
        .bind_inputs(&enthalpy, &[actual], 0, test_span())?
        .expect("flattened record inputs should bind from positional record actual");

    assert_var_ref_name(
        projected.full.get("state_p").expect("state_p should bind"),
        "p",
    );
    assert_var_ref_name(
        projected.full.get("state_T").expect("state_T should bind"),
        "T",
    );
    Ok(())
}

#[test]
fn nested_flattened_record_call_uses_caller_projection_scope() -> Result<(), LowerError> {
    let mut dae_model = dae::Dae::default();
    let mut state_ctor = rumoca_core::Function::new("My.State", test_span());
    state_ctor.is_constructor = true;
    state_ctor.pure = true;
    state_ctor.inputs.push(scalar_function_param("p"));
    state_ctor.inputs.push(scalar_function_param("T"));
    state_ctor
        .outputs
        .push(record_function_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.State"), state_ctor);

    let mut make_state = rumoca_core::Function::new("My.makeState", test_span());
    make_state.pure = true;
    make_state.inputs.push(scalar_function_param("p"));
    make_state.inputs.push(scalar_function_param("T"));
    make_state
        .outputs
        .push(record_function_param("state", "My.State"));
    make_state.body.push(scalar_assignment(
        "state",
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.State").into(),
            args: vec![local_var("p"), local_var("T")],
            is_constructor: true,
            span: test_span(),
        },
    ));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.makeState"), make_state);

    let mut density = rumoca_core::Function::new("My.density", test_span());
    density.pure = true;
    density.inputs.push(scalar_function_param("state_p"));
    density.inputs.push(scalar_function_param("state_T"));
    density.outputs.push(scalar_function_param("d"));
    density
        .body
        .push(scalar_assignment("d", local_var("state_p")));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.density"), density);

    let mut use_density = rumoca_core::Function::new("My.useDensity", test_span());
    use_density.pure = true;
    use_density.inputs.push(scalar_function_param("state_p"));
    use_density.inputs.push(scalar_function_param("state_T"));
    use_density.outputs.push(scalar_function_param("y"));
    use_density.body.push(scalar_assignment(
        "y",
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.density").into(),
            args: vec![local_var("state")],
            is_constructor: false,
            span: test_span(),
        },
    ));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.useDensity"), use_density);

    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.useDensity").into(),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.makeState").into(),
            args: vec![local_var("p"), local_var("T")],
            is_constructor: false,
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };

    let outputs = analysis
        .function_call_outputs_with_owner(&call, 0, test_span())?
        .expect("nested flattened call should project outputs");
    assert_eq!(outputs.len(), 1);
    assert_var_ref_name(&outputs[0].expr, "p");
    Ok(())
}

#[test]
fn record_field_projection_selects_compile_time_if_branch() -> Result<(), LowerError> {
    let mut dae_model = dae::Dae::default();
    let mut state_ctor = rumoca_core::Function::new("My.State", test_span());
    state_ctor.is_constructor = true;
    state_ctor.pure = true;
    state_ctor.inputs.push(scalar_function_param("p"));
    state_ctor
        .outputs
        .push(record_function_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(rumoca_core::VarName::new("My.State"), state_ctor);
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let value = rumoca_core::Expression::If {
        branches: vec![(
            binary(
                rumoca_core::OpBinary::Eq,
                integer(1),
                integer(0),
                test_span(),
            ),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("My.State").into(),
                args: vec![rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("__rumoca_named_arg__.p").into(),
                    args: vec![real(1.0)],
                    is_constructor: true,
                    span: test_span(),
                }],
                is_constructor: true,
                span: test_span(),
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.State").into(),
            args: vec![rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("__rumoca_named_arg__.p").into(),
                args: vec![real(2.0)],
                is_constructor: true,
                span: test_span(),
            }],
            is_constructor: true,
            span: test_span(),
        }),
        span: test_span(),
    };

    let projected = analysis
        .project_record_field_value(&value, "p", &scope, test_span())?
        .expect("compile-time record field branch should project");

    assert!(matches!(
        projected,
        rumoca_core::Expression::Literal {
            value: Literal::Real(2.0),
            ..
        }
    ));
    Ok(())
}

#[test]
fn scoped_single_scalar_value_has_scalar_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope
        .scalars
        .insert("tau_inv".to_string(), vec![real(17.0)]);
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("tau_inv"),
        subscripts: Vec::new(),
        span: test_span(),
    };

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(Vec::new()))
    );
}

#[test]
fn scalar_projected_output_uses_empty_selector_indices() -> Result<(), LowerError> {
    let outputs = project_target_scalar_outputs(&[], vec![real(3.0)], test_span())?;

    assert_eq!(outputs.len(), 1);
    assert!(outputs[0].selector_indices.is_empty());
    Ok(())
}

#[test]
fn literal_binary_operand_has_known_scalar_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let expr = binary(
        rumoca_core::OpBinary::Add,
        real(1.0),
        array(vec![real(2.0), real(3.0)], false),
        test_span(),
    );

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(vec![2]))
    );
}

#[test]
fn scoped_full_binding_has_substituted_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("gain".to_string(), real(5.0));
    let expr = local_var("gain");

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(Vec::new()))
    );
}

#[test]
fn projected_scope_dimensions_override_full_binding_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.full.insert("x".to_string(), real(5.0));
    scope.dims.insert("x".to_string(), vec![2]);
    let expr = local_var("x");

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(vec![2]))
    );
}

#[test]
fn array_of_vector_values_infers_matrix_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.dims.insert("row1".to_string(), vec![3]);
    scope.dims.insert("row2".to_string(), vec![3]);
    scope.dims.insert("row3".to_string(), vec![3]);
    let expr = array(
        vec![local_var("row1"), local_var("row2"), local_var("row3")],
        false,
    );

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(vec![3, 3]))
    );
}

#[test]
fn array_with_cross_row_infers_matrix_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.dims.insert("e_x".to_string(), vec![3]);
    scope.dims.insert("e_z".to_string(), vec![3]);
    let expr = array(
        vec![
            local_var("e_x"),
            builtin(
                rumoca_core::BuiltinFunction::Cross,
                vec![local_var("e_z"), local_var("e_x")],
            ),
            local_var("e_z"),
        ],
        false,
    );

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(vec![3, 3]))
    );
}

#[test]
fn array_with_cross_row_projects_nested_scalar() -> Result<(), LowerError> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope.dims.insert("e_x".to_string(), vec![3]);
    scope.dims.insert("e_z".to_string(), vec![3]);
    let expr = array(
        vec![
            local_var("e_x"),
            builtin(
                rumoca_core::BuiltinFunction::Cross,
                vec![local_var("e_z"), local_var("e_x")],
            ),
            local_var("e_z"),
        ],
        false,
    );

    let projected = analysis
        .project_value(&expr, &[3, 3], 3, &scope, 0, test_span())?
        .expect("array row vector expression should project to a scalar");

    let rumoca_core::Expression::Index {
        base, subscripts, ..
    } = projected
    else {
        panic!("expected indexed cross-product expression, got {projected:?}");
    };
    let rumoca_core::Expression::BuiltinCall { function, .. } = base.as_ref() else {
        panic!("expected indexed cross-product base, got {base:?}");
    };
    let [rumoca_core::Subscript::Index { value, .. }] = subscripts.as_slice() else {
        panic!("expected one generated subscript, got {subscripts:?}");
    };

    assert_eq!(*function, rumoca_core::BuiltinFunction::Cross);
    assert_eq!(*value, 1);
    Ok(())
}

#[test]
fn dae_scalar_variable_has_known_scalar_dimensions() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("angle"),
        dae::Variable {
            name: rumoca_core::VarName::new("angle"),
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        },
    );
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let expr = local_var("angle");

    assert_eq!(
        analysis.expr_dims(&expr, &scope, 0, test_span()),
        Ok(Some(Vec::new()))
    );
}

#[test]
fn projected_function_field_outputs_infer_dense_selector_dimensions() -> Result<(), LowerError> {
    let outputs = vec![
        ProjectedFunctionOutput {
            field_path: vec!["w".to_string()],
            selector_indices: vec![1],
            expr: real(1.0),
        },
        ProjectedFunctionOutput {
            field_path: vec!["w".to_string()],
            selector_indices: vec![2],
            expr: real(2.0),
        },
        ProjectedFunctionOutput {
            field_path: vec!["w".to_string()],
            selector_indices: vec![3],
            expr: real(3.0),
        },
    ];

    let dims = projected_field_output_dims(&outputs, "w", test_span())?;

    assert_eq!(dims, Some(vec![3]));
    Ok(())
}

#[test]
fn repeated_scalar_field_outputs_have_unknown_dimensions() -> Result<(), LowerError> {
    let outputs = vec![
        ProjectedFunctionOutput {
            field_path: vec!["record".to_string()],
            selector_indices: Vec::new(),
            expr: real(1.0),
        },
        ProjectedFunctionOutput {
            field_path: vec!["record".to_string()],
            selector_indices: Vec::new(),
            expr: real(2.0),
        },
    ];

    let dims = projected_field_output_dims(&outputs, "record", test_span())?;

    assert_eq!(dims, None);
    Ok(())
}

#[test]
fn array_binary_projection_rejects_unknown_operand_dimensions_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_51.mo",
        ),
        4,
        17,
    );
    let expr = binary(
        rumoca_core::OpBinary::Add,
        local_var("runtime_value"),
        array(vec![real(2.0), real(3.0)], false),
        span,
    );
    let ctx = ProjectionValueCtx {
        dims: &[2],
        flat_index: 0,
        scope: &scope,
        depth: 0,
        span,
    };

    let rumoca_core::Expression::Binary { lhs, rhs, op, .. } = &expr else {
        panic!("test expression must be binary");
    };
    let err = analysis
        .project_binary_value(op, lhs, rhs, &ctx)
        .expect_err("unknown operand dimensions must bubble a typed error");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "binary lhs has unknown dimensions".to_string()
    );
}

#[test]
fn checked_usize_dimension_rejects_i64_overflow_with_span() {
    let Some(dim) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_47.mo",
        ),
        8,
        19,
    );

    let err = checked_usize_dims_to_i64(&[dim], "array expression dimension", span)
        .expect_err("dimension must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        format!("invalid IR contract: array expression dimension {dim} exceeds i64 range")
    );
}

#[test]
fn checked_projection_offset_rejects_host_index_overflow_with_span() -> Result<(), String> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_48.mo",
        ),
        3,
        11,
    );

    let Err(mul_err) =
        checked_projection_offset(usize::MAX, 2, 0, "matrix product flat index", span)
    else {
        return Err("overflowing projection offset multiplication succeeded".to_string());
    };
    assert_eq!(mul_err.source_span(), Some(span));
    assert_eq!(
        mul_err.reason(),
        "invalid IR contract: matrix product flat index multiplication overflows host index range"
            .to_string()
    );

    let Err(add_err) =
        checked_projection_offset(usize::MAX, 1, 1, "matrix product flat index", span)
    else {
        return Err("overflowing projection offset addition succeeded".to_string());
    };
    assert_eq!(add_err.source_span(), Some(span));
    assert_eq!(
        add_err.reason(),
        "invalid IR contract: matrix product flat index addition overflows host index range"
            .to_string()
    );

    Ok(())
}

#[test]
fn checked_projection_offset_dummy_span_stays_unspanned() {
    let err = checked_projection_offset(
        usize::MAX,
        2,
        0,
        "matrix product flat index",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("overflowing projection offset multiplication must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy projection offset span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("multiplication overflows"));
}

#[test]
fn checked_usize_dims_to_i64_dummy_span_stays_unspanned() {
    let err = checked_usize_dims_to_i64(
        &[usize::MAX],
        "array expression dimension",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("dimension must fit in Modelica integer range");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy dimension span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("exceeds i64 range"));
}

#[test]
fn reserve_projection_capacity_dummy_span_stays_unspanned() {
    let mut values = Vec::<ProjectedFunctionOutput>::new();
    let err = reserve_projection_capacity(
        &mut values,
        usize::MAX,
        "projected output count",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("impossible projection capacity must be rejected");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy projection capacity span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("capacity exceeds host memory limits"));
}

#[test]
fn scalar_count_rejects_host_index_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_52.mo",
        ),
        1,
        9,
    );

    let err = scalar_count_for_dims(&[i64::MAX, i64::MAX], "projected value dimensions", span)
        .expect_err("overflowing scalar count must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(err.reason().contains("projected value dimensions"));
}

#[test]
fn scalar_count_dummy_span_stays_unspanned() {
    let err = scalar_count_for_dims(
        &[i64::MAX, i64::MAX],
        "projected value dimensions",
        rumoca_core::Span::DUMMY,
    )
    .expect_err("overflowing scalar count must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "dummy scalar-count span should not be fabricated into a source span: {err:?}"
    );
    assert!(err.reason().contains("projected value dimensions"));
}

#[test]
fn flat_index_rejects_host_index_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_53.mo",
        ),
        1,
        9,
    );

    let err = flat_index_from_indices(
        &[i64::MAX, i64::MAX],
        &[i64::MAX, i64::MAX],
        span,
        "projected scalar selection flat index",
    )
    .expect_err("overflowing flat index must fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason()
            .contains("projected scalar selection flat index")
    );
}

#[test]
fn matrix_matrix_projection_with_zero_columns_declines() -> Result<(), String> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_49.mo",
        ),
        1,
        9,
    );
    let ctx = ProjectionValueCtx {
        dims: &[],
        flat_index: 0,
        scope: &scope,
        depth: 0,
        span,
    };

    let projected = analysis
        .project_matrix_matrix_product(&real(1.0), &real(1.0), &[1, 1], &[1, 0], &ctx, 0)
        .map_err(|err| format!("zero-column matrix projection failed: {err:?}"))?;
    if projected.is_some() {
        return Err("zero-column matrix projection produced a scalar value".to_string());
    }

    Ok(())
}

#[test]
fn project_reference_indices_preserves_indexed_component_parts() {
    let reference = component_reference(vec![
        rumoca_core::ComponentRefPart {
            ident: "vehicle".to_string(),
            span: test_span(),
            subs: Vec::new(),
        },
        rumoca_core::ComponentRefPart {
            ident: "motor".to_string(),
            span: test_span(),
            subs: vec![rumoca_core::Subscript::generated_index(1, test_span())],
        },
        rumoca_core::ComponentRefPart {
            ident: "history".to_string(),
            span: test_span(),
            subs: Vec::new(),
        },
    ]);

    let projected = project_reference_field_path_and_indices(&reference, &[], &[2], test_span())
        .expect("structured reference projection should succeed");

    let component_ref = projected
        .component_ref()
        .expect("projected reference should preserve component-reference structure");
    assert_eq!(projected.as_str(), "vehicle.motor[1].history[2]");
    assert_eq!(component_ref.parts[1].ident, "motor");
    assert_eq!(component_ref.parts[1].subs.len(), 1);
    assert_eq!(component_ref.parts[2].ident, "history");
    assert_eq!(component_ref.parts[2].subs.len(), 1);
}

#[test]
fn project_reference_indices_rejects_i64_overflow_with_span() {
    let Some(index) = usize::try_from(i64::MAX)
        .ok()
        .and_then(|value| value.checked_add(1))
    else {
        return;
    };
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_46.mo",
        ),
        6,
        14,
    );
    let reference = component_reference(vec![rumoca_core::ComponentRefPart {
        ident: "x".to_string(),
        span,
        subs: Vec::new(),
    }]);

    let err = project_reference_field_path_and_indices(&reference, &[], &[index], span)
        .expect_err("projected reference index must fit in Modelica integer range");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        format!(
            "invalid IR contract: function output projection subscript index {index} exceeds i64 range"
        )
    );
}

fn scalar_function_param(name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        def_id: None,
        name: name.to_string(),
        span: test_span(),
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![],
        shape_expr: Vec::new(),
        default: None,
        description: None,
    }
}

fn function_param_with_dims(name: &str, dims: &[i64]) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        dims: dims.to_vec(),
        ..scalar_function_param(name)
    }
}

fn real_with_span(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn function_param_with_type(name: &str, type_name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        type_name: type_name.to_string(),
        ..scalar_function_param(name)
    }
}

fn record_function_param(name: &str, type_name: &str) -> rumoca_core::FunctionParam {
    rumoca_core::FunctionParam {
        type_class: Some(rumoca_core::ClassType::Record),
        ..function_param_with_type(name, type_name)
    }
}

#[test]
fn vector_function_input_rejects_scalar_actual_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsVector", test_span());
    function.inputs.push(function_param_with_dims("u", &[2]));
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_52.mo",
        ),
        3,
        8,
    );

    let err = match analysis.bind_inputs(&function, &[real_with_span(1.0, span)], 0, span) {
        Ok(_) => panic!("scalar actual must not be projected as vector input"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.needsVector` input `u` expects dimensions [2], got []"
    );
}

#[test]
fn dynamic_vector_function_input_uses_actual_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsDynamicVector", test_span());
    function.inputs.push(function_param_with_dims("u", &[0]));

    let scope = analysis
        .bind_inputs(
            &function,
            &[array(vec![real(1.0), real(2.0), real(3.0)], false)],
            0,
            test_span(),
        )
        .expect("dynamic vector input projection should not fail")
        .expect("dynamic vector input should bind");

    assert_eq!(scope.dims.get("u"), Some(&vec![3]));
    assert_eq!(scope.scalars.get("u").map(Vec::len), Some(3));
}

#[test]
fn compile_time_if_selection_uses_projected_size_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.selectBySize", test_span());
    function.inputs.push(function_param_with_dims("u", &[0]));
    let scope = analysis
        .bind_inputs(
            &function,
            &[array(vec![real(1.0), real(2.0), real(3.0)], false)],
            0,
            test_span(),
        )
        .expect("dynamic vector input projection should not fail")
        .expect("dynamic vector input should bind");
    let condition = binary(
        rumoca_core::OpBinary::Eq,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![var_ref("u"), integer(1)],
            span: test_span(),
        },
        integer(3),
        test_span(),
    );
    let selected_branch = real(10.0);
    let else_branch = real(20.0);
    let branches = vec![(condition, selected_branch.clone())];

    let selected = analysis
        .compile_time_if_selection(&branches, &else_branch, &scope)
        .expect("compile-time if selection should not fail")
        .expect("size(u, 1) should select a branch");

    assert_eq!(selected, &selected_branch);
}

#[test]
fn compile_time_size_uses_stream_variable_dimensions() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("Xi"),
        dae::Variable {
            name: rumoca_core::VarName::new("Xi"),
            dims: vec![1],
            ..rumoca_ir_dae::Variable::empty_with_span(test_span())
        },
    );
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("inStream").into(),
                args: vec![var_ref("Xi")],
                is_constructor: false,
                span: test_span(),
            },
            integer(1),
        ],
        span: test_span(),
    };

    assert_eq!(
        analysis
            .compile_time_scalar_in_scope(&expr, &FunctionProjectionScope::default())
            .expect("stream size should not fail"),
        Some(1.0)
    );
}

#[test]
fn array_like_projection_expands_stream_size_selected_cat() -> Result<(), LowerError> {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("Xi"),
        dae::Variable {
            name: rumoca_core::VarName::new("Xi"),
            dims: vec![1],
            ..rumoca_ir_dae::Variable::empty_with_span(test_span())
        },
    );
    let structural_bindings = IndexMap::new();
    let xi_stream = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("inStream").into(),
        args: vec![var_ref("Xi")],
        is_constructor: false,
        span: test_span(),
    };
    let cat = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cat,
        args: vec![
            integer(1),
            xi_stream.clone(),
            array(
                vec![binary(
                    rumoca_core::OpBinary::Sub,
                    integer(1),
                    rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Sum,
                        args: vec![xi_stream.clone()],
                        span: test_span(),
                    },
                    test_span(),
                )],
                false,
            ),
        ],
        span: test_span(),
    };
    let condition = binary(
        rumoca_core::OpBinary::Eq,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![xi_stream.clone(), integer(1)],
            span: test_span(),
        },
        integer(2),
        test_span(),
    );
    let branches = vec![(condition, xi_stream.clone())];
    let expr = rumoca_core::Expression::If {
        branches: branches.clone(),
        else_branch: Box::new(cat.clone()),
        span: test_span(),
    };
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    assert_eq!(
        analysis.compile_time_if_selection(&branches, &cat, &FunctionProjectionScope::default())?,
        Some(&cat)
    );
    assert!(
        analysis
            .project_value_scalars(
                &cat,
                &[2],
                &FunctionProjectionScope::default(),
                0,
                test_span()
            )?
            .is_some()
    );
    assert_eq!(
        analysis.expr_dims(&expr, &FunctionProjectionScope::default(), 0, test_span())?,
        Some(vec![2])
    );

    let values = project_array_like_scalars_with_owner(
        &expr,
        &dae_model,
        &structural_bindings,
        test_span(),
    )?
    .expect("stream-size-selected cat should expand");

    assert_eq!(values.len(), 2);
    assert_var_ref_name(&values[0], "Xi[1]");
    assert!(matches!(
        &values[1],
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            ..
        }
    ));
    Ok(())
}

#[test]
fn scalar_real_function_input_rejects_vector_actual_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsScalar", test_span());
    function.inputs.push(scalar_function_param("u"));
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_54.mo",
        ),
        7,
        13,
    );

    let err = match analysis.bind_inputs(
        &function,
        &[rumoca_core::Expression::Array {
            elements: vec![real(1.0), real(2.0)],
            is_matrix: false,
            span,
        }],
        0,
        span,
    ) {
        Ok(_) => panic!("vector actual must not be projected as scalar Real input"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.needsScalar` input `u` expects dimensions [], got [2]"
    );
}

#[test]
fn scalar_real_function_input_accepts_singleton_vector_actual() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsScalar", test_span());
    function.inputs.push(scalar_function_param("u"));

    let scope = analysis
        .bind_inputs(&function, &[array(vec![real(1.0)], false)], 0, test_span())
        .expect("single scalar vector actual should bind")
        .expect("single scalar vector actual should project");

    assert_eq!(scope.dims.get("u"), Some(&vec![1]));
    assert_eq!(scope.scalars.get("u").map(Vec::len), Some(1));
}

#[test]
fn generated_function_call_projection_errors_use_owner_span() {
    let owner_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_53.mo",
        ),
        3,
        8,
    );
    let mut dae_model = dae::Dae::default();
    let mut function = rumoca_core::Function::new("My.needsVector", test_span());
    function.inputs.push(rumoca_core::FunctionParam {
        span: owner_span,
        ..function_param_with_dims("u", &[2])
    });
    function.outputs.push(scalar_function_param("y"));
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);
    let structural_bindings = IndexMap::new();
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.needsVector").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    };

    let err = function_call_projected_scalars_with_owner(
        &call,
        &dae_model,
        &structural_bindings,
        owner_span,
    )
    .expect_err("generated invalid projection must report an error");

    assert_eq!(err.source_span(), Some(owner_span));
    assert_eq!(
        err.reason(),
        "function `My.needsVector` input `u` expects dimensions [2], got []"
    );
}

#[test]
fn projected_record_field_expands_dynamic_cat_output() -> Result<(), LowerError> {
    let mut dae_model = dae::Dae::default();
    let mut state_ctor = rumoca_core::Function::new("My.State", test_span());
    state_ctor.is_constructor = true;
    state_ctor.pure = true;
    state_ctor.inputs.push(function_param_with_dims("X", &[0]));
    state_ctor
        .outputs
        .push(record_function_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(state_ctor.name.clone(), state_ctor);

    let mut set_state = rumoca_core::Function::new("My.setState", test_span());
    set_state.pure = true;
    set_state.inputs.push(function_param_with_dims("X", &[0]));
    set_state
        .outputs
        .push(record_function_param("state", "My.State"));
    let x_value = rumoca_core::Expression::If {
        branches: vec![(
            binary(
                rumoca_core::OpBinary::Eq,
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Size,
                    args: vec![var_ref("X"), integer(1)],
                    span: test_span(),
                },
                integer(2),
                test_span(),
            ),
            var_ref("X"),
        )],
        else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args: vec![
                integer(1),
                var_ref("X"),
                array(
                    vec![binary(
                        rumoca_core::OpBinary::Sub,
                        integer(1),
                        rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Sum,
                            args: vec![var_ref("X")],
                            span: test_span(),
                        },
                        test_span(),
                    )],
                    false,
                ),
            ],
            span: test_span(),
        }),
        span: test_span(),
    };
    set_state.body.push(scalar_assignment(
        "state",
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.State").into(),
            args: vec![rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("__rumoca_named_arg__.X").into(),
                args: vec![x_value],
                is_constructor: true,
                span: test_span(),
            }],
            is_constructor: true,
            span: test_span(),
        },
    ));
    dae_model
        .symbols
        .functions
        .insert(set_state.name.clone(), set_state);
    let structural_bindings = IndexMap::new();
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.setState").into(),
            args: vec![array(vec![real(0.25)], false)],
            is_constructor: false,
            span: test_span(),
        }),
        field: "X".to_string(),
        span: test_span(),
    };

    let values = function_call_projected_scalars_with_owner(
        &expr,
        &dae_model,
        &structural_bindings,
        test_span(),
    )?
    .expect("projected record field should expand");

    assert_eq!(values.len(), 2);
    assert!(matches!(
        &values[0],
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            ..
        } if (*value - 0.25).abs() < 1e-12
    ));
    assert!(matches!(
        &values[1],
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            ..
        }
    ));
    Ok(())
}

#[test]
fn projected_record_field_expands_stream_variable_cat_output() -> Result<(), LowerError> {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("Xi"),
        dae::Variable {
            name: rumoca_core::VarName::new("Xi"),
            dims: vec![1],
            ..rumoca_ir_dae::Variable::empty_with_span(test_span())
        },
    );
    let mut state_ctor = rumoca_core::Function::new("My.State", test_span());
    state_ctor.is_constructor = true;
    state_ctor.pure = true;
    state_ctor.inputs.push(function_param_with_dims("X", &[0]));
    state_ctor
        .outputs
        .push(record_function_param("state", "My.State"));
    dae_model
        .symbols
        .functions
        .insert(state_ctor.name.clone(), state_ctor);

    let mut set_state = rumoca_core::Function::new("My.setState", test_span());
    set_state.pure = true;
    set_state.inputs.push(function_param_with_dims("X", &[0]));
    set_state
        .outputs
        .push(record_function_param("state", "My.State"));
    let x_value = rumoca_core::Expression::If {
        branches: vec![(
            binary(
                rumoca_core::OpBinary::Eq,
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Size,
                    args: vec![var_ref("X"), integer(1)],
                    span: test_span(),
                },
                integer(2),
                test_span(),
            ),
            var_ref("X"),
        )],
        else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Cat,
            args: vec![
                integer(1),
                var_ref("X"),
                array(
                    vec![binary(
                        rumoca_core::OpBinary::Sub,
                        integer(1),
                        rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Sum,
                            args: vec![var_ref("X")],
                            span: test_span(),
                        },
                        test_span(),
                    )],
                    false,
                ),
            ],
            span: test_span(),
        }),
        span: test_span(),
    };
    set_state.body.push(scalar_assignment(
        "state",
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.State").into(),
            args: vec![rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("__rumoca_named_arg__.X").into(),
                args: vec![x_value],
                is_constructor: true,
                span: test_span(),
            }],
            is_constructor: true,
            span: test_span(),
        },
    ));
    dae_model
        .symbols
        .functions
        .insert(set_state.name.clone(), set_state);
    let structural_bindings = IndexMap::new();
    let xi_stream = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("inStream").into(),
        args: vec![var_ref("Xi")],
        is_constructor: false,
        span: test_span(),
    };
    let expr = rumoca_core::Expression::FieldAccess {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("My.setState").into(),
            args: vec![xi_stream.clone()],
            is_constructor: false,
            span: test_span(),
        }),
        field: "X".to_string(),
        span: test_span(),
    };

    let values = function_call_projected_scalars_with_owner(
        &expr,
        &dae_model,
        &structural_bindings,
        test_span(),
    )?
    .expect("projected stream record field should expand");

    assert_eq!(values.len(), 2);
    assert_var_ref_name(&values[0], "Xi[1]");
    assert!(matches!(
        &values[1],
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            ..
        }
    ));
    Ok(())
}

#[test]
fn record_like_function_input_accepts_structured_actual_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.needsRecord", test_span());
    function
        .inputs
        .push(function_param_with_type("q", "Pkg.Quaternion"));

    let scope = analysis
        .bind_inputs(
            &function,
            &[array(
                vec![real(1.0), real(2.0), real(3.0), real(4.0)],
                false,
            )],
            0,
            test_span(),
        )
        .expect("record-like input binding should not fail")
        .expect("record-like input should bind");

    assert_eq!(scope.dims.get("q"), Some(&vec![4]));
    assert_eq!(scope.scalars.get("q").map(Vec::len), Some(4));
}

#[test]
fn function_projection_initializes_array_local_from_declaration_binding() -> Result<(), LowerError>
{
    let mut dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let mut function = rumoca_core::Function::new("My.localDefault", test_span());
    function.outputs.push(function_param_with_dims("y", &[3]));
    let mut local = function_param_with_dims("x", &[3]);
    local.default = Some(array(vec![real(1.0), real(2.0), real(3.0)], false));
    function.locals.push(local);
    function.body.push(scalar_assignment("y", local_var("x")));
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.localDefault").into(),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };

    let projected = function_call_projected_scalars_with_owner(
        &call,
        &dae_model,
        &structural_bindings,
        test_span(),
    )?
    .expect("function call should project declaration-bound array local");
    let values = projected
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::Literal {
                value: Literal::Real(value),
                ..
            } => *value,
            other => panic!("expected real literal projection, got {other:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(values, vec![1.0, 2.0, 3.0]);
    Ok(())
}

#[test]
fn vector_constructor_input_rejects_scalar_actual_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let input = function_param_with_dims("u", &[2]);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_53.mo",
        ),
        5,
        11,
    );
    let actual = real_with_span(1.0, span);

    let err = analysis
        .optional_constructor_input_scalars(&actual, &input, &scope, 0, span)
        .expect_err("scalar actual must not be projected as vector constructor input");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "record constructor input `u` expects dimensions [2], got []"
    );
}

#[test]
fn scalar_constructor_input_uses_formal_scalar_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let input = scalar_function_param("u");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_57.mo",
        ),
        5,
        11,
    );
    let actual = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("runtime_scalar"),
        subscripts: Vec::new(),
        span,
    };

    let (dims, scalars) = analysis
        .optional_constructor_input_scalars(&actual, &input, &scope, 0, span)
        .expect("scalar constructor input projection should not fail")
        .expect("scalar primitive input should project as a scalar");

    assert!(dims.is_empty());
    assert_eq!(scalars.len(), 1);
}

#[test]
fn record_like_constructor_input_declines_unknown_actual_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let input = function_param_with_type("q", "Pkg.Quaternion");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_58.mo",
        ),
        6,
        14,
    );
    let actual = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("runtime_record"),
        subscripts: Vec::new(),
        span,
    };

    let scalars = analysis
        .optional_constructor_input_scalars(&actual, &input, &scope, 0, span)
        .expect("unknown record-like constructor input dimensions should decline");

    assert!(scalars.is_none());
}

#[test]
fn if_projection_rejects_scalar_values_without_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let entry_scope = FunctionProjectionScope::default();
    let mut else_scope = FunctionProjectionScope::default();
    else_scope.scalars.insert("x".to_string(), vec![real(0.0)]);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_59.mo",
        ),
        7,
        18,
    );

    let err = match analysis.merged_if_scope(&entry_scope, &[], &[], &else_scope, span) {
        Ok(_) => panic!("merged scalar projection without dimensions must fail"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: if-statement projection for `x` has scalar values but no dimensions"
    );
}

#[test]
fn if_projection_rejects_conflicting_branch_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut entry_scope = FunctionProjectionScope::default();
    entry_scope
        .scalars
        .insert("x".to_string(), vec![real(0.0), real(0.0)]);
    entry_scope.dims.insert("x".to_string(), vec![2]);
    let mut branch_scope = entry_scope.clone();
    branch_scope.dims.insert("x".to_string(), vec![1, 2]);
    let condition = real(1.0);
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_60.mo",
        ),
        8,
        21,
    );

    let err = match analysis.merged_if_scope(
        &entry_scope,
        &[condition],
        &[branch_scope],
        &entry_scope,
        span,
    ) {
        Ok(_) => panic!("merged scalar projection with conflicting dimensions must fail"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "invalid IR contract: if-statement projection for `x` has mismatched dimensions: [2] and [1, 2]"
    );
}

fn scalar_assignment(target: &str, value: rumoca_core::Expression) -> rumoca_core::Statement {
    assignment_with_span(target, value, test_span())
}

fn assignment_with_span(
    target: &str,
    value: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: target.to_string(),
                span,
                subs: Vec::new(),
            }],
            def_id: None,
        },
        value,
        span,
    }
}

#[test]
fn vector_assignment_rejects_scalar_value_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.badAssign", test_span());
    function.locals.push(function_param_with_dims("x", &[2]));
    let mut scope = FunctionProjectionScope::default();
    let mut projected = Vec::new();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_55.mo",
        ),
        2,
        9,
    );
    let statement = assignment_with_span("x", real_with_span(1.0, span), span);

    let err = analysis
        .apply_assignment(&function, &statement, &mut scope, &mut projected, 0, span)
        .expect_err("scalar assignment to vector local must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.badAssign` assignment to `x` expects dimensions [2], got []"
    );
}

#[test]
fn scalar_assignment_rejects_vector_value_with_span() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut function = rumoca_core::Function::new("My.badAssign", test_span());
    function.locals.push(scalar_function_param("x"));
    let mut scope = FunctionProjectionScope::default();
    let mut projected = Vec::new();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_56.mo",
        ),
        4,
        12,
    );
    let statement = assignment_with_span(
        "x",
        rumoca_core::Expression::Array {
            elements: vec![real(1.0), real(2.0)],
            is_matrix: false,
            span,
        },
        span,
    );

    let err = analysis
        .apply_assignment(&function, &statement, &mut scope, &mut projected, 0, span)
        .expect_err("vector assignment to scalar local must fail");

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "function `My.badAssign` assignment to `x` expects dimensions [], got [2]"
    );
}

#[test]
fn unassigned_projected_scalar_reports_projection_span() -> Result<(), String> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_57.mo",
        ),
        6,
        15,
    );
    let values = vec![rumoca_core::Expression::Empty { span }];

    let Err(err) = assigned_projected_scalar_value("x", &[1], &values, 0, span) else {
        return Err("unassigned projected scalar slot succeeded".to_string());
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "projected local component `x[1]` is unassigned"
    );
    Ok(())
}

#[test]
fn scalar_selector_rejects_colon_with_subscript_span() -> Result<(), String> {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let scope = FunctionProjectionScope::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_58.mo",
        ),
        2,
        3,
    );
    let subscript = rumoca_core::Subscript::colon(span);

    let Err(err) = subscript_selector_expr(&subscript, &analysis, &scope, 0) else {
        return Err("colon scalar selector succeeded".to_string());
    };

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "colon subscript cannot select a scalar projected value"
    );
    Ok(())
}

#[test]
fn guarded_assignment_without_base_reports_assignment_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_derivative_rhs_function_projection_tests_source_59.mo",
        ),
        9,
        21,
    );

    let err = guarded_assignment_without_base("y", span);

    assert_eq!(err.source_span(), Some(span));
    assert_eq!(
        err.reason(),
        "if-statement assignment to `y` requires an existing binding or an else assignment"
    );
}

fn local_var(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

/// A pure function whose projected output doubles in size per statement,
/// crossing `MAX_FUNCTION_PROJECTION_NODES` long before it finishes.
fn over_budget_function() -> rumoca_core::Function {
    let mut body = vec![scalar_assignment(
        "y",
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(local_var("x")),
            rhs: Box::new(local_var("x")),
            span: test_span(),
        },
    )];
    for _ in 0..16 {
        body.push(scalar_assignment(
            "y",
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(local_var("y")),
                rhs: Box::new(local_var("y")),
                span: test_span(),
            },
        ));
    }
    rumoca_core::Function {
        name: rumoca_core::VarName::new("My.explode"),
        def_id: None,
        inputs: vec![scalar_function_param("x")],
        outputs: vec![scalar_function_param("y")],
        locals: vec![],
        body,
        is_constructor: false,
        pure: true,
        external: None,
        derivatives: vec![],
        span: test_span(),
    }
}

#[test]
fn over_budget_projection_is_a_typed_error_and_declines_at_the_boundary() {
    let mut dae_model = dae::Dae::default();
    dae_model.symbols.functions.insert(
        rumoca_core::VarName::new("My.explode"),
        over_budget_function(),
    );
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.explode").into(),
        args: vec![real(2.0)],
        is_constructor: false,
        span: test_span(),
    };

    let err = analysis
        .function_call_outputs_with_owner(&call, 0, test_span())
        .expect_err("over-budget projection must surface as a typed error");
    assert!(err.is_projection_budget_exceeded(), "got: {err:?}");
    assert!(err.reason().contains("My.explode"), "got: {}", err.reason());

    // The outermost boundary resolves the decline by keeping the runtime
    // call; the memoized decline must answer follow-up probes identically.
    for _ in 0..2 {
        let outputs = analysis
            .top_level_function_call_outputs(&call, test_span())
            .expect("budget decline must not fail the outer lowering");
        assert!(outputs.is_none());
    }
}

#[test]
fn projection_declines_when_output_leaks_function_local_reference() {
    let mut function = rumoca_core::Function::new("My.leaksLocal", test_span());
    function.outputs.push(scalar_function_param("y"));
    function
        .locals
        .push(function_param_with_type("scratch", "Pkg.Record"));
    function.body.push(scalar_assignment(
        "y",
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("scratch.value"),
            subscripts: Vec::new(),
            span: test_span(),
        },
    ));

    let mut dae_model = dae::Dae::default();
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.leaksLocal").into(),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };

    let outputs = analysis
        .top_level_function_call_outputs(&call, test_span())
        .expect("local leakage should decline optional projection");

    assert!(outputs.is_none());
}

#[test]
fn projection_allows_input_actual_with_same_name_as_formal() {
    let mut function = rumoca_core::Function::new("My.sameName", test_span());
    function.inputs.push(scalar_function_param("T"));
    function.outputs.push(scalar_function_param("y"));
    function.body.push(scalar_assignment("y", local_var("T")));

    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("T"),
        dae::Variable {
            name: rumoca_core::VarName::new("T"),
            ..rumoca_ir_dae::Variable::empty_with_span(test_span())
        },
    );
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("My.sameName").into(),
        args: vec![local_var("T")],
        is_constructor: false,
        span: test_span(),
    };

    let outputs = analysis
        .top_level_function_call_outputs(&call, test_span())
        .expect("same-name formal and actual should not fail projection")
        .expect("same-name actual should remain projectable");

    assert_eq!(outputs.len(), 1);
}
