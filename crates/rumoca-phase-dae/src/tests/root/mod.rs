use super::*;

mod input_binding_tests;
mod input_promotion_extra;
mod input_promotion_indexed;
mod tests_equation_classification;
mod tests_flow_sum;
mod tests_function_param_calls;
mod tests_regressions;

/// Helper to create a rumoca_core::ComponentReference from a simple name.
fn make_comp_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: crate::test_support::test_span(),
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: crate::test_support::test_span(),
            subs: vec![],
        }],
        def_id: None,
    }
}

/// Helper to create an assignment statement.
fn make_assignment(name: &str) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: make_comp_ref(name),
        value: rumoca_core::Expression::Empty {
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
    }
}
/// Helper to create a when statement with assignments.
fn make_when_stmt(names: &[&str]) -> rumoca_core::Statement {
    let stmts: Vec<_> = names.iter().map(|n| make_assignment(n)).collect();
    rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Empty {
                span: crate::test_support::test_span(),
            },
            stmts,
        }],
        span: crate::test_support::test_span(),
    }
}
fn make_var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: crate::test_support::test_span(),
    }
}

fn find_equation_defined_inputs_for_test(flat: &flat::Model) -> std::collections::HashSet<VarName> {
    let internal_inputs =
        InternalInputIndex::new(flat).expect("fixture internal input index should build");
    find_equation_defined_inputs(flat, &internal_inputs)
}
/// Connection references shaped like flatten output: structured component
/// parts with parsed subscripts, never name-encoded paths.
fn make_structured_var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(
            rumoca_core::component_reference_from_flat_name(
                &VarName::new(name),
                crate::test_support::test_span(),
            )
            .expect("fixture name must form a component reference"),
        ),
        subscripts: vec![],
        span: crate::test_support::test_span(),
    }
}

fn add_connection_equation(flat: &mut Model, lhs: &str, rhs: &str) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_structured_var_ref(lhs)),
            rhs: Box::new(make_structured_var_ref(rhs)),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: lhs.to_string(),
            rhs: rhs.to_string(),
        },
        scalar_count: 1,
    });
}

fn add_component_equation(flat: &mut Model, lhs: &str, rhs: rumoca_core::Expression) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_structured_var_ref(lhs)),
            rhs: Box::new(rhs),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
        scalar_count: 1,
    });
}

fn add_primitive_real(flat: &mut Model, name: &str) {
    flat.add_variable(
        VarName::new(name),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new(name),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
}

fn add_scalar_ode_with_rhs_call(flat: &mut Model, state_name: &str, call_name: &str) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref(state_name)],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: VarName::new(call_name).into(),
                args: vec![make_var_ref(state_name)],
                is_constructor: false,
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });
}

#[test]
fn test_todae_rewrites_missing_scoped_parameter_start_reference() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("nT"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("nT"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.577350269),
                span: crate::test_support::test_span(),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    flat.add_variable(
        VarName::new("idealTransformer.idealTransformer[1].n"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("idealTransformer.idealTransformer[1].n"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(make_var_ref("idealTransformer.nT")),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for parameter-only model");

    let nested = dae
        .variables
        .parameters
        .get(&rumoca_core::VarName::new(
            "idealTransformer.idealTransformer[1].n",
        ))
        .expect("missing nested transformer parameter");
    let start = nested
        .start
        .as_ref()
        .expect("nested transformer parameter should keep rewritten start expression");

    match start {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert!(
                subscripts.is_empty(),
                "expected scalar rewritten start reference"
            );
            assert_eq!(name.as_str(), "nT");
        }
        other => panic!("expected VarRef start expression, got {other:?}"),
    }
}

#[test]
fn test_todae_rewrites_misqualified_record_parameter_alias_field() {
    let mut flat = Model::new();

    flat.add_variable(
        VarName::new("aimcData.statorCoreParameters.wRef"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("aimcData.statorCoreParameters.wRef"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(314.159),
                span: crate::test_support::test_span(),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    flat.add_variable(
        VarName::new("aimc.statorCoreParameters.wRef"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("aimc.statorCoreParameters.wRef"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(make_var_ref("aimc.aimcData.statorCoreParameters")),
                field: "wRef".to_string(),
                span: crate::test_support::test_span(),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("record-parameter alias should resolve to the matching scalar field");

    let nested = dae
        .variables
        .parameters
        .get(&rumoca_core::VarName::new("aimc.statorCoreParameters.wRef"))
        .expect("missing nested stator core parameter");
    let start = nested
        .start
        .as_ref()
        .expect("nested stator core parameter should keep rewritten start expression");

    match start {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            assert!(
                subscripts.is_empty(),
                "expected scalar rewritten record-field reference"
            );
            assert_eq!(name.as_str(), "aimcData.statorCoreParameters.wRef");
        }
        other => panic!("expected VarRef start expression, got {other:?}"),
    }
}

#[test]
fn test_todae_rejects_unresolved_function_calls() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: VarName::new("missingFn").into(),
                args: vec![make_var_ref("x")],
                is_constructor: false,
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("missingFn should fail in ToDae validation");

    assert!(
        matches!(err, ToDaeError::UnresolvedFunctionCall { ref name, .. } if name == "missingFn"),
        "expected unresolved function diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_rejects_unresolved_references() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(make_var_ref("missingRef")),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("missingRef should fail in ToDae validation");

    assert!(
        matches!(err, ToDaeError::UnresolvedReference { ref name, .. } if name == "missingRef"),
        "expected unresolved reference diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_rejects_unresolved_component_qualified_constant_like_ref() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(make_var_ref("HeatingDiode1.k")),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("unresolved qualified reference should fail in ToDae validation");

    assert!(
        matches!(err, ToDaeError::UnresolvedReference { ref name, .. } if name == "HeatingDiode1.k"),
        "expected unresolved reference diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_rejects_non_external_function_without_body() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let stub = rumoca_core::Function::new("f", crate::test_support::test_span());
    flat.add_function(stub);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: VarName::new("f").into(),
                args: vec![make_var_ref("x")],
                is_constructor: false,
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("empty non-external function should fail in ToDae validation");

    assert!(
        matches!(err, ToDaeError::FunctionWithoutBody { ref name, .. } if name == "f"),
        "expected function-without-body diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_rejects_reinit_on_non_state_variable() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_primitive_real(&mut flat, "trigger");

    let mut when_clause =
        rumoca_ir_flat::WhenClause::new(make_var_ref("trigger"), crate::test_support::test_span());
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::reinit(
        VarName::new("x"),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
        "reinit x",
    ));
    flat.when_clauses.push(when_clause);

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("reinit(x, ...) must fail when x is not a state");

    assert!(
        matches!(err, ToDaeError::ReinitNonState { ref name, .. } if name == "x"),
        "expected reinit-non-state diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_ignores_unreachable_function_without_body() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let stub = rumoca_core::Function::new("f_unused", crate::test_support::test_span());
    flat.add_function(stub);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(make_var_ref("x")),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("unreachable empty function bodies should not fail ToDae validation");
}

#[test]
fn test_todae_accepts_unique_member_style_function_call_by_leaf() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut fn_def = rumoca_core::Function::new(
        "Modelica.Mechanics.MultiBody.World.gravityAcceleration",
        crate::test_support::test_span(),
    );
    fn_def.body.push(rumoca_core::Statement::Return {
        span: crate::test_support::test_span(),
    });
    flat.add_function(fn_def);

    add_scalar_ode_with_rhs_call(&mut flat, "x", "world.gravityAcceleration");

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("unique leaf match should resolve member-style call for codegen export");
}

#[test]
fn test_todae_rejects_ambiguous_member_style_function_call_by_leaf() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    for name in [
        "Modelica.Mechanics.MultiBody.World.gravityAcceleration",
        "Modelica.Mechanics.MultiBody.Examples.gravityAcceleration",
    ] {
        let mut fn_def = rumoca_core::Function::new(name, crate::test_support::test_span());
        fn_def.body.push(rumoca_core::Statement::Return {
            span: crate::test_support::test_span(),
        });
        flat.add_function(fn_def);
    }

    add_scalar_ode_with_rhs_call(&mut flat, "x", "world.gravityAcceleration");

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("ambiguous leaf match should fail closed");

    assert!(
        matches!(
            err,
            ToDaeError::UnresolvedFunctionCall { ref name, .. }
                if name == "world.gravityAcceleration"
        ),
        "expected unresolved function diagnostic for ambiguous member-style call, got {err:?}"
    );
}

#[test]
fn test_todae_accepts_runtime_intrinsic_cardinality_call() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_scalar_ode_with_rhs_call(&mut flat, "x", "cardinality");

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("cardinality should be treated as runtime intrinsic during validation");
}

#[test]
fn test_todae_accepts_runtime_intrinsic_complex_call() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_scalar_ode_with_rhs_call(&mut flat, "x", "Complex");

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("Complex should be treated as runtime intrinsic during validation");
}

#[test]
fn test_todae_accepts_record_constructor_calls_for_known_type_names() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    flat.add_variable(
        VarName::new("state"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("state"),
            is_primitive: false,
            binding: Some(rumoca_core::Expression::FunctionCall {
                name: VarName::new("Common.BaseProps_Tpoly").into(),
                args: vec![],
                is_constructor: true,
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    flat.variable_type_names
        .insert(VarName::new("state"), "Common.BaseProps_Tpoly".to_string());

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("record constructor calls should be accepted for known type names");
}

#[test]
fn test_todae_rejects_constructor_field_selection_without_signature() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("My.Record.C").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(1.0),
                        span: crate::test_support::test_span(),
                    }],
                    is_constructor: true,
                    span: crate::test_support::test_span(),
                }),
                field: "badField".to_string(),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("missing constructor signature should fail in ToDae validation");

    assert!(
        matches!(
            err,
            ToDaeError::ConstructorFieldSelectionUnresolved { ref selection, .. }
                if selection.starts_with("My.Record.C.badField")
        ),
        "expected constructor selection diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_accepts_constructor_field_selection_with_signature() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut constructor = Function::new("My.Record.C", crate::test_support::test_span());
    constructor.add_input(rumoca_core::FunctionParam::new(
        "noiseMin",
        "Real",
        crate::test_support::test_span(),
    ));
    flat.add_function(constructor);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("My.Record.C").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(1.0),
                        span: crate::test_support::test_span(),
                    }],
                    is_constructor: true,
                    span: crate::test_support::test_span(),
                }),
                field: "noiseMin".to_string(),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("constructor selection should pass when signature includes selected field");
}

#[test]
fn test_todae_rejects_complex_constructor_selection_without_signature_metadata() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("Complex").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(1.0),
                        span: crate::test_support::test_span(),
                    }],
                    is_constructor: true,
                    span: crate::test_support::test_span(),
                }),
                field: "re".to_string(),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("constructor field selections require constructor signature metadata");

    assert!(
        matches!(
            err,
            ToDaeError::ConstructorFieldSelectionUnresolved { ref selection, .. }
                if selection.starts_with("Complex.re")
        ),
        "expected constructor selection diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_rejects_parameter_constructor_selection_in_final_dae_validation() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    flat.add_variable(
        VarName::new("p"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("p"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("My.Param.C").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(2.0),
                        span: crate::test_support::test_span(),
                    }],
                    is_constructor: true,
                    span: crate::test_support::test_span(),
                }),
                field: "gain".to_string(),
                span: crate::test_support::test_span(),
            }),
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: crate::test_support::test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: crate::test_support::test_span(),
            }),
            span: crate::test_support::test_span(),
        },
        span: crate::test_support::test_span(),
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("parameter constructor selection should fail in final DAE validation");

    assert!(
        matches!(
            err,
            ToDaeError::ConstructorFieldSelectionUnresolved { ref selection, .. }
                if selection == "My.Param.C.gain"
        ),
        "expected constructor selection diagnostic, got {err:?}"
    );
}

#[test]
fn test_insert_discrete_var_routes_discrete_type_to_discrete_valued() {
    let mut dae = Dae::new();
    let name = VarName::new("flag");
    let dae_var = Variable::new(
        flat_to_dae_var_name(&name),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    let flat_var = flat::Variable {
        name: name.clone(),
        is_discrete_type: true,
        is_primitive: true,
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };

    insert_discrete_var(&mut dae, &name, dae_var, &flat_var);

    assert!(
        dae.variables
            .discrete_valued
            .contains_key(&flat_to_dae_var_name(&name))
    );
    assert!(
        !dae.variables
            .discrete_reals
            .contains_key(&flat_to_dae_var_name(&name))
    );
}

#[test]
fn test_insert_discrete_var_routes_real_discrete_to_discrete_reals() {
    let mut dae = Dae::new();
    let name = VarName::new("x");
    let dae_var = Variable::new(
        flat_to_dae_var_name(&name),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    let flat_var = flat::Variable {
        name: name.clone(),
        variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
        is_primitive: true,
        ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(file!()),
            1,
            2,
        ))
    };

    insert_discrete_var(&mut dae, &name, dae_var, &flat_var);

    assert!(
        dae.variables
            .discrete_reals
            .contains_key(&flat_to_dae_var_name(&name))
    );
    assert!(
        !dae.variables
            .discrete_valued
            .contains_key(&flat_to_dae_var_name(&name))
    );
}

#[test]
fn test_todae_routes_explicit_discrete_integer_when_assignment_to_f_m() {
    let mut flat = Model::new();
    let name = VarName::new("mode");
    flat.add_variable(
        name.clone(),
        crate::test_support::with_component_ref(flat::Variable {
            name: name.clone(),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    let mut when_clause = rumoca_ir_flat::WhenClause::new(
        rumoca_core::Expression::Literal {
            value: Literal::Boolean(true),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
    );
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::assign(
        name.clone(),
        rumoca_core::Expression::Literal {
            value: Literal::Integer(1),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
        "test",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("explicit discrete integer when assignment should route to f_m");

    assert!(
        dae.variables
            .discrete_valued
            .contains_key(&flat_to_dae_var_name(&name))
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&flat_to_dae_var_name(&name))
    );
    assert_eq!(
        dae.discrete.valued_updates.len(),
        1,
        "expected one discrete-valued event equation"
    );
}

#[test]
fn test_todae_lowers_when_multi_output_function_call_to_selection_updates() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "seed");
    flat.add_variable(
        VarName::new("trigger"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("trigger"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    for name in ["noise.r_raw", "noise.state"] {
        flat.add_variable(
            VarName::new(name),
            crate::test_support::with_component_ref(flat::Variable {
                name: VarName::new(name),
                variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
                is_primitive: true,
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            }),
        );
    }

    let mut function = rumoca_core::Function::new("Noise.next", crate::test_support::test_span());
    function.add_input(rumoca_core::FunctionParam::new(
        "seed",
        "Real",
        crate::test_support::test_span(),
    ));
    function.add_output(rumoca_core::FunctionParam::new(
        "r_raw",
        "Real",
        crate::test_support::test_span(),
    ));
    function.add_output(rumoca_core::FunctionParam::new(
        "state",
        "Real",
        crate::test_support::test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("r_raw"),
        value: make_var_ref("seed"),
        span: crate::test_support::test_span(),
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("state"),
        value: make_var_ref("seed"),
        span: crate::test_support::test_span(),
    });
    flat.add_function(function);

    let mut when_clause =
        rumoca_ir_flat::WhenClause::new(make_var_ref("trigger"), crate::test_support::test_span());
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::function_call_outputs(
        vec![VarName::new("noise.r_raw"), VarName::new("noise.state")],
        rumoca_core::Expression::FunctionCall {
            name: VarName::new("Noise.next").into(),
            args: vec![make_var_ref("seed")],
            is_constructor: false,
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
        "noise update",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("when multi-output function call should lower to per-output updates");

    let updates = dae
        .discrete
        .real_updates
        .iter()
        .map(|eq| format!("{:?}", eq.rhs))
        .collect::<Vec<_>>();
    assert_eq!(updates.len(), 2);
    assert!(
        updates.iter().any(|rhs| rhs.contains("Noise.next.r_raw")),
        "missing first output selection in updates: {updates:?}"
    );
    assert!(
        updates.iter().any(|rhs| rhs.contains("Noise.next.state")),
        "missing second output selection in updates: {updates:?}"
    );
}

#[test]
fn test_todae_preserves_indexed_explicit_discrete_assignment_targets() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("auxiliary"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("auxiliary"),
            dims: vec![3],
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    for (index, value) in [(1, 1), (2, 2), (3, 3)] {
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: VarName::new("auxiliary").into(),
                    subscripts: vec![rumoca_core::Subscript::generated_index(
                        index,
                        crate::test_support::test_span(),
                    )],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
            span: crate::test_support::test_span(),
            origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
                component: format!("aux[{index}]"),
            },
            scalar_count: 1,
        });
    }

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("indexed discrete assignments should preserve their element targets in ToDae");

    let lhs_names: std::collections::HashSet<_> = dae
        .discrete
        .valued_updates
        .iter()
        .filter_map(|eq| eq.lhs.as_ref())
        .map(|name| name.as_str().to_string())
        .collect();

    assert_eq!(dae.discrete.valued_updates.len(), 3);
    assert!(
        lhs_names.contains("auxiliary[1]"),
        "first indexed target must stay explicit"
    );
    assert!(
        lhs_names.contains("auxiliary[2]"),
        "second indexed target must stay explicit"
    );
    assert!(
        lhs_names.contains("auxiliary[3]"),
        "third indexed target must stay explicit"
    );
}

#[test]
fn test_todae_canonicalizes_constant_expr_subscripts_in_explicit_targets() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("auxiliary"),
        crate::test_support::with_component_ref(flat::Variable {
            name: VarName::new("auxiliary"),
            dims: vec![3],
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );

    for (offset, value) in [(0, 1), (1, 2), (2, 3)] {
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: VarName::new("auxiliary").into(),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(
                        Box::new(rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span: crate::test_support::test_span(),
                            }),
                            rhs: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(offset),
                                span: crate::test_support::test_span(),
                            }),
                            span: crate::test_support::test_span(),
                        }),
                        crate::test_support::test_span(),
                    )],
                    span: crate::test_support::test_span(),
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    span: crate::test_support::test_span(),
                }),
                span: crate::test_support::test_span(),
            },
            span: crate::test_support::test_span(),
            origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
                component: format!("aux_expr[{offset}]"),
            },
            scalar_count: 1,
        });
    }

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("constant-expression indexed targets should canonicalize in ToDae");

    let lhs_names: std::collections::HashSet<_> = dae
        .discrete
        .valued_updates
        .iter()
        .filter_map(|eq| eq.lhs.as_ref())
        .map(|name| name.as_str().to_string())
        .collect();

    assert!(lhs_names.contains("auxiliary[1]"));
    assert!(lhs_names.contains("auxiliary[2]"));
    assert!(lhs_names.contains("auxiliary[3]"));
}

#[test]
fn test_todae_routes_explicit_discrete_real_when_assignment_to_f_z() {
    let mut flat = Model::new();
    let name = VarName::new("d");
    flat.add_variable(
        name.clone(),
        crate::test_support::with_component_ref(flat::Variable {
            name: name.clone(),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        }),
    );
    let mut when_clause = rumoca_ir_flat::WhenClause::new(
        rumoca_core::Expression::Literal {
            value: Literal::Boolean(true),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
    );
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::assign(
        name.clone(),
        rumoca_core::Expression::Literal {
            value: Literal::Real(0.0),
            span: crate::test_support::test_span(),
        },
        crate::test_support::test_span(),
        "test",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("discrete real when assignment should route to f_z");

    assert!(
        dae.variables
            .discrete_reals
            .contains_key(&flat_to_dae_var_name(&name))
    );
    assert!(
        !dae.variables
            .algebraics
            .contains_key(&flat_to_dae_var_name(&name))
    );
    assert_eq!(
        dae.discrete.real_updates.len(),
        1,
        "expected one discrete real event equation"
    );
}
