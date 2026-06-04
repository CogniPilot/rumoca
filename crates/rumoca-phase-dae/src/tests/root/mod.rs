use super::*;

/// Helper to create a rumoca_core::ComponentReference from a simple name.
fn make_comp_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
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
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }
}
/// Helper to create a when statement with assignments.
fn make_when_stmt(names: &[&str]) -> rumoca_core::Statement {
    let stmts: Vec<_> = names.iter().map(|n| make_assignment(n)).collect();
    rumoca_core::Statement::When {
        blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Empty {
                span: rumoca_core::Span::DUMMY,
            },
            stmts,
        }],
        span: rumoca_core::Span::DUMMY,
    }
}
fn make_var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: VarName::new(name).into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn find_equation_defined_inputs_for_test(flat: &flat::Model) -> std::collections::HashSet<VarName> {
    let internal_inputs = InternalInputIndex::new(flat);
    find_equation_defined_inputs(flat, &internal_inputs)
}
fn add_connection_equation(flat: &mut Model, lhs: &str, rhs: &str) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref(lhs)),
            rhs: Box::new(make_var_ref(rhs)),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
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
            lhs: Box::new(make_var_ref(lhs)),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
        scalar_count: 1,
    });
}

fn add_primitive_real(flat: &mut Model, name: &str) {
    flat.add_variable(
        VarName::new(name),
        flat::Variable {
            name: VarName::new(name),
            is_primitive: true,
            ..Default::default()
        },
    );
}

fn add_scalar_ode_with_rhs_call(flat: &mut Model, state_name: &str, call_name: &str) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref(state_name)],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: VarName::new(call_name).into(),
                args: vec![make_var_ref(state_name)],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
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
        flat::Variable {
            name: VarName::new("nT"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.577350269),
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.add_variable(
        VarName::new("idealTransformer.idealTransformer[1].n"),
        flat::Variable {
            name: VarName::new("idealTransformer.idealTransformer[1].n"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(make_var_ref("idealTransformer.nT")),
            is_primitive: true,
            ..Default::default()
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
        flat::Variable {
            name: VarName::new("aimcData.statorCoreParameters.wRef"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(314.159),
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.add_variable(
        VarName::new("aimc.statorCoreParameters.wRef"),
        flat::Variable {
            name: VarName::new("aimc.statorCoreParameters.wRef"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(make_var_ref("aimc.aimcData.statorCoreParameters")),
                field: "wRef".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..Default::default()
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: VarName::new("missingFn").into(),
                args: vec![make_var_ref("x")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(make_var_ref("missingRef")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(make_var_ref("HeatingDiode1.k")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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

    let stub = rumoca_core::Function::new("f", Span::DUMMY);
    flat.add_function(stub);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: VarName::new("f").into(),
                args: vec![make_var_ref("x")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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

    let mut when_clause = rumoca_ir_flat::WhenClause::new(make_var_ref("trigger"), Span::DUMMY);
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::reinit(
        VarName::new("x"),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "reinit x",
    ));
    flat.when_clauses.push(when_clause);

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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

    let stub = rumoca_core::Function::new("f_unused", Span::DUMMY);
    flat.add_function(stub);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(make_var_ref("x")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("unreachable empty function bodies should not fail ToDae validation");
}

#[test]
fn test_todae_rejects_member_style_function_call_without_resolved_name() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut fn_def = rumoca_core::Function::new(
        "Modelica.Mechanics.MultiBody.World.gravityAcceleration",
        Span::DUMMY,
    );
    fn_def.body.push(rumoca_core::Statement::Return {
        span: rumoca_core::Span::DUMMY,
    });
    flat.add_function(fn_def);

    add_scalar_ode_with_rhs_call(&mut flat, "x", "world.gravityAcceleration");

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect_err("member-style call should fail without prior name resolution");

    assert!(
        matches!(
            err,
            ToDaeError::UnresolvedFunctionCall { ref name, .. }
                if name == "world.gravityAcceleration"
        ),
        "expected unresolved function diagnostic for member-style call, got {err:?}"
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
            preserve_overridable_param_starts: false,
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
            preserve_overridable_param_starts: false,
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
        flat::Variable {
            name: VarName::new("state"),
            is_primitive: false,
            binding: Some(rumoca_core::Expression::FunctionCall {
                name: VarName::new("Common.BaseProps_Tpoly").into(),
                args: vec![],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
    flat.variable_type_names
        .insert(VarName::new("state"), "Common.BaseProps_Tpoly".to_string());

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("My.Record.C").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: true,
                    span: rumoca_core::Span::DUMMY,
                }),
                field: "badField".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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

    let mut constructor = Function::new("My.Record.C", Span::DUMMY);
    constructor.add_input(rumoca_core::FunctionParam::new("noiseMin", "Real"));
    flat.add_function(constructor);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("My.Record.C").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: true,
                    span: rumoca_core::Span::DUMMY,
                }),
                field: "noiseMin".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("Complex").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: true,
                    span: rumoca_core::Span::DUMMY,
                }),
                field: "re".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
        flat::Variable {
            name: VarName::new("p"),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            is_primitive: true,
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FunctionCall {
                    name: VarName::new("My.Param.C").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: Literal::Real(2.0),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: true,
                    span: rumoca_core::Span::DUMMY,
                }),
                field: "gain".to_string(),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "probe".to_string(),
        },
        scalar_count: 1,
    });

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
    let dae_var = Variable::new(flat_to_dae_var_name(&name));
    let flat_var = flat::Variable {
        name: name.clone(),
        is_discrete_type: true,
        is_primitive: true,
        ..Default::default()
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
    let dae_var = Variable::new(flat_to_dae_var_name(&name));
    let flat_var = flat::Variable {
        name: name.clone(),
        variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
        is_primitive: true,
        ..Default::default()
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
        flat::Variable {
            name: name.clone(),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    let mut when_clause = rumoca_ir_flat::WhenClause::new(
        rumoca_core::Expression::Literal {
            value: Literal::Boolean(true),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
    );
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::assign(
        name.clone(),
        rumoca_core::Expression::Literal {
            value: Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "test",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
        flat::Variable {
            name: VarName::new("trigger"),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    for name in ["noise.r_raw", "noise.state"] {
        flat.add_variable(
            VarName::new(name),
            flat::Variable {
                name: VarName::new(name),
                variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
                is_primitive: true,
                ..Default::default()
            },
        );
    }

    let mut function = rumoca_core::Function::new("Noise.next", Span::DUMMY);
    function.add_input(rumoca_core::FunctionParam::new("seed", "Real"));
    function.add_output(rumoca_core::FunctionParam::new("r_raw", "Real"));
    function.add_output(rumoca_core::FunctionParam::new("state", "Real"));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("r_raw"),
        value: make_var_ref("seed"),
        span: rumoca_core::Span::DUMMY,
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: make_comp_ref("state"),
        value: make_var_ref("seed"),
        span: rumoca_core::Span::DUMMY,
    });
    flat.add_function(function);

    let mut when_clause = rumoca_ir_flat::WhenClause::new(make_var_ref("trigger"), Span::DUMMY);
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::function_call_outputs(
        vec![VarName::new("noise.r_raw"), VarName::new("noise.state")],
        rumoca_core::Expression::FunctionCall {
            name: VarName::new("Noise.next").into(),
            args: vec![make_var_ref("seed")],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "noise update",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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
        flat::Variable {
            name: VarName::new("auxiliary"),
            dims: vec![3],
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    for (index, value) in [(1, 1), (2, 2), (3, 3)] {
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: VarName::new("auxiliary").into(),
                    subscripts: vec![rumoca_core::Subscript::generated_index(
                        index,
                        rumoca_core::Span::DUMMY,
                    )],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
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
            preserve_overridable_param_starts: false,
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
        flat::Variable {
            name: VarName::new("auxiliary"),
            dims: vec![3],
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    for (offset, value) in [(0, 1), (1, 2), (2, 3)] {
        flat.add_equation(rumoca_ir_flat::Equation {
            residual: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: VarName::new("auxiliary").into(),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(1),
                                span: rumoca_core::Span::DUMMY,
                            }),
                            rhs: Box::new(rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Integer(offset),
                                span: rumoca_core::Span::DUMMY,
                            }),
                            span: rumoca_core::Span::DUMMY,
                        },
                    ))],
                    span: rumoca_core::Span::DUMMY,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(value),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            span: Span::DUMMY,
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
            preserve_overridable_param_starts: false,
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
        flat::Variable {
            name: name.clone(),
            variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );
    let mut when_clause = rumoca_ir_flat::WhenClause::new(
        rumoca_core::Expression::Literal {
            value: Literal::Boolean(true),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
    );
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::assign(
        name.clone(),
        rumoca_core::Expression::Literal {
            value: Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "test",
    ));
    flat.when_clauses.push(when_clause);

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
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

#[test]
fn test_should_skip_binding_for_explicit_var_keeps_record_prefix_unknown_binding() {
    let name = VarName::new("core.V_m.re");
    let var = flat::Variable {
        name: name.clone(),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("core.port_p.V_m")),
            rhs: Box::new(make_var_ref("core.port_n.V_m")),
            span: rumoca_core::Span::DUMMY,
        }),
        ..Default::default()
    };

    let unknowns: HashSet<VarName> = [
        VarName::new("core.V_m.re"),
        VarName::new("core.port_p.V_m.re"),
        VarName::new("core.port_p.V_m.im"),
        VarName::new("core.port_n.V_m.re"),
        VarName::new("core.port_n.V_m.im"),
    ]
    .into_iter()
    .collect();
    let unknown_prefix_children = build_unknown_prefix_children(&unknowns);

    assert!(
        !should_skip_binding_for_explicit_var(&name, &var, &unknowns, &unknown_prefix_children),
        "record-prefix bindings that reference other unknown fields must be kept"
    );
}

#[test]
fn test_should_keep_connected_input_binding_for_connected_input_with_binding() {
    let name = VarName::new("u");
    let var = flat::Variable {
        name: name.clone(),
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Literal {
            value: Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }),
        ..Default::default()
    };
    let mut connected_input_only = HashSet::default();
    connected_input_only.insert(name.clone());

    assert!(should_keep_connected_input_binding(
        &VariableKind::Input,
        &name,
        &var,
        &connected_input_only
    ));
}

#[test]
fn test_should_keep_connected_input_binding_rejects_missing_binding() {
    let name = VarName::new("u");
    let var = flat::Variable {
        name: name.clone(),
        causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
        is_primitive: true,
        binding: None,
        ..Default::default()
    };
    let mut connected_input_only = HashSet::default();
    connected_input_only.insert(name.clone());

    assert!(!should_keep_connected_input_binding(
        &VariableKind::Input,
        &name,
        &var,
        &connected_input_only
    ));
}

#[test]
fn test_should_keep_connected_input_binding_rejects_non_input_kind() {
    let name = VarName::new("x");
    let var = flat::Variable {
        name: name.clone(),
        causality: rumoca_core::Causality::Output(rumoca_core::Token::default()),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Literal {
            value: Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }),
        ..Default::default()
    };
    let mut connected_input_only = HashSet::default();
    connected_input_only.insert(name.clone());

    assert!(!should_keep_connected_input_binding(
        &VariableKind::Algebraic,
        &name,
        &var,
        &connected_input_only
    ));
}

#[test]
fn test_should_skip_binding_for_explicit_var_skips_constant_binding() {
    let name = VarName::new("x");
    let var = flat::Variable {
        name: name.clone(),
        is_primitive: true,
        binding: Some(rumoca_core::Expression::Literal {
            value: Literal::Integer(0),
            span: rumoca_core::Span::DUMMY,
        }),
        ..Default::default()
    };
    let unknowns: HashSet<VarName> = [name.clone()].into_iter().collect();
    let unknown_prefix_children = build_unknown_prefix_children(&unknowns);

    assert!(
        should_skip_binding_for_explicit_var(&name, &var, &unknowns, &unknown_prefix_children),
        "constant bindings with no other unknown refs should be skipped when explicit equations exist"
    );
}

#[test]
fn test_collect_vars_with_unknown_rhs_resolves_collapsed_array_member_refs() {
    let mut flat = Model::new();
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(make_var_ref("ht.Ts")),
            rhs: Box::new(make_var_ref("ht.heatPorts.T")),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "ht".to_string(),
        },
        scalar_count: 1,
    });

    let unknowns: HashSet<VarName> = [
        VarName::new("ht.Ts"),
        VarName::new("ht.heatPorts[1].T"),
        VarName::new("ht.heatPorts[1].Q_flow"),
    ]
    .into_iter()
    .collect();

    let defined = collect_vars_with_unknown_rhs(&flat, &unknowns);
    assert!(
        defined.contains(&VarName::new("ht.Ts")),
        "collapsed array-member RHS refs must mark LHS as unknown-related"
    );
}

#[test]
fn test_empty_model() {
    let flat = Model::new();
    let dae = to_dae(&flat).unwrap();
    assert!(crate::balance::is_balanced(&dae));
    assert_eq!(crate::balance::balance(&dae), 0);
}

#[test]
fn test_internal_input_with_der_becomes_state() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("medium.p"),
        flat::Variable {
            name: VarName::new("medium.p"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("medium.p")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae(&flat).expect("internal input der-equation should compile");
    assert!(
        dae.variables
            .states
            .contains_key(&rumoca_core::VarName::new("medium.p")),
        "internal input with der() must become a state unknown"
    );
    assert!(
        !dae.variables
            .inputs
            .contains_key(&rumoca_core::VarName::new("medium.p")),
        "internal input with der() must not remain an external input"
    );
    assert_eq!(crate::balance::balance(&dae), 0);
}

#[test]
fn test_top_level_input_with_der_remains_input() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("u"),
        flat::Variable {
            name: VarName::new("u"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("u")],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: Literal::Real(0.0),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("top-level input der-equation should compile");
    assert!(
        dae.variables
            .inputs
            .contains_key(&rumoca_core::VarName::new("u")),
        "external top-level input with der() must remain an input"
    );
    assert!(
        !dae.variables
            .states
            .contains_key(&rumoca_core::VarName::new("u")),
        "external top-level input with der() must not become a state"
    );
}

#[test]
fn test_component_ref_to_var_name() {
    let comp = make_comp_ref("myVar");
    let name = comp.to_var_name();
    assert_eq!(name.as_str(), "myVar");
}

#[test]
fn test_component_ref_to_var_name_qualified() {
    let comp = rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![
            rumoca_core::ComponentRefPart {
                ident: "comp".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![],
            },
            rumoca_core::ComponentRefPart {
                ident: "var".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![],
            },
        ],
        def_id: None,
    };
    let name = comp.to_var_name();
    assert_eq!(name.as_str(), "comp.var");
}

#[test]
fn test_collect_when_statement_targets_simple() {
    // Test: when statements should collect their targets
    let stmts = vec![make_when_stmt(&["x", "y"])];
    let mut targets = HashSet::default();
    collect_when_statement_targets(&stmts, &mut targets);

    assert_eq!(targets.len(), 2);
    assert!(targets.contains(&VarName::new("x")));
    assert!(targets.contains(&VarName::new("y")));
}

#[test]
fn test_collect_when_statement_targets_nested_in_if() {
    // Test: when statements inside if should be found
    let when_stmt = make_when_stmt(&["discrete_var"]);
    let if_stmt = rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: rumoca_core::Expression::Empty {
                span: rumoca_core::Span::DUMMY,
            },
            stmts: vec![when_stmt],
        }],
        else_block: None,

        span: rumoca_core::Span::DUMMY,
    };

    let mut targets = HashSet::default();
    collect_when_statement_targets(&[if_stmt], &mut targets);

    assert_eq!(targets.len(), 1);
    assert!(targets.contains(&VarName::new("discrete_var")));
}

#[test]
fn test_collect_when_statement_targets_ignores_non_when() {
    // Test: regular assignments outside when should not be collected
    let stmts = vec![make_assignment("continuous_var")];
    let mut targets = HashSet::default();
    collect_when_statement_targets(&stmts, &mut targets);

    assert!(targets.is_empty());
}

#[test]
fn test_is_input_input_connection_true() {
    // Test: connection between two inputs should return true

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("a"),
        Variable::new(rumoca_core::VarName::new("a")),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("b"),
        Variable::new(rumoca_core::VarName::new("b")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("a").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("b").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "a".to_string(),
            rhs: "b".to_string(),
        },
        scalar_count: 1,
    };

    assert!(is_input_input_connection(&eq, &dae));
}

#[test]
fn test_is_input_input_connection_false_one_algebraic() {
    // Test: connection between input and algebraic should return false

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("a"),
        Variable::new(rumoca_core::VarName::new("a")),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("b"),
        Variable::new(rumoca_core::VarName::new("b")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("a").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("b").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "a".to_string(),
            rhs: "b".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_input_connection(&eq, &dae));
}

#[test]
fn test_is_input_input_connection_false_not_connection() {
    // Test: non-connection equations should return false

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("a"),
        Variable::new(rumoca_core::VarName::new("a")),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("b"),
        Variable::new(rumoca_core::VarName::new("b")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("a").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("b").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_input_connection(&eq, &dae));
}

#[test]
fn test_is_input_default_equation_true_for_parameter_rhs() {
    let mut flat = Model::new();
    flat.top_level_input_components.insert("x_in".to_string());

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("x_in"),
        Variable::new(rumoca_core::VarName::new("x_in")),
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("x_param"),
        Variable::new(rumoca_core::VarName::new("x_param")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_in").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_param").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_is_input_default_equation_false_for_unknown_rhs() {
    let mut flat = Model::new();
    flat.top_level_input_components.insert("x_in".to_string());

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("x_in"),
        Variable::new(rumoca_core::VarName::new("x_in")),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("x_unknown"),
        Variable::new(rumoca_core::VarName::new("x_unknown")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_in").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_unknown").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_is_input_default_equation_false_for_rhs_input_alias() {
    let mut flat = Model::new();
    flat.top_level_input_components.insert("x_in".to_string());
    flat.top_level_input_components.insert("y_in".to_string());

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("x_in"),
        Variable::new(rumoca_core::VarName::new("x_in")),
    );
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("y_in"),
        Variable::new(rumoca_core::VarName::new("y_in")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("x_in").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("y_in").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "model".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_is_input_default_equation_false_for_internal_input_default() {
    let flat = Model::new();

    let mut dae = Dae::new();
    dae.variables.inputs.insert(
        rumoca_core::VarName::new("transition1.condition"),
        Variable::new(rumoca_core::VarName::new("transition1.condition")),
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("alwaysTrue"),
        Variable::new(rumoca_core::VarName::new("alwaysTrue")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("transition1.condition").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: VarName::new("alwaysTrue").into(),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "transition1".to_string(),
        },
        scalar_count: 1,
    };

    assert!(!is_input_default_equation(&eq, &flat, &dae));
}

#[test]
fn test_connected_input_binding_kept_for_input_only_connection_alias() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("inner.p"),
        flat::Variable {
            name: VarName::new("inner.p"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            binding: Some(rumoca_core::Expression::Literal {
                value: Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("inner.q"),
        flat::Variable {
            name: VarName::new("inner.q"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );

    add_connection_equation(&mut flat, "inner.q", "inner.p");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("to_dae should succeed for connected internal input alias");

    assert!(
        dae.variables
            .algebraics
            .contains_key(&rumoca_core::VarName::new("inner.p"))
            && dae
                .variables
                .algebraics
                .contains_key(&rumoca_core::VarName::new("inner.q")),
        "connected internal inputs should be promoted to algebraics"
    );
    assert!(
        dae.continuous
            .equations
            .iter()
            .any(|eq| eq.origin.contains("binding equation for inner.p")),
        "binding equation for connected input should be kept for input-only alias set"
    );
    assert_eq!(
        crate::balance::balance(&dae),
        0,
        "input-only connection aliases with a binding must stay balanced"
    );
}

#[test]
fn test_connected_input_alias_with_multilayer_subscripts_promotes_internal_inputs() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("bus.signal"),
        flat::Variable {
            name: VarName::new("bus.signal"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("bus.target"),
        flat::Variable {
            name: VarName::new("bus.target"),
            causality: rumoca_core::Causality::Input(rumoca_core::Token::default()),
            variability: rumoca_core::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );

    add_connection_equation(&mut flat, "bus[1].signal[2]", "bus[1].target[3]");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
            preserve_overridable_param_starts: false,
        },
    )
    .expect("to_dae should succeed for multi-layer indexed input aliases");

    for name in ["bus.signal", "bus.target"] {
        let n = rumoca_core::VarName::new(name);
        assert!(
            dae.variables.algebraics.contains_key(&n),
            "internal input {name} should be promoted through multi-layer subscript fallback"
        );
        assert!(
            !dae.variables.inputs.contains_key(&n),
            "internal input {name} should not remain classified as input after promotion"
        );
    }
}

mod input_promotion_extra;
mod input_promotion_indexed;
mod tests_embedded_range;
mod tests_equation_classification;
mod tests_flow_sum;
mod tests_function_param_calls;
mod tests_regressions;
mod tests_scalar_shape;
