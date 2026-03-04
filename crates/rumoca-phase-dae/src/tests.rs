use super::*;
use rumoca_ir_ast as ast;

/// Helper to create a flat::ComponentReference from a simple name.
fn make_comp_ref(name: &str) -> flat::ComponentReference {
    flat::ComponentReference {
        local: false,
        parts: vec![flat::ComponentRefPart {
            ident: name.to_string(),
            subs: vec![],
        }],
        def_id: None,
    }
}
/// Helper to create an assignment statement.
fn make_assignment(name: &str) -> rumoca_ir_flat::Statement {
    rumoca_ir_flat::Statement::Assignment {
        comp: make_comp_ref(name),
        value: flat::Expression::Empty,
    }
}
/// Helper to create a when statement with assignments.
fn make_when_stmt(names: &[&str]) -> rumoca_ir_flat::Statement {
    let stmts: Vec<_> = names.iter().map(|n| make_assignment(n)).collect();
    rumoca_ir_flat::Statement::When(vec![flat::StatementBlock {
        cond: flat::Expression::Empty,
        stmts,
    }])
}
fn make_var_ref(name: &str) -> flat::Expression {
    flat::Expression::VarRef {
        name: VarName::new(name),
        subscripts: vec![],
    }
}
fn add_connection_equation(flat: &mut Model, lhs: &str, rhs: &str) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: rumoca_ir_ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref(lhs)),
            rhs: Box::new(make_var_ref(rhs)),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: lhs.to_string(),
            rhs: rhs.to_string(),
        },
        scalar_count: 1,
    });
}

fn add_component_equation(flat: &mut Model, lhs: &str, rhs: flat::Expression) {
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: rumoca_ir_ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref(lhs)),
            rhs: Box::new(rhs),
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
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref(state_name)],
            }),
            rhs: Box::new(flat::Expression::FunctionCall {
                name: VarName::new(call_name),
                args: vec![make_var_ref(state_name)],
                is_constructor: false,
            }),
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
            variability: rumoca_ir_ast::Variability::Parameter(ast::Token::default()),
            binding: Some(flat::Expression::Literal(rumoca_ir_flat::Literal::Real(
                0.577350269,
            ))),
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.add_variable(
        VarName::new("idealTransformer.idealTransformer[1].n"),
        flat::Variable {
            name: VarName::new("idealTransformer.idealTransformer[1].n"),
            variability: rumoca_ir_ast::Variability::Parameter(ast::Token::default()),
            binding: Some(make_var_ref("idealTransformer.nT")),
            is_primitive: true,
            ..Default::default()
        },
    );

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for parameter-only model");

    let nested = dae
        .parameters
        .get(&VarName::new("idealTransformer.idealTransformer[1].n"))
        .expect("missing nested transformer parameter");
    let start = nested
        .start
        .as_ref()
        .expect("nested transformer parameter should keep rewritten start expression");

    match start {
        flat::Expression::VarRef { name, subscripts } => {
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
fn test_todae_rejects_unresolved_function_calls() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(flat::Expression::FunctionCall {
                name: VarName::new("missingFn"),
                args: vec![make_var_ref("x")],
                is_constructor: false,
            }),
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
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(make_var_ref("missingRef")),
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
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(make_var_ref("HeatingDiode1.k")),
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

    let stub = rumoca_ir_flat::Function::new("f", Span::DUMMY);
    flat.add_function(stub);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(flat::Expression::FunctionCall {
                name: VarName::new("f"),
                args: vec![make_var_ref("x")],
                is_constructor: false,
            }),
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
        },
    )
    .expect_err("empty non-external function should fail in ToDae validation");

    assert!(
        matches!(err, ToDaeError::FunctionWithoutBody { ref name, .. } if name == "f"),
        "expected function-without-body diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_preserves_function_algorithm_bodies_for_codegen_readability() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut fn_def = rumoca_ir_flat::Function::new("f", Span::DUMMY);
    fn_def.add_input(flat::FunctionParam::new("u", "Real"));
    fn_def.add_output(flat::FunctionParam::new("y", "Real"));
    fn_def.body.push(flat::Statement::Assignment {
        comp: make_comp_ref("y"),
        value: flat::Expression::Binary {
            op: ast::OpBinary::Add(ast::Token::default()),
            lhs: Box::new(make_var_ref("u")),
            rhs: Box::new(flat::Expression::Literal(rumoca_ir_flat::Literal::Real(
                1.0,
            ))),
        },
    });
    flat.add_function(fn_def);

    add_scalar_ode_with_rhs_call(&mut flat, "x", "f");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should keep reachable function bodies");

    let lowered_fn = dae
        .functions
        .get(&VarName::new("f"))
        .expect("function f should be preserved in DAE");
    assert_eq!(
        lowered_fn.body.len(),
        1,
        "function algorithm body must be preserved for downstream codegen readability"
    );
}

#[test]
fn test_todae_lowers_supported_model_algorithms_to_equations() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::Assignment {
            comp: make_comp_ref("y"),
            value: make_var_ref("x"),
        }],
        Span::DUMMY,
        "model algorithm".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should lower simple model algorithms");

    assert!(
        dae.f_x
            .iter()
            .any(|eq| eq.origin.contains("algorithm assignment")),
        "lowered model algorithm assignment should appear in f_x"
    );
}

#[test]
fn test_todae_lowers_model_algorithm_for_loop_with_static_range() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::For {
            indices: vec![flat::ForIndex {
                ident: "i".to_string(),
                range: flat::Expression::Range {
                    start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                    step: None,
                    end: Box::new(flat::Expression::Literal(flat::Literal::Integer(3))),
                },
            }],
            equations: vec![flat::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("i"),
            }],
        }],
        Span::DUMMY,
        "model for".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("for-loop algorithms with static ranges should lower to equations");

    assert!(
        dae.f_x
            .iter()
            .any(|eq| eq.origin.contains("algorithm for-assignment")),
        "expected lowered for-loop assignment equations in f_x"
    );
}

#[test]
fn test_todae_rejects_model_algorithm_for_loop_with_non_constant_range() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "n");
    add_primitive_real(&mut flat, "y");

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::For {
            indices: vec![flat::ForIndex {
                ident: "i".to_string(),
                range: flat::Expression::Range {
                    start: Box::new(flat::Expression::Literal(flat::Literal::Integer(1))),
                    step: None,
                    end: Box::new(make_var_ref("n")),
                },
            }],
            equations: vec![flat::Statement::Assignment {
                comp: make_comp_ref("y"),
                value: make_var_ref("i"),
            }],
        }],
        Span::DUMMY,
        "model for dynamic range".to_string(),
    ));

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect_err("for-loop algorithms with non-constant ranges must fail");

    assert!(
        matches!(
            err,
            ToDaeError::UnsupportedAlgorithm {
                ref section,
                ref origin,
                ..
            } if section == "model"
                && (origin.contains("statement=ForRangeEndNotConstant")
                    || origin.contains("statement=ForRangeNotConstant"))
        ),
        "expected ED013 unsupported diagnostic for non-constant for range, got {err:?}"
    );
}

#[test]
fn test_todae_lowers_multi_output_algorithm_function_call_to_output_projections() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "u");
    add_primitive_real(&mut flat, "y1");
    add_primitive_real(&mut flat, "y2");

    let mut f = rumoca_ir_flat::Function::new("Pkg.multi", Span::DUMMY);
    f.add_input(flat::FunctionParam::new("u", "Real"));
    f.add_output(flat::FunctionParam::new("y1", "Real"));
    f.add_output(flat::FunctionParam::new("y2", "Real"));
    f.body.push(flat::Statement::Assignment {
        comp: make_comp_ref("y1"),
        value: make_var_ref("u"),
    });
    f.body.push(flat::Statement::Assignment {
        comp: make_comp_ref("y2"),
        value: flat::Expression::Literal(flat::Literal::Real(0.0)),
    });
    flat.add_function(f);

    flat.algorithms.push(flat::Algorithm::new(
        vec![flat::Statement::FunctionCall {
            comp: make_comp_ref("Pkg.multi"),
            args: vec![make_var_ref("u")],
            outputs: vec![make_var_ref("y1"), make_var_ref("y2")],
        }],
        Span::DUMMY,
        "model multi-output call".to_string(),
    ));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("multi-output model algorithm calls should lower to projection equations");

    let mut saw_y1 = false;
    let mut saw_y2 = false;
    for eq in &dae.f_x {
        let flat::Expression::Binary { rhs, .. } = &eq.rhs else {
            continue;
        };
        let flat::Expression::FunctionCall { name, .. } = rhs.as_ref() else {
            continue;
        };
        if name.as_str() == "Pkg.multi.y1" {
            saw_y1 = true;
        } else if name.as_str() == "Pkg.multi.y2" {
            saw_y2 = true;
        }
    }
    assert!(saw_y1, "missing lowered projection call for first output");
    assert!(saw_y2, "missing lowered projection call for second output");
}

#[test]
fn test_todae_ignores_unreachable_function_without_body() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let stub = rumoca_ir_flat::Function::new("f_unused", Span::DUMMY);
    flat.add_function(stub);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(make_var_ref("x")),
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
        },
    )
    .expect("unreachable empty function bodies should not fail ToDae validation");
}

#[test]
fn test_todae_rejects_member_style_function_call_without_resolved_name() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut fn_def = rumoca_ir_flat::Function::new(
        "Modelica.Mechanics.MultiBody.World.gravityAcceleration",
        Span::DUMMY,
    );
    fn_def.body.push(rumoca_ir_flat::Statement::Return);
    flat.add_function(fn_def);

    add_scalar_ode_with_rhs_call(&mut flat, "x", "world.gravityAcceleration");

    let err = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
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
        flat::Variable {
            name: VarName::new("state"),
            is_primitive: false,
            binding: Some(flat::Expression::FunctionCall {
                name: VarName::new("Common.BaseProps_Tpoly"),
                args: vec![],
                is_constructor: true,
            }),
            ..Default::default()
        },
    );
    flat.variable_type_names
        .insert(VarName::new("state"), "Common.BaseProps_Tpoly".to_string());

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(flat::Expression::Literal(Literal::Real(0.0))),
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
        },
    )
    .expect("record constructor calls should be accepted for known type names");
}

#[test]
fn test_todae_rejects_constructor_field_projection_without_signature() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(flat::Expression::FieldAccess {
                base: Box::new(flat::Expression::FunctionCall {
                    name: VarName::new("My.Record.C"),
                    args: vec![flat::Expression::Literal(Literal::Real(1.0))],
                    is_constructor: true,
                }),
                field: "badField".to_string(),
            }),
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
        },
    )
    .expect_err("missing constructor signature should fail in ToDae validation");

    assert!(
        matches!(
            err,
            ToDaeError::ConstructorFieldProjectionUnresolved { ref projection, .. }
                if projection.starts_with("My.Record.C.badField")
        ),
        "expected constructor projection diagnostic, got {err:?}"
    );
}

#[test]
fn test_todae_accepts_constructor_field_projection_with_signature() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    let mut constructor = Function::new("My.Record.C", Span::DUMMY);
    constructor.add_input(flat::FunctionParam::new("noiseMin", "Real"));
    flat.add_function(constructor);

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(flat::Expression::FieldAccess {
                base: Box::new(flat::Expression::FunctionCall {
                    name: VarName::new("My.Record.C"),
                    args: vec![flat::Expression::Literal(Literal::Real(1.0))],
                    is_constructor: true,
                }),
                field: "noiseMin".to_string(),
            }),
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
        },
    )
    .expect("constructor projection should pass when signature includes projected field");
}

#[test]
fn test_todae_allows_complex_constructor_re_im_projection_without_signature() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(flat::Expression::FieldAccess {
                base: Box::new(flat::Expression::FunctionCall {
                    name: VarName::new("Complex"),
                    args: vec![flat::Expression::Literal(Literal::Real(1.0))],
                    is_constructor: true,
                }),
                field: "re".to_string(),
            }),
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
        },
    )
    .expect("Complex constructor .re/.im projections are accepted without constructor signatures");
}

#[test]
fn test_todae_rejects_parameter_constructor_projection_in_final_dae_validation() {
    let mut flat = Model::new();
    add_primitive_real(&mut flat, "x");
    flat.add_variable(
        VarName::new("p"),
        flat::Variable {
            name: VarName::new("p"),
            variability: ast::Variability::Parameter(ast::Token::default()),
            is_primitive: true,
            binding: Some(flat::Expression::FieldAccess {
                base: Box::new(flat::Expression::FunctionCall {
                    name: VarName::new("My.Param.C"),
                    args: vec![flat::Expression::Literal(Literal::Real(2.0))],
                    is_constructor: true,
                }),
                field: "gain".to_string(),
            }),
            ..Default::default()
        },
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("x")],
            }),
            rhs: Box::new(flat::Expression::Literal(Literal::Real(0.0))),
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
        },
    )
    .expect_err("parameter constructor projection should fail in final DAE validation");

    assert!(
        matches!(
            err,
            ToDaeError::ConstructorFieldProjectionUnresolved { ref projection, .. }
                if projection == "My.Param.C.gain"
        ),
        "expected constructor projection diagnostic, got {err:?}"
    );
}

#[test]
fn test_insert_discrete_var_routes_discrete_type_to_discrete_valued() {
    let mut dae = Dae::new();
    let name = VarName::new("flag");
    let dae_var = Variable::new(name.clone());
    let flat_var = flat::Variable {
        name: name.clone(),
        is_discrete_type: true,
        is_primitive: true,
        ..Default::default()
    };

    insert_discrete_var(&mut dae, &name, dae_var, &flat_var);

    assert!(dae.discrete_valued.contains_key(&name));
    assert!(!dae.discrete_reals.contains_key(&name));
}

#[test]
fn test_insert_discrete_var_routes_real_discrete_to_discrete_reals() {
    let mut dae = Dae::new();
    let name = VarName::new("x");
    let dae_var = Variable::new(name.clone());
    let flat_var = flat::Variable {
        name: name.clone(),
        variability: rumoca_ir_ast::Variability::Discrete(ast::Token::default()),
        is_primitive: true,
        ..Default::default()
    };

    insert_discrete_var(&mut dae, &name, dae_var, &flat_var);

    assert!(dae.discrete_reals.contains_key(&name));
    assert!(!dae.discrete_valued.contains_key(&name));
}

#[test]
fn test_todae_routes_explicit_discrete_integer_when_assignment_to_f_m() {
    let mut flat = Model::new();
    let name = VarName::new("mode");
    flat.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            variability: rumoca_ir_ast::Variability::Discrete(ast::Token::default()),
            is_discrete_type: true,
            is_primitive: true,
            ..Default::default()
        },
    );
    let mut when_clause = rumoca_ir_flat::WhenClause::new(
        flat::Expression::Literal(Literal::Boolean(true)),
        Span::DUMMY,
    );
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::assign(
        name.clone(),
        flat::Expression::Literal(Literal::Integer(1)),
        Span::DUMMY,
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

    assert!(dae.discrete_valued.contains_key(&name));
    assert!(!dae.algebraics.contains_key(&name));
    assert_eq!(
        dae.f_m.len(),
        1,
        "expected one discrete-valued event equation"
    );
}

#[test]
fn test_todae_routes_explicit_discrete_real_when_assignment_to_f_z() {
    let mut flat = Model::new();
    let name = VarName::new("d");
    flat.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            variability: rumoca_ir_ast::Variability::Discrete(ast::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );
    let mut when_clause = rumoca_ir_flat::WhenClause::new(
        flat::Expression::Literal(Literal::Boolean(true)),
        Span::DUMMY,
    );
    when_clause.add_equation(rumoca_ir_flat::WhenEquation::assign(
        name.clone(),
        flat::Expression::Literal(Literal::Real(0.0)),
        Span::DUMMY,
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

    assert!(dae.discrete_reals.contains_key(&name));
    assert!(!dae.algebraics.contains_key(&name));
    assert_eq!(
        dae.f_z.len(),
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
        binding: Some(flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref("core.port_p.V_m")),
            rhs: Box::new(make_var_ref("core.port_n.V_m")),
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
        causality: ast::Causality::Input(ast::Token::default()),
        is_primitive: true,
        binding: Some(flat::Expression::Literal(Literal::Real(1.0))),
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
        causality: ast::Causality::Input(ast::Token::default()),
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
        causality: ast::Causality::Output(ast::Token::default()),
        is_primitive: true,
        binding: Some(flat::Expression::Literal(Literal::Real(1.0))),
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
        binding: Some(flat::Expression::Literal(Literal::Integer(0))),
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
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref("ht.Ts")),
            rhs: Box::new(make_var_ref("ht.heatPorts.T")),
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
    assert!(dae.is_balanced());
    assert_eq!(dae.balance(), 0);
}

#[test]
fn test_internal_input_with_der_becomes_state() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("medium.p"),
        flat::Variable {
            name: VarName::new("medium.p"),
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("medium.p")],
            }),
            rhs: Box::new(flat::Expression::Literal(Literal::Real(0.0))),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "medium".to_string(),
        },
        scalar_count: 1,
    });

    let dae = to_dae(&flat).expect("internal input der-equation should compile");
    assert!(
        dae.states.contains_key(&VarName::new("medium.p")),
        "internal input with der() must become a state unknown"
    );
    assert!(
        !dae.inputs.contains_key(&VarName::new("medium.p")),
        "internal input with der() must not remain an external input"
    );
    assert_eq!(dae.balance(), 0);
}

#[test]
fn test_top_level_input_with_der_remains_input() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("u"),
        flat::Variable {
            name: VarName::new("u"),
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![make_var_ref("u")],
            }),
            rhs: Box::new(flat::Expression::Literal(Literal::Real(0.0))),
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
        },
    )
    .expect("top-level input der-equation should compile");
    assert!(
        dae.inputs.contains_key(&VarName::new("u")),
        "external top-level input with der() must remain an input"
    );
    assert!(
        !dae.states.contains_key(&VarName::new("u")),
        "external top-level input with der() must not become a state"
    );
}

#[test]
fn test_component_ref_to_varname() {
    let comp = make_comp_ref("myVar");
    let name = component_reference_to_var_name(&comp);
    assert_eq!(name.as_str(), "myVar");
}

#[test]
fn test_component_ref_to_varname_qualified() {
    let comp = flat::ComponentReference {
        local: false,
        parts: vec![
            flat::ComponentRefPart {
                ident: "comp".to_string(),
                subs: vec![],
            },
            flat::ComponentRefPart {
                ident: "var".to_string(),
                subs: vec![],
            },
        ],
        def_id: None,
    };
    let name = component_reference_to_var_name(&comp);
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
    let if_stmt = rumoca_ir_flat::Statement::If {
        cond_blocks: vec![flat::StatementBlock {
            cond: flat::Expression::Empty,
            stmts: vec![when_stmt],
        }],
        else_block: None,
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
    dae.inputs
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.inputs
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("a"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("b"),
                subscripts: vec![],
            }),
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
    dae.inputs
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("a"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("b"),
                subscripts: vec![],
            }),
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
    dae.inputs
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.inputs
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("a"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("b"),
                subscripts: vec![],
            }),
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
    dae.inputs
        .insert(VarName::new("x_in"), Variable::new(VarName::new("x_in")));
    dae.parameters.insert(
        VarName::new("x_param"),
        Variable::new(VarName::new("x_param")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("x_in"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("x_param"),
                subscripts: vec![],
            }),
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
    dae.inputs
        .insert(VarName::new("x_in"), Variable::new(VarName::new("x_in")));
    dae.algebraics.insert(
        VarName::new("x_unknown"),
        Variable::new(VarName::new("x_unknown")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("x_in"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("x_unknown"),
                subscripts: vec![],
            }),
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
    dae.inputs
        .insert(VarName::new("x_in"), Variable::new(VarName::new("x_in")));
    dae.inputs
        .insert(VarName::new("y_in"), Variable::new(VarName::new("y_in")));

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("x_in"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("y_in"),
                subscripts: vec![],
            }),
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
    dae.inputs.insert(
        VarName::new("transition1.condition"),
        Variable::new(VarName::new("transition1.condition")),
    );
    dae.parameters.insert(
        VarName::new("alwaysTrue"),
        Variable::new(VarName::new("alwaysTrue")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("transition1.condition"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("alwaysTrue"),
                subscripts: vec![],
            }),
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
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            binding: Some(flat::Expression::Literal(Literal::Real(1.0))),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("inner.q"),
        flat::Variable {
            name: VarName::new("inner.q"),
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );

    add_connection_equation(&mut flat, "inner.q", "inner.p");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for connected internal input alias");

    assert!(
        dae.algebraics.contains_key(&VarName::new("inner.p"))
            && dae.algebraics.contains_key(&VarName::new("inner.q")),
        "connected internal inputs should be promoted to algebraics"
    );
    assert!(
        dae.f_x
            .iter()
            .any(|eq| eq.origin.contains("binding equation for inner.p")),
        "binding equation for connected input should be kept for input-only alias set"
    );
    assert_eq!(
        dae.balance(),
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
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("bus.target"),
        flat::Variable {
            name: VarName::new("bus.target"),
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );

    add_connection_equation(&mut flat, "bus[1].signal[2]", "bus[1].target[3]");

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for multi-layer indexed input aliases");

    for name in ["bus.signal", "bus.target"] {
        let n = VarName::new(name);
        assert!(
            dae.algebraics.contains_key(&n),
            "internal input {name} should be promoted through multi-layer subscript fallback"
        );
        assert!(
            !dae.inputs.contains_key(&n),
            "internal input {name} should not remain classified as input after promotion"
        );
    }
}

#[test]
fn test_rhs_intra_component_alias_with_multilayer_connected_lhs_does_not_promote_input() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("test.p"),
        flat::Variable {
            name: VarName::new("test.p"),
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("test.conn.field"),
        flat::Variable {
            name: VarName::new("test.conn.field"),
            variability: ast::Variability::Empty,
            is_primitive: true,
            connected: true,
            dims: vec![2, 3],
            ..Default::default()
        },
    );

    add_component_equation(&mut flat, "test.conn[1].field[2]", make_var_ref("test.p"));

    let dae = to_dae_with_options(
        &flat,
        ToDaeOptions {
            error_on_unbalanced: false,
        },
    )
    .expect("to_dae should succeed for multi-layer connected LHS alias");

    let input = VarName::new("test.p");
    assert!(
        dae.inputs.contains_key(&input),
        "RHS input should remain an input when aliased from a connected multi-layer LHS"
    );
    assert!(
        !dae.algebraics.contains_key(&input),
        "RHS input should not be promoted to algebraic when LHS is connected"
    );
}

#[test]
fn test_get_output_in_input_output_connection_subscripted_output() {
    let mut dae = Dae::new();
    dae.inputs.insert(
        VarName::new("gain.u"),
        Variable::new(VarName::new("gain.u")),
    );
    dae.outputs.insert(
        VarName::new("table.y"),
        Variable::new(VarName::new("table.y")),
    );

    let eq = rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("table.y[1]"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("gain.u"),
                subscripts: vec![],
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::Connection {
            lhs: "table.y[1]".to_string(),
            rhs: "gain.u".to_string(),
        },
        scalar_count: 1,
    };

    let out = get_output_in_input_output_connection(&eq, &dae)
        .expect("subscripted output/input connection should resolve output side");
    assert_eq!(out, VarName::new("table.y[1]"));
}

#[test]
fn test_classify_equations_skips_subscripted_output_input_connection_when_output_has_component_equation()
 {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("table.y"),
        flat::Variable {
            name: VarName::new("table.y"),
            causality: ast::Causality::Output(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("gain.u"),
        flat::Variable {
            name: VarName::new("gain.u"),
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );

    // Component equation for one output element.
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("table.y[1]"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::Literal(Literal::Real(1.0))),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "table".to_string(),
        },
        scalar_count: 1,
    });

    // Input-output alias should be skipped because output already has a component equation.
    add_connection_equation(&mut flat, "table.y[1]", "gain.u");

    let mut dae = Dae::new();
    dae.outputs.insert(
        VarName::new("table.y"),
        Variable::new(VarName::new("table.y")),
    );
    dae.inputs.insert(
        VarName::new("gain.u"),
        Variable::new(VarName::new("gain.u")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(
        dae.f_x.len(),
        1,
        "connection equation should be skipped for output element defined by component equation"
    );
    assert!(dae.f_x[0].origin.contains("equation from table"));
}

#[test]
fn test_classify_equations_skips_output_known_connection_when_output_has_component_equation() {
    let mut flat = Model::new();
    flat.add_variable(
        VarName::new("gain.y"),
        flat::Variable {
            name: VarName::new("gain.y"),
            causality: ast::Causality::Output(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("gain.u"),
        flat::Variable {
            name: VarName::new("gain.u"),
            causality: ast::Causality::Input(ast::Token::default()),
            variability: ast::Variability::Empty,
            is_primitive: true,
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("outBus.x"),
        flat::Variable {
            name: VarName::new("outBus.x"),
            variability: ast::Variability::Parameter(ast::Token::default()),
            is_primitive: true,
            ..Default::default()
        },
    );

    // Output has an explicit component equation.
    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("gain.y"),
                subscripts: vec![],
            }),
            rhs: Box::new(flat::Expression::VarRef {
                name: VarName::new("gain.u"),
                subscripts: vec![],
            }),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "gain".to_string(),
        },
        scalar_count: 1,
    });

    // Redundant alias to non-unknown bus member should be skipped.
    add_connection_equation(&mut flat, "gain.y", "outBus.x");

    let mut dae = Dae::new();
    dae.outputs.insert(
        VarName::new("gain.y"),
        Variable::new(VarName::new("gain.y")),
    );
    dae.inputs.insert(
        VarName::new("gain.u"),
        Variable::new(VarName::new("gain.u")),
    );
    dae.parameters.insert(
        VarName::new("outBus.x"),
        Variable::new(VarName::new("outBus.x")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert_eq!(
        dae.f_x.len(),
        1,
        "output->known connection should be skipped when output has defining component equation"
    );
    assert!(dae.f_x[0].origin.contains("equation from gain"));
}

#[test]
fn test_classify_equations_skips_unconnected_flow_for_top_level_overconstrained_connector() {
    let mut flat = Model::new();
    flat.top_level_connectors.insert("port".to_string());

    flat.add_variable(
        VarName::new("port.reference.gamma"),
        flat::Variable {
            name: VarName::new("port.reference.gamma"),
            variability: ast::Variability::Empty,
            is_primitive: true,
            is_overconstrained: true,
            oc_record_path: Some("port.reference".to_string()),
            oc_eq_constraint_size: Some(0),
            ..Default::default()
        },
    );
    flat.add_variable(
        VarName::new("port.Phi.re"),
        flat::Variable {
            name: VarName::new("port.Phi.re"),
            flow: true,
            is_primitive: true,
            ..Default::default()
        },
    );

    flat.add_equation(rumoca_ir_flat::Equation {
        residual: flat::Expression::Binary {
            op: ast::OpBinary::Sub(ast::Token::default()),
            lhs: Box::new(make_var_ref("port.Phi.re")),
            rhs: Box::new(flat::Expression::Literal(Literal::Real(0.0))),
        },
        span: Span::DUMMY,
        origin: rumoca_ir_flat::EquationOrigin::UnconnectedFlow {
            variable: "port.Phi.re".to_string(),
        },
        scalar_count: 1,
    });

    let mut dae = Dae::new();
    dae.algebraics.insert(
        VarName::new("port.Phi.re"),
        Variable::new(VarName::new("port.Phi.re")),
    );

    let prefix_counts = build_prefix_counts(&flat);
    classify_equations(&mut dae, &flat, &prefix_counts);

    assert!(
        dae.f_x.is_empty(),
        "top-level OC connector unconnected flow closure should not be counted in local structural equations"
    );
}

mod tests_embedded_range;
mod tests_flow_sum;
mod tests_regressions;
mod tests_scalar_shape;
