//! SPEC_0021 file-size exception: constant-substitution regressions still cover
//! parameter bindings, scoped aliases, and static initial algorithms together.
//! split plan: split tests by substitution source and static statement family.

use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_flatten_substitute_constant_source_91.mo"),
        10,
        22,
    )
}

fn simple_assignment(value: rumoca_core::Expression) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "y".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![],
            }],
            def_id: None,
        },
        value,
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_ref_with_target_def_id(path: &str, def_id: rumoca_core::DefId) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            path,
            rumoca_core::ComponentReference {
                local: false,
                span: rumoca_core::Span::DUMMY,
                parts: rumoca_core::split_path_with_indices(path)
                    .into_iter()
                    .map(|ident| rumoca_core::ComponentRefPart {
                        ident: ident.to_string(),
                        span: rumoca_core::Span::DUMMY,
                        subs: vec![],
                    })
                    .collect(),
                def_id: Some(def_id),
            },
        ),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn named_arg(name: &str, value: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![value],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    }
}

fn spanned_var_ref(name: &str) -> rumoca_core::Expression {
    let var_name = rumoca_core::VarName::new(name);
    let component_ref = rumoca_core::component_reference_from_flat_name(&var_name, test_span())
        .expect("test reference should parse as a component reference");
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref),
        subscripts: vec![],
        span: test_span(),
    }
}

#[test]
fn substitute_known_constants_prefers_integer_parameter_binding_over_stale_real_start() {
    let mut ctx = Context::new();
    ctx.parameter_values
        .insert("periodicClock.factor".to_string(), 20);
    ctx.real_parameter_values
        .insert("periodicClock.factor".to_string(), 0.0);

    let substituted = substitute_known_constants_expr(
        spanned_var_ref("periodicClock.factor"),
        &ctx,
        &std::collections::HashSet::default(),
        &std::collections::HashSet::default(),
        "",
    )
    .unwrap();

    assert_eq!(
        substituted,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(20),
            span: test_span(),
        }
    );
}

#[test]
fn substitute_known_constants_prefers_scoped_instance_for_def_id_sibling_parameter() {
    let n_nodes_def = rumoca_core::DefId::new(42);
    let mut ctx = Context::new();
    ctx.parameter_values.insert("nNodes".to_string(), 2);
    ctx.parameter_values.insert("pipe.nNodes".to_string(), 20);
    ctx.target_def_names
        .insert(n_nodes_def, "nNodes".to_string());

    let substituted = substitute_known_constants_expr(
        var_ref_with_target_def_id("nNodes", n_nodes_def),
        &ctx,
        &std::collections::HashSet::default(),
        &std::collections::HashSet::default(),
        "pipe",
    )
    .unwrap();

    assert_eq!(
        substituted,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(20),
            span: rumoca_core::Span::DUMMY,
        }
    );
}

#[test]
fn substitute_known_constants_prefers_scoped_instance_for_class_qualified_def_id_parameter() {
    let n_nodes_def = rumoca_core::DefId::new(43);
    let mut ctx = Context::new();
    ctx.parameter_values.insert(
        "Modelica.Fluid.Pipes.BaseClasses.PartialTwoPortFlow.nNodes".to_string(),
        2,
    );
    ctx.parameter_values.insert("pipe.nNodes".to_string(), 20);
    ctx.target_def_names.insert(
        n_nodes_def,
        "Modelica.Fluid.Pipes.BaseClasses.PartialTwoPortFlow.nNodes".to_string(),
    );

    let substituted = substitute_known_constants_expr(
        var_ref_with_target_def_id(
            "Modelica.Fluid.Pipes.BaseClasses.PartialTwoPortFlow.nNodes",
            n_nodes_def,
        ),
        &ctx,
        &std::collections::HashSet::default(),
        &std::collections::HashSet::default(),
        "pipe",
    )
    .unwrap();

    assert_eq!(
        substituted,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(20),
            span: rumoca_core::Span::DUMMY,
        }
    );
}

#[test]
fn substitute_known_constants_preserves_tunable_real_parameter_binding_in_equation() {
    let mut model = flat::Model::new();
    let a_name = rumoca_core::VarName::new("a");
    model.add_variable(
        a_name.clone(),
        flat::Variable {
            name: a_name,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: test_span(),
            }),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_variable(
        rumoca_core::VarName::new("x"),
        flat::Variable {
            name: rumoca_core::VarName::new("x"),
            variability: rumoca_core::Variability::Continuous(rumoca_core::Token::default()),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(var_ref("a")),
            rhs: Box::new(var_ref("x")),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::Binding {
            variable: "x".to_string(),
        },
    ));

    substitute_known_constants_in_flat(&mut model, &Context::new()).unwrap();

    let rumoca_core::Expression::Binary { lhs, .. } = &model.equations[0].residual else {
        panic!("expected product residual");
    };
    assert!(matches!(
        lhs.as_ref(),
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "a" && subscripts.is_empty()
    ));
}

#[test]
fn substitute_known_constants_recovers_clock_factor_binding_from_integer_constructor_bounds() {
    let mut model = flat::Model::new();
    let factor_name = rumoca_core::VarName::new("periodicClock.factor");
    model.add_variable(
        factor_name.clone(),
        flat::Variable {
            name: factor_name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(int_literal(20)),
            binding_from_modification: true,
            start: Some(int_literal(0)),
            min: Some(int_literal(0)),
            is_discrete_type: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model
        .variable_type_names
        .insert(factor_name, "Integer".to_string());
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("periodicClock.c")),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Clock"),
                args: vec![
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Integer"),
                        args: vec![named_arg("min", int_literal(0))],
                        is_constructor: true,
                        span: test_span(),
                    },
                    var_ref("periodicClock.resolutionFactor"),
                ],
                is_constructor: true,
                span: test_span(),
            }),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "periodicClock.c".to_string(),
        },
    ));

    substitute_known_constants_in_flat(&mut model, &Context::new()).unwrap();

    let rumoca_core::Expression::Binary { rhs, .. } = &model.equations[0].residual else {
        panic!("expected residual assignment");
    };
    let rumoca_core::Expression::FunctionCall { args, .. } = rhs.as_ref() else {
        panic!("expected Clock call");
    };
    assert!(matches!(
        args[0],
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(20),
            ..
        }
    ));
}

#[test]
fn substitute_known_constants_recovers_subsample_factor_binding_from_integer_constructor_bounds() {
    let mut model = flat::Model::new();
    let factor_name = rumoca_core::VarName::new("subSample1.factor");
    model.add_variable(
        factor_name.clone(),
        flat::Variable {
            name: factor_name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(int_literal(5)),
            binding_from_modification: true,
            start: Some(int_literal(0)),
            min: Some(int_literal(1)),
            is_discrete_type: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model
        .variable_type_names
        .insert(factor_name, "Integer".to_string());
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("subSample1.y")),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("subSample"),
                args: vec![
                    var_ref("subSample1.u"),
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Integer"),
                        args: vec![named_arg("min", int_literal(1))],
                        is_constructor: true,
                        span: test_span(),
                    },
                ],
                is_constructor: false,
                span: test_span(),
            }),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "subSample1".to_string(),
        },
    ));

    substitute_known_constants_in_flat(&mut model, &Context::new()).unwrap();

    let rumoca_core::Expression::Binary { rhs, .. } = &model.equations[0].residual else {
        panic!("expected residual assignment");
    };
    let rumoca_core::Expression::FunctionCall { args, .. } = rhs.as_ref() else {
        panic!("expected subSample call");
    };
    assert!(matches!(
        args[1],
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(5),
            ..
        }
    ));
}

#[test]
fn substitute_known_constants_reconciles_zero_fill_extent_with_declared_dims() {
    let mut model = flat::Model::new();
    let name = rumoca_core::VarName::new("sum.k");
    let stale_fill = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![int_literal(1), int_literal(0)],
        span: test_span(),
    };
    model.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(stale_fill.clone()),
            start: Some(stale_fill.clone()),
            dims: vec![2],
            is_discrete_type: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("sum.y")),
            rhs: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(var_ref("sum.k")),
                rhs: Box::new(var_ref("sum.u")),
                span: test_span(),
            }),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "sum.y".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert(name.as_str().to_string(), stale_fill.clone());
    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let var = model.variables.get(&name).expect("variable should remain");
    for expr in [var.binding.as_ref(), var.start.as_ref()] {
        let Some(rumoca_core::Expression::BuiltinCall { args, .. }) = expr else {
            panic!("expected fill expression");
        };
        assert_eq!(literal_integer_value(&args[1]), Some(2));
    }
    let rumoca_core::Expression::Binary { rhs, .. } = &model.equations[0].residual else {
        panic!("expected residual assignment");
    };
    let rumoca_core::Expression::Binary { lhs, .. } = rhs.as_ref() else {
        panic!("expected substituted multiplication");
    };
    let rumoca_core::Expression::BuiltinCall { args, .. } = lhs.as_ref() else {
        panic!("expected substituted fill expression");
    };
    assert_eq!(literal_integer_value(&args[1]), Some(2));
}

#[test]
fn substitute_known_constants_reconciles_equation_constructor_extent_with_lhs_dims() {
    let mut model = flat::Model::new();
    let lhs_name = rumoca_core::VarName::new("volume.portsData_diameter");
    model.add_variable(
        lhs_name.clone(),
        flat::Variable {
            name: lhs_name,
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("volume.portsData_diameter")),
            rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Zeros,
                args: vec![int_literal(0)],
                span: test_span(),
            }),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "volume".to_string(),
        },
    ));

    substitute_known_constants_in_flat(&mut model, &Context::new()).unwrap();

    let rumoca_core::Expression::Binary { rhs, .. } = &model.equations[0].residual else {
        panic!("expected residual assignment");
    };
    let rumoca_core::Expression::BuiltinCall { args, .. } = rhs.as_ref() else {
        panic!("expected zeros expression");
    };
    assert_eq!(literal_integer_value(&args[0]), Some(4));
}

#[test]
fn substitute_known_constants_uses_instance_parameter_bindings_in_component_equations() {
    let mut model = flat::Model::new();
    let n_name = rumoca_core::VarName::new("ductOut.flowModel.n");
    model.add_variable(
        n_name.clone(),
        flat::Variable {
            name: n_name,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(int_literal(4)),
            binding_from_modification: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let m_name = rumoca_core::VarName::new("ductOut.flowModel.m");
    model.add_variable(
        m_name.clone(),
        flat::Variable {
            name: m_name,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref("n")),
                rhs: Box::new(int_literal(1)),
                span: test_span(),
            }),
            binding_from_modification: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Zeros,
                args: vec![var_ref("m")],
                span: test_span(),
            }),
            rhs: Box::new(var_ref("ductOut.flowModel.Ib_flows")),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "ductOut.flowModel".to_string(),
        },
    ));

    substitute_known_constants_in_flat(&mut model, &Context::new()).unwrap();

    let rumoca_core::Expression::Binary { lhs, .. } = &model.equations[0].residual else {
        panic!("expected residual subtraction");
    };
    let rumoca_core::Expression::BuiltinCall { args, .. } = lhs.as_ref() else {
        panic!("expected zeros expression");
    };
    assert_eq!(eval_test_integer_expr(&args[0]), Some(3));
}

#[test]
fn substitute_known_constants_keeps_live_boolean_output_definition() {
    let mut model = flat::Model::new();
    let y_name = rumoca_core::VarName::new("source.y");
    model.add_variable(
        y_name.clone(),
        flat::Variable {
            name: y_name,
            is_discrete_type: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let k_name = rumoca_core::VarName::new("source.k");
    model.add_variable(
        k_name.clone(),
        flat::Variable {
            name: k_name,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: test_span(),
            }),
            is_discrete_type: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(spanned_var_ref("source.y")),
            rhs: Box::new(spanned_var_ref("source.k")),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "source".to_string(),
        },
    ));

    // Structural branch evaluation may know the value of a discrete Boolean,
    // but that must not erase the runtime-visible output that defines it.
    let mut ctx = Context::new();
    ctx.boolean_parameter_values
        .insert("source.y".to_string(), true);
    ctx.boolean_parameter_values
        .insert("source.k".to_string(), true);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::Binary { lhs, rhs, .. } = &model.equations[0].residual else {
        panic!("expected residual assignment");
    };
    assert!(matches!(
        lhs.as_ref(),
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "source.y"
    ));
    assert!(matches!(
        rhs.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(true),
            ..
        }
    ));
}

#[test]
fn substitute_known_constants_replaces_live_class_constant_without_flat_binding() {
    let mut model = flat::Model::new();
    let pi_name = rumoca_core::VarName::new("sine.pi");
    model.add_variable(
        pi_name.clone(),
        flat::Variable {
            name: pi_name,
            variability: rumoca_core::Variability::Constant(rumoca_core::Token::default()),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let y_name = rumoca_core::VarName::new("sine.y");
    model.add_variable(
        y_name.clone(),
        flat::Variable {
            name: y_name,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(spanned_var_ref("sine.y")),
            rhs: Box::new(spanned_var_ref("sine.pi")),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "sine".to_string(),
        },
    ));

    // Class constants can be instantiated without retaining their source
    // binding on the Flat declaration. The compiler-owned class-constant
    // identity still makes the value structural and safe to substitute.
    let mut ctx = Context::new();
    ctx.class_constant_keys.insert("sine.pi".to_string());
    ctx.real_parameter_values
        .insert("sine.pi".to_string(), std::f64::consts::PI);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::Binary { lhs, rhs, .. } = &model.equations[0].residual else {
        panic!("expected residual assignment");
    };
    assert!(matches!(
        lhs.as_ref(),
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "sine.y"
    ));
    assert!(matches!(
        rhs.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if (*value - std::f64::consts::PI).abs() < f64::EPSILON
    ));
}

#[test]
fn substitute_known_constants_preserves_modified_parameter_over_stale_context_constant() {
    let mut model = flat::Model::new();
    let source_name = rumoca_core::VarName::new("source.q_end");
    model.add_variable(
        source_name.clone(),
        flat::Variable {
            name: source_name,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.5),
                span: test_span(),
            }),
            binding_from_modification: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let projected_name = rumoca_core::VarName::new("source.p_q_end");
    model.add_variable(
        projected_name.clone(),
        flat::Variable {
            name: projected_name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(spanned_var_ref("source.q_end")),
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    // The context still carries the declaration default, while the Flat
    // variable owns the effective instance modification.
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "source.q_end".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: test_span(),
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let binding = model
        .variables
        .get(&projected_name)
        .and_then(|var| var.binding.as_ref())
        .expect("projected parameter binding should remain");
    assert!(matches!(
        binding,
        rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "source.q_end"
    ));
}

#[test]
fn substitute_algorithms_uses_instance_parameter_bindings_in_rhs_and_range() {
    let mut model = flat::Model::new();
    for (name, binding) in [("table.y0", 3), ("table.n", 2), ("i", 99)] {
        let var_name = rumoca_core::VarName::new(name);
        model.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name,
                variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
                binding: Some(int_literal(binding)),
                binding_from_modification: true,
                is_discrete_type: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    let alias_name = rumoca_core::VarName::new("n");
    model.add_variable(
        alias_name.clone(),
        flat::Variable {
            name: alias_name,
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(var_ref("i")),
            binding_from_modification: true,
            is_discrete_type: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.algorithms.push(flat::Algorithm {
        statements: vec![
            simple_assignment(var_ref("table.y0")),
            rumoca_core::Statement::For {
                indices: vec![rumoca_core::ForIndex {
                    ident: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(int_literal(1)),
                        step: None,
                        end: Box::new(var_ref("table.n")),
                        span: test_span(),
                    },
                }],
                equations: vec![
                    simple_assignment(var_ref("i")),
                    simple_assignment(var_ref("n")),
                ],
                span: test_span(),
            },
            simple_assignment(rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(var_ref("i")),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(int_literal(1)),
                        step: None,
                        end: Box::new(int_literal(2)),
                        span: test_span(),
                    },
                }],
                filter: None,
                span: test_span(),
            }),
        ],
        outputs: vec![],
        span: test_span(),
        origin: "algorithm from table".to_string(),
    });

    let mut ctx = Context::new();
    ctx.parameter_values.insert("table.y0".to_string(), 1);
    ctx.parameter_values.insert("table.n".to_string(), 1);
    ctx.parameter_values.insert("i".to_string(), 99);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Statement::Assignment { value, .. } = &model.algorithms[0].statements[0]
    else {
        panic!("expected initial assignment");
    };
    assert_eq!(literal_integer_value(value), Some(3));

    let rumoca_core::Statement::For {
        indices, equations, ..
    } = &model.algorithms[0].statements[1]
    else {
        panic!("expected table loop");
    };
    let rumoca_core::Expression::Range { end, .. } = &indices[0].range else {
        panic!("expected table loop range");
    };
    assert_eq!(literal_integer_value(end), Some(2));
    let rumoca_core::Statement::Assignment { value, .. } = &equations[0] else {
        panic!("expected loop-body assignment");
    };
    assert!(matches!(
        value,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "i" && subscripts.is_empty()
    ));
    let rumoca_core::Statement::Assignment { value, .. } = &equations[1] else {
        panic!("expected alias assignment");
    };
    assert_eq!(literal_integer_value(value), Some(99));

    let rumoca_core::Statement::Assignment { value, .. } = &model.algorithms[0].statements[2]
    else {
        panic!("expected comprehension assignment");
    };
    let rumoca_core::Expression::ArrayComprehension { expr, .. } = value else {
        panic!("expected array comprehension");
    };
    assert!(matches!(
        expr.as_ref(),
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "i" && subscripts.is_empty()
    ));
}

#[test]
fn substitute_known_constants_uses_declared_dims_for_size_before_stale_start() {
    let mut model = flat::Model::new();
    let columns = rumoca_core::VarName::new("lossTable.columns");
    let stale_columns_start = rumoca_core::Expression::Range {
        start: Box::new(int_literal(2)),
        step: None,
        end: Box::new(int_literal(2)),
        span: test_span(),
    };
    model.add_variable(
        columns.clone(),
        flat::Variable {
            name: columns.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            start: Some(stale_columns_start.clone()),
            dims: vec![2],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Size,
                args: vec![var_ref("lossTable.columns"), int_literal(1)],
                span: test_span(),
            }),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "lossTable".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert(columns.as_str().to_string(), stale_columns_start);
    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::Binary { rhs, .. } = &model.equations[0].residual else {
        panic!("expected residual assignment");
    };
    assert_eq!(literal_integer_value(rhs), Some(2));
}

fn int_literal(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn literal_integer_value(expr: &rumoca_core::Expression) -> Option<i64> {
    let rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        ..
    } = expr
    else {
        return None;
    };
    Some(*value)
}

fn eval_test_integer_expr(expr: &rumoca_core::Expression) -> Option<i64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_test_integer_expr(lhs)?;
            let rhs = eval_test_integer_expr(rhs)?;
            rumoca_core::eval_ast_integer_binary(op, lhs, rhs)
        }
        _ => None,
    }
}

fn assert_ones_extent(expr: &rumoca_core::Expression, expected_extent: i64) {
    let rumoca_core::Expression::BuiltinCall { function, args, .. } = expr else {
        panic!("expected builtin call, got {expr:?}");
    };
    assert_eq!(*function, rumoca_core::BuiltinFunction::Ones);
    assert_eq!(args.len(), 1);
    assert_eq!(eval_test_integer_expr(&args[0]), Some(expected_extent));
}

fn assert_indexed_var_ref(
    expr: &rumoca_core::Expression,
    expected_name: &str,
    expected_index: i64,
) {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        panic!("expected indexed varref `{expected_name}`, got {expr:?}");
    };
    assert_eq!(name.as_str(), expected_name);
    assert!(matches!(
        subscripts.as_slice(),
        [rumoca_core::Subscript::Expr { expr, .. }]
            if literal_integer_value(expr) == Some(expected_index)
    ));
}

#[test]
fn variable_binding_substitution_uses_flat_binding_before_stale_context_value() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "pipe.n");
    add_primitive_variable(&mut model, "pipe.flowModel.n");
    add_primitive_variable(&mut model, "pipe.flowModel.Res_turbulent_internal");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("pipe.n"))
        .expect("variable should exist")
        .binding = Some(int_literal(2));
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("pipe.flowModel.n"))
        .expect("variable should exist")
        .binding = Some(rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(int_literal(3)),
        rhs: Box::new(int_literal(1)),
        span: rumoca_core::Span::DUMMY,
    });
    model
        .variables
        .get_mut(&rumoca_core::VarName::new(
            "pipe.flowModel.Res_turbulent_internal",
        ))
        .expect("variable should exist")
        .binding = Some(rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Ones,
        args: vec![rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("pipe.flowModel.n")),
            rhs: Box::new(int_literal(1)),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    });

    let mut ctx = Context::new();
    ctx.parameter_values
        .insert("pipe.flowModel.n".to_string(), 2);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new(
            "pipe.flowModel.Res_turbulent_internal",
        ))
        .expect("variable should exist")
        .binding
        .as_ref()
        .expect("binding should remain");
    assert_ones_extent(binding, 3);
}

fn assert_symbolically_indexed_var_ref(
    expr: &rumoca_core::Expression,
    expected_name: &str,
    expected_index_name: &str,
) {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        panic!("expected indexed varref `{expected_name}`, got {expr:?}");
    };
    assert_eq!(name.as_str(), expected_name);
    assert!(matches!(
        subscripts.as_slice(),
        [rumoca_core::Subscript::Expr { expr, .. }]
            if matches!(
                expr.as_ref(),
                rumoca_core::Expression::VarRef { name, subscripts, .. }
                    if name.as_str() == expected_index_name && subscripts.is_empty()
            )
    ));
}

fn string_literal(value: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(value.to_string()),
        span: rumoca_core::Span::DUMMY,
    }
}

fn reference_x_fill_expr() -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Div,
                lhs: Box::new(int_literal(1)),
                rhs: Box::new(var_ref("nS")),
                span: rumoca_core::Span::DUMMY,
            },
            var_ref("nS"),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_primitive_variable(model: &mut flat::Model, name: &str) {
    model.add_variable(
        rumoca_core::VarName::new(name),
        flat::Variable {
            name: rumoca_core::VarName::new(name),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
}

#[test]
fn substitutes_qualified_real_class_constant_inside_builtin_array_binding() {
    let mut model = flat::Model::new();
    let name = rumoca_core::VarName::new("pid.D.T");
    model.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            variability: rumoca_core::Variability::Parameter(rumoca_core::Token::default()),
            binding: Some(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Max,
                args: vec![rumoca_core::Expression::Array {
                    elements: vec![
                        var_ref("pid.Nd"),
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Mul,
                            lhs: Box::new(int_literal(100)),
                            rhs: Box::new(spanned_var_ref("Modelica.Constants.eps")),
                            span: test_span(),
                        },
                    ],
                    is_matrix: false,
                    span: test_span(),
                }],
                span: test_span(),
            }),
            binding_from_modification: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut ctx = Context::new();
    ctx.class_constant_keys
        .insert("Modelica.Constants.eps".to_string());
    ctx.real_parameter_values
        .insert("Modelica.Constants.eps".to_string(), f64::EPSILON);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(
        !expr_contains_var_ref(
            model.variables[&name].binding.as_ref().unwrap(),
            "Modelica.Constants.eps"
        ),
        "Modelica.Constants.eps should be folded inside runtime parameter modifier bindings: {:?}",
        model.variables[&name].binding
    );
}

#[test]
fn substitutes_well_known_real_class_constant_in_equations() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "x");
    model.add_equation(flat::Equation::new(
        spanned_var_ref("ModelicaServices.Machine.eps"),
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    let mut ctx = Context::new();
    ctx.class_constant_keys
        .insert("ModelicaServices.Machine.eps".to_string());
    ctx.real_parameter_values
        .insert("ModelicaServices.Machine.eps".to_string(), f64::EPSILON);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(
        matches!(
            model.equations[0].residual,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(_),
                ..
            }
        ),
        "class constant should be substituted in equation residuals: {:?}",
        model.equations[0].residual
    );
}

#[test]
fn substitute_known_constants_preserves_named_arg_marker_for_record_constructor_value() {
    let mut model = flat::Model::new();
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.f"),
            args: vec![
                named_arg("x", var_ref("live_x")),
                named_arg("per", var_ref("pCur1")),
            ],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "pCur1".to_string(),
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Buildings.Fluid.Movers.Data.Generic"),
            args: vec![var_ref("pCur1.V_flow"), var_ref("pCur1.dp")],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::FunctionCall { args, .. } = &model.equations[0].residual else {
        panic!("expected function call");
    };
    assert_eq!(args.len(), 2);
    let rumoca_core::Expression::FunctionCall {
        name,
        args: per_args,
        is_constructor: true,
        ..
    } = &args[1]
    else {
        panic!("expected named per argument marker");
    };
    assert_eq!(name.as_str(), "__rumoca_named_arg__.per");
    assert!(matches!(
        per_args.as_slice(),
        [rumoca_core::Expression::FunctionCall { name, is_constructor: true, .. }]
            if name.as_str() == "Buildings.Fluid.Movers.Data.Generic"
    ));
}

#[test]
fn substitute_known_constants_preserves_path_like_string_literal() {
    let mut ctx = Context::new();
    ctx.string_parameter_values.insert(
        "zone.spawnExe".to_string(),
        "spawn-0.4.3-7048a72798".to_string(),
    );

    let substituted = substitute_known_constants_expr(
        var_ref("zone.spawnExe"),
        &ctx,
        &rustc_hash::FxHashSet::default(),
        &std::collections::HashSet::new(),
        "",
    )
    .unwrap();

    assert_eq!(substituted, string_literal("spawn-0.4.3-7048a72798"));
}

#[test]
fn substitute_known_constants_recovers_path_like_string_variable_start() {
    let mut model = flat::Model::new();
    let name = rumoca_core::VarName::new("zone.spawnExe");
    model.add_variable(
        name.clone(),
        flat::Variable {
            name: name.clone(),
            type_id: rumoca_core::TypeId(3),
            start: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::FieldAccess {
                    base: Box::new(rumoca_core::Expression::FieldAccess {
                        base: Box::new(var_ref("zone")),
                        field: "spawn-0".to_string(),
                        span: test_span(),
                    }),
                    field: "4".to_string(),
                    span: test_span(),
                }),
                field: "3-7048a72798".to_string(),
                span: test_span(),
            }),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model
        .variable_type_names
        .insert(name.clone(), "String".to_string());

    substitute_known_constants_in_flat(&mut model, &Context::new()).unwrap();

    assert_eq!(
        model.variables.get(&name).and_then(|var| var.start.clone()),
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("spawn-0.4.3-7048a72798".to_string()),
            span: test_span(),
        })
    );
}

#[test]
fn collapse_index_refs_preserves_structured_distinct_record_endpoints() {
    let mut model = flat::Model::new();
    for (index, name) in [
        "device.i.re",
        "device.i.im",
        "device.pin_p.i.re",
        "device.pin_p.i.im",
        "device.pin_n.i.re",
        "device.pin_n.i.im",
    ]
    .into_iter()
    .enumerate()
    {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                component_ref: Some(rumoca_core::ComponentReference::from_flat_segments(
                    name,
                    test_span(),
                    Some(rumoca_core::DefId::new(950 + index as u32)),
                )),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let source_def_id = rumoca_core::DefId::new(990);
    let pin_p = var_ref_with_target_def_id("device.pin_p.i", source_def_id);
    let pin_n = var_ref_with_target_def_id("device.pin_n.i", source_def_id);
    let rumoca_core::Expression::VarRef {
        name: expected_pin_p,
        ..
    } = &pin_p
    else {
        unreachable!("test helper returns a var ref");
    };
    let rumoca_core::Expression::VarRef {
        name: expected_pin_n,
        ..
    } = &pin_n
    else {
        unreachable!("test helper returns a var ref");
    };
    let expected_pin_p = expected_pin_p.clone();
    let expected_pin_n = expected_pin_n.clone();
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(pin_p),
            rhs: Box::new(pin_n),
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "device".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    let rumoca_core::Expression::Binary { lhs, rhs, .. } = &model.equations[0].residual else {
        panic!("expected connector balance expression");
    };
    let rumoca_core::Expression::VarRef {
        name: actual_pin_p, ..
    } = lhs.as_ref()
    else {
        panic!("expected pin_p aggregate reference");
    };
    let rumoca_core::Expression::VarRef {
        name: actual_pin_n, ..
    } = rhs.as_ref()
    else {
        panic!("expected pin_n aggregate reference");
    };
    assert_eq!(
        [
            (actual_pin_p.as_str(), actual_pin_p.target_def_id()),
            (actual_pin_n.as_str(), actual_pin_n.target_def_id()),
        ],
        [
            ("device.pin_p.i", Some(source_def_id)),
            ("device.pin_n.i", Some(source_def_id)),
        ]
    );
    assert_eq!(actual_pin_p, &expected_pin_p);
    assert_eq!(actual_pin_n, &expected_pin_n);
}

#[test]
fn collapse_index_refs_collapses_indexed_field_access_to_known_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("port_a[1].Q_flow"),
        flat::Variable {
            name: rumoca_core::VarName::new("port_a[1].Q_flow"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("port_a"),
                    subscripts: vec![],
                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            field: "Q_flow".to_string(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "port_a[1].Q_flow" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_penultimate_preserves_proven_nested_indexed_component_boundary() {
    let mut model = flat::Model::new();
    for name in [
        "stack.cell[1,1].local_reset",
        "stack.cell[1,1].cell.local_reset",
    ] {
        let var_name = rumoca_core::VarName::new(name);
        model.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                component_ref: rumoca_core::component_reference_from_flat_name(
                    &var_name,
                    test_span(),
                ),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let known = KnownFlatVars::build(&model);
    assert!(
        collapse_penultimate_field_to_known_var(
            "stack.cell[1,1].cell.local_reset",
            test_span(),
            &known,
        )
        .is_none()
    );
}

#[test]
fn collapse_penultimate_prefers_direct_indexed_candidate_when_both_spellings_exist() {
    let mut model = flat::Model::new();
    for name in ["states[1].h", "states.h[1]"] {
        let var_name = rumoca_core::VarName::new(name);
        model.add_variable(
            var_name.clone(),
            flat::Variable {
                name: var_name.clone(),
                component_ref: rumoca_core::component_reference_from_flat_name(
                    &var_name,
                    test_span(),
                ),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let known = KnownFlatVars::build(&model);
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = collapse_penultimate_field_to_known_var("states[1].phase.h", test_span(), &known)
        .expect("direct indexed candidate should win")
    else {
        panic!("expected collapsed VarRef");
    };
    assert_eq!(name.as_str(), "states[1].h");
    assert!(name.has_structure());
    assert!(subscripts.is_empty());
}

#[test]
fn collapse_penultimate_supports_symbolic_and_range_indexed_direct_candidates() {
    for (path, expected) in [
        ("states[i].phase.h", "states[i].h"),
        ("states[1:2].phase.h", "states[1:2].h"),
    ] {
        let mut model = flat::Model::new();
        add_primitive_variable(&mut model, expected);
        let known = KnownFlatVars::build(&model);
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = collapse_penultimate_field_to_known_var(path, test_span(), &known)
            .expect("indexed direct candidate should collapse")
        else {
            panic!("expected collapsed VarRef");
        };
        assert_eq!(name.as_str(), expected);
        assert!(subscripts.is_empty());
    }
}

#[test]
fn collapse_penultimate_uses_alternate_projection_and_preserves_reference_structure() {
    let mut model = flat::Model::new();
    let var_name = rumoca_core::VarName::new("states.h[1]");
    model.add_variable(
        var_name.clone(),
        flat::Variable {
            name: var_name.clone(),
            component_ref: rumoca_core::component_reference_from_flat_name(&var_name, test_span()),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let known = KnownFlatVars::build(&model);
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = collapse_penultimate_field_to_known_var("states[1].phase.h", test_span(), &known)
        .expect("alternate array-field projection should collapse")
    else {
        panic!("expected collapsed VarRef");
    };
    assert_eq!(name.as_str(), "states.h[1]");
    assert!(name.has_structure());
    assert!(subscripts.is_empty());
}

#[test]
fn collapse_index_refs_preserves_array_member_aggregate_projection() {
    let mut model = flat::Model::new();
    for name in [
        "vehicle.omega",
        "vehicle.motor[1].omega",
        "vehicle.motor[2].omega",
    ] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    model.add_equation(flat::Equation::new(
        spanned_var_ref("vehicle.motor.omega"),
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "vehicle".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "vehicle.motor.omega" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_repeated_record_field_tail_to_known_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let states = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("pipe.flowModel.states"),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            1,
            rumoca_core::Span::DUMMY,
        )],
        span: rumoca_core::Span::DUMMY,
    };
    let phase = rumoca_core::Expression::FieldAccess {
        base: Box::new(states),
        field: "phase".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    let phase_phase = rumoca_core::Expression::FieldAccess {
        base: Box::new(phase),
        field: "phase".to_string(),
        span: rumoca_core::Span::DUMMY,
    };
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::FieldAccess {
            base: Box::new(phase_phase),
            field: "phase".to_string(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);
    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states[1].phase" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_rendered_repeated_record_field_tail_to_known_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states[1].phase"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states[1].phase.phase.phase"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states[1].phase" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_repeated_record_field_tail_to_array_field_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states.phase[1]"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states.phase[1]"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states[1].phase.phase"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states.phase[1]" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_overexpanded_record_sibling_field_to_array_field_var() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.flowModel.states.h[1]"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.flowModel.states.h[1]"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states[1].phase.h"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "pipe.flowModel.states.h[1]" && subscripts.is_empty()
    ));
}

#[test]
fn collapse_index_refs_collapses_repeated_record_field_tail_to_array_base_ref() {
    let mut model = flat::Model::new();
    let leaf = "pipe.flowModel.states.phase[1]";
    model.add_variable(
        rumoca_core::VarName::new(leaf),
        flat::Variable {
            name: rumoca_core::VarName::new(leaf),
            component_ref: Some(rumoca_core::ComponentReference::from_flat_segments(
                leaf,
                test_span(),
                None,
            )),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("pipe.flowModel.states.phase.phase"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = &model.equations[0].residual
    else {
        panic!("expected collapsed VarRef");
    };
    assert_eq!(name.as_str(), "pipe.flowModel.states.phase");
    assert!(name.has_structure());
    assert!(subscripts.is_empty());
}

#[test]
fn collapse_index_refs_collapses_indexed_var_ref_to_known_scalar_var() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "arr[1]");
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Index {
            base: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("arr"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    collapse_index_refs_to_known_varrefs(&mut model);

    assert!(matches!(
        &model.equations[0].residual,
        rumoca_core::Expression::VarRef { name, subscripts, .. }
            if name.as_str() == "arr[1]" && subscripts.is_empty()
    ));
}

#[test]
fn recover_indexed_lhs_dimensions_does_not_expand_known_symbolic_dimension() {
    let mut model = flat::Model::new();
    let y_name = rumoca_core::VarName::new("aD_Converter.y");
    model.add_variable(
        y_name.clone(),
        flat::Variable {
            name: y_name.clone(),
            dims: vec![7],
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("aD_Converter.y"),
                subscripts: vec![rumoca_core::Subscript::generated_index(
                    8,
                    rumoca_core::Span::DUMMY,
                )],
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(int_literal(0)),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "aD_Converter".to_string(),
        },
    ));

    recover_indexed_lhs_dimensions(&mut model);

    assert_eq!(
        model.variables.get(&y_name).expect("y variable").dims,
        vec![7]
    );
}

#[test]
fn substitutes_known_constants_inside_function_defaults_and_body() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("u", "Real", test_span()).with_default(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("Pkg.Constants.k"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Pkg.Constants.k"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Pkg.Constants.k".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(42.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    assert!(matches!(
        function.inputs[0].default,
        Some(rumoca_core::Expression::Literal { value: rumoca_core::Literal::Real(v), span: rumoca_core::Span::DUMMY }) if (v - 42.0).abs() < f64::EPSILON
    ));
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::Literal { value: rumoca_core::Literal::Real(v), .. } if (*v - 42.0).abs() < f64::EPSILON
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_scoped_relative_constant_alias_field() {
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "medium.steam".to_string(),
        var_ref("Utilities.Water95_Utilities.Constants"),
    );
    ctx.real_parameter_values.insert(
        "medium.Utilities.Water95_Utilities.Constants.R_s".to_string(),
        461.526,
    );

    let substituted = substitute_known_constants_expr(
        var_ref("steam.R_s"),
        &ctx,
        &rustc_hash::FxHashSet::default(),
        &HashSet::new(),
        "medium",
    )
    .unwrap();

    assert!(matches!(
        substituted,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(v),
            ..
        } if (v - 461.526).abs() < f64::EPSILON
    ));
}

#[test]
fn substitutes_assert_condition_with_component_origin_scope() {
    let mut model = flat::Model::new();
    model.assert_equations.push(flat::AssertEquation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var_ref("TD")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        },
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("F or TD has to be positive".to_string()),
            span: test_span(),
        },
        None,
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "line".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.real_parameter_values.insert("TD".to_string(), 0.0);
    ctx.real_parameter_values
        .insert("line.TD".to_string(), 0.001);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let condition = &model.assert_equations[0].condition;
    let condition_uses_scoped_td = match condition {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(true),
            ..
        } => true,
        rumoca_core::Expression::Binary { lhs, .. } => matches!(
            lhs.as_ref(),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if (*value - 0.001).abs() < f64::EPSILON
        ),
        _ => false,
    };
    assert!(
        condition_uses_scoped_td,
        "unexpected condition: {condition:?}"
    );
}

#[test]
fn substituted_assert_condition_prefers_evaluated_scalar_over_stale_default() {
    let mut model = flat::Model::new();
    let condition = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Or,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var_ref("F")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        }),
        rhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Gt,
            lhs: Box::new(var_ref("TD")),
            rhs: Box::new(int_literal(0)),
            span: test_span(),
        }),
        span: test_span(),
    };
    model.assert_equations.push(flat::AssertEquation::new(
        condition,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("F or TD has to be positive".to_string()),
            span: test_span(),
        },
        None,
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "line".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("line.F".to_string(), int_literal(0));
    ctx.constant_values
        .insert("line.TD".to_string(), int_literal(0));
    ctx.real_parameter_values.insert("line.F".to_string(), 0.0);
    ctx.real_parameter_values
        .insert("line.TD".to_string(), 0.001);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let rumoca_core::Expression::Binary { rhs, .. } = &model.assert_equations[0].condition else {
        panic!(
            "unexpected condition: {:?}",
            model.assert_equations[0].condition
        );
    };
    let rumoca_core::Expression::Binary { lhs, .. } = rhs.as_ref() else {
        panic!("unexpected TD comparison: {rhs:?}");
    };
    assert!(matches!(
        lhs.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if (*value - 0.001).abs() < f64::EPSILON
    ));
}

#[test]
fn substitutes_function_scope_constants_inside_defaults_and_body() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("u", "Real", test_span()).with_default(
            rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new("reference_X"),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
        ),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("reference_X"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let reference_x = rumoca_core::Expression::Array {
        elements: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    };
    let mut ctx = Context::new();
    ctx.constant_values
        .insert("Pkg.reference_X".to_string(), reference_x.clone());

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    assert_eq!(function.inputs[0].default, Some(reference_x.clone()));
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => {
            assert_eq!(value, &reference_x);
        }
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_record_array_field_projection_from_flat_var_ref() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("u", "Real", test_span())
            .with_default(var_ref("ConcreteMedium.data.MM")),
    );
    model.add_function(function);

    let record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("DataRecord"),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("__rumoca_named_arg__.MM"),
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(28.0),
                span: rumoca_core::Span::DUMMY,
            }],
            is_constructor: true,
            span: rumoca_core::Span::DUMMY,
        }],
        is_constructor: true,
        span: rumoca_core::Span::DUMMY,
    };
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "ConcreteMedium.data".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![record],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );
    ctx.constant_values.insert(
        "ConcreteMedium.data_alias".to_string(),
        var_ref("PartialMedium.data"),
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    let Some(rumoca_core::Expression::Array { elements, .. }) = &function.inputs[0].default else {
        panic!("expected projected array default");
    };
    assert!(matches!(
        elements.as_slice(),
        [rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        }] if (*value - 28.0).abs() < f64::EPSILON
    ));
}

#[test]
fn substitutes_positional_record_constructor_field_projection() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("u", "Real", test_span())
            .with_default(var_ref("GasData.Air.R")),
    );
    model.add_function(function);

    let mut constructor = rumoca_core::Function::new("DataRecord", Span::DUMMY);
    constructor.is_constructor = true;
    constructor.add_input(rumoca_core::FunctionParam::new(
        "name",
        "String",
        test_span(),
    ));
    constructor.add_input(rumoca_core::FunctionParam::new("R", "Real", test_span()));

    let mut ctx = Context::new();
    ctx.functions
        .insert("DataRecord".to_string(), constructor.clone());
    let record = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("DataRecord"),
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("Air".to_string()),
                span: Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(287.0),
                span: Span::DUMMY,
            },
        ],
        is_constructor: true,
        span: Span::DUMMY,
    };
    ctx.constant_values
        .insert("GasData.Air".to_string(), record.clone());
    ctx.constant_values
        .insert("GasData.Air.R".to_string(), record);

    assert!(
        matches!(
            resolve_projected_constant_path("GasData.Air.R", Span::DUMMY, &ctx),
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            }) if (value - 287.0).abs() < f64::EPSILON
        ),
        "projected constant path should select positional constructor field"
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    let actual = &function.inputs[0].default;
    assert!(
        matches!(
            actual,
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            }) if (*value - 287.0).abs() < f64::EPSILON
        ),
        "expected projected R field, got {actual:?}"
    );
}

#[test]
fn does_not_substitute_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.g", Span::DUMMY);
    function.add_input(rumoca_core::FunctionParam::new("k", "Real", test_span()));
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("k"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "k".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(7.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.g"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "k"
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_substitute_indexed_function_local_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.g_indexed", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("table", "Real", test_span()).with_dims(vec![7, 2]),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("table"),
            subscripts: vec![
                rumoca_core::Subscript::generated_expr(
                    Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("next"),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rumoca_core::Span::DUMMY,
                ),
                rumoca_core::Subscript::generated_index(1, rumoca_core::Span::DUMMY),
            ],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.g_indexed"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => match value {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                assert_eq!(name.as_str(), "table");
                assert_eq!(subscripts.len(), 2);
            }
            other => panic!("expected table varref, got {other:?}"),
        },
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_inline_multi_indexed_constant_varref_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.h", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Modelica.Blocks.Sources.IntegerTable.table[1,1]"),
            subscripts: vec![],
            span: test_span(),
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Modelica.Blocks.Sources.IntegerTable.table".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.h"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => match value {
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => {
                assert!(matches!(
                    base.as_ref(),
                    rumoca_core::Expression::Array { elements, is_matrix, .. }
                        if !*is_matrix && elements.len() == 2
                ));
                assert_eq!(subscripts.len(), 2);
                assert!(matches!(
                    &subscripts[0],
                    rumoca_core::Subscript::Expr { expr, .. }
                        if matches!(expr.as_ref(), rumoca_core::Expression::Literal { value: rumoca_core::Literal::Integer(1), .. })
                ));
                assert!(matches!(
                    &subscripts[1],
                    rumoca_core::Subscript::Expr { expr, .. }
                        if matches!(expr.as_ref(), rumoca_core::Expression::Literal { value: rumoca_core::Literal::Integer(1), .. })
                ));
            }
            other => panic!("expected indexed expression, got {other:?}"),
        },
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_indexed_array_comprehension_parameter_binding_as_scalar_element() {
    let span = test_span();
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "fluidVolumes".to_string(),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("crossAreas"),
                        subscripts: vec![rumoca_core::Subscript::Expr {
                            expr: Box::new(var_ref("i")),
                            span,
                        }],
                        span,
                    }),
                    rhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::new("lengths"),
                        subscripts: vec![rumoca_core::Subscript::Expr {
                            expr: Box::new(var_ref("i")),
                            span,
                        }],
                        span,
                    }),
                    span,
                }),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(int_literal(1)),
                        step: None,
                        end: Box::new(int_literal(2)),
                        span,
                    },
                }],
                filter: None,
                span,
            }),
            rhs: Box::new(var_ref("nParallel")),
            span,
        },
    );
    ctx.constant_values
        .insert("nParallel".to_string(), int_literal(3));

    let mut live_vars = rustc_hash::FxHashSet::default();
    live_vars.insert("crossAreas".to_string());
    live_vars.insert("lengths".to_string());
    let substituted = substitute_known_constants_expr(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("fluidVolumes"),
            subscripts: vec![rumoca_core::Subscript::generated_index(2, span)],
            span,
        },
        &ctx,
        &live_vars,
        &HashSet::new(),
        "",
    )
    .expect("indexed comprehension parameter should substitute");

    let rumoca_core::Expression::Binary { lhs, rhs, .. } = substituted else {
        panic!("expected scalar product, got {substituted:?}");
    };
    assert!(matches!(
        rhs.as_ref(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            ..
        }
    ));
    let rumoca_core::Expression::Binary {
        lhs: cross_area,
        rhs: length,
        ..
    } = lhs.as_ref()
    else {
        panic!("expected selected comprehension body, got {lhs:?}");
    };
    assert_indexed_var_ref(cross_area, "crossAreas", 2);
    assert_indexed_var_ref(length, "lengths", 2);
}

#[test]
fn substitutes_symbolically_indexed_array_comprehension_parameter_binding() {
    let span = test_span();
    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "fluidVolumes".to_string(),
        rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("crossAreas"),
                    subscripts: vec![rumoca_core::Subscript::Expr {
                        expr: Box::new(var_ref("j")),
                        span,
                    }],
                    span,
                }),
                rhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new("lengths"),
                    subscripts: vec![rumoca_core::Subscript::Expr {
                        expr: Box::new(var_ref("j")),
                        span,
                    }],
                    span,
                }),
                span,
            }),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: "j".to_string(),
                range: rumoca_core::Expression::Range {
                    start: Box::new(int_literal(1)),
                    step: None,
                    end: Box::new(int_literal(2)),
                    span,
                },
            }],
            filter: None,
            span,
        },
    );

    let mut live_vars = rustc_hash::FxHashSet::default();
    live_vars.insert("crossAreas".to_string());
    live_vars.insert("lengths".to_string());
    let substituted = substitute_known_constants_expr(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("fluidVolumes"),
            subscripts: vec![rumoca_core::Subscript::Expr {
                expr: Box::new(var_ref("i")),
                span,
            }],
            span,
        },
        &ctx,
        &live_vars,
        &HashSet::new(),
        "",
    )
    .expect("symbolically indexed comprehension parameter should substitute");

    let rumoca_core::Expression::Binary { lhs, rhs, .. } = substituted else {
        panic!("expected selected comprehension body, got {substituted:?}");
    };
    assert_symbolically_indexed_var_ref(&lhs, "crossAreas", "i");
    assert_symbolically_indexed_var_ref(&rhs, "lengths", "i");
}

#[test]
fn rejects_unspanned_inline_indexed_constant_varref_names() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.unspanned_inline", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("Pkg.table[1]"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "Pkg.table".to_string(),
        rumoca_core::Expression::Array {
            elements: vec![int_literal(1)],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        },
    );

    match substitute_known_constants_in_flat(&mut model, &ctx) {
        Err(FlattenError::MissingSourceContext { reason }) => assert!(
            reason.contains("flatten inline indexed constant"),
            "unexpected reason: {reason}"
        ),
        other => panic!("expected missing-source-context error, got {other:?}"),
    }
}

#[test]
fn inline_indexed_name_uses_structured_scalar_name_parser() {
    assert_eq!(
        split_inline_indexed_name("table[1, 2]"),
        Some(("table", vec![1, 2]))
    );
    assert_eq!(
        split_inline_indexed_name("pkg.table[index.with.dot].value[3]"),
        Some(("pkg.table[index.with.dot].value", vec![3]))
    );
    assert!(split_inline_indexed_name("table").is_none());
    assert!(split_inline_indexed_name("table[1").is_none());
    assert!(split_inline_indexed_name("[1]").is_none());
    assert!(split_inline_indexed_name("table[index.with.dot]").is_none());
}

#[test]
fn does_not_substitute_inline_indexed_varref_when_base_is_local() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.inline_local", Span::DUMMY);
    function.add_input(
        rumoca_core::FunctionParam::new("table", "Real", test_span()).with_dims(vec![7, 2]),
    );
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("table[1,1]"),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "table".to_string(),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.inline_local"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, subscripts, .. }
                if name.as_str() == "table[1,1]" && subscripts.is_empty()
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn substitutes_variable_attribute_constants_in_variable_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.medium.X");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("tank.medium.X"))
        .expect("variable should exist")
        .start = Some(var_ref("reference_X"));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "tank.medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("tank.medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let start = model
        .variables
        .get(&rumoca_core::VarName::new("tank.medium.X"))
        .expect("variable should exist")
        .start
        .as_ref()
        .expect("start attribute should remain");
    assert!(!expr_contains_var_ref(start, "reference_X"));
    assert!(!expr_contains_var_ref(start, "nS"));
}

#[test]
fn substitutes_component_equation_constants_in_origin_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.medium.X");
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("tank.medium.X")),
            rhs: Box::new(var_ref("reference_X")),
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank.medium".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.constant_values.insert(
        "tank.medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("tank.medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let residual = &model.equations[0].residual;
    assert!(!expr_contains_var_ref(residual, "reference_X"));
    assert!(!expr_contains_var_ref(residual, "nS"));
}

#[test]
fn substitutes_fully_qualified_constant_alias_in_declaration_scope() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "tank.X_start");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("tank.X_start"))
        .expect("variable should exist")
        .start = Some(var_ref("Pkg.Medium.X_default"));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("Pkg.Medium.X_default".to_string(), var_ref("reference_X"));
    ctx.constant_values.insert(
        "Pkg.Medium.reference_X".to_string(),
        reference_x_fill_expr(),
    );
    ctx.parameter_values.insert("Pkg.Medium.nS".to_string(), 1);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let start = model
        .variables
        .get(&rumoca_core::VarName::new("tank.X_start"))
        .expect("variable should exist")
        .start
        .as_ref()
        .expect("start attribute should remain");
    assert!(!expr_contains_var_ref(start, "Pkg.Medium.X_default"));
    assert!(!expr_contains_var_ref(start, "reference_X"));
    assert!(!expr_contains_var_ref(start, "nS"));
}

#[test]
fn scoped_relative_alias_keeps_live_flat_variable_before_constant_expansion() {
    let mut model = flat::Model::new();
    add_primitive_variable(&mut model, "jointRRP.e_ia");
    add_primitive_variable(&mut model, "jointRRP.jointUSP.e2_ia");
    add_primitive_variable(&mut model, "jointRRP.jointUSP.rod1.e2_ia");
    model
        .variables
        .get_mut(&rumoca_core::VarName::new("jointRRP.e_ia"))
        .expect("variable should exist")
        .binding = Some(var_ref("jointUSP.e2_ia"));

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("jointRRP.jointUSP.e2_ia".to_string(), var_ref("rod1.e2_ia"));
    ctx.constant_values.insert(
        "jointRRP.rod1.e2_ia".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("jointRRP.e_ia"))
        .expect("variable should exist")
        .binding
        .as_ref()
        .expect("binding should remain");
    assert!(matches!(
        binding,
        rumoca_core::Expression::VarRef { name, .. }
            if name.as_str() == "jointRRP.jointUSP.e2_ia"
    ));
}

#[test]
fn does_not_substitute_array_shaped_scalar_parameter_ref() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        spanned_var_ref("CriticalDamping.c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.real_parameter_values
        .insert("CriticalDamping.c0".to_string(), 0.0);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "CriticalDamping.c0" && subscripts.is_empty()
    ));
}

#[test]
fn does_not_substitute_scoped_zero_length_array_parameter_ref() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        var_ref("c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.real_parameter_values
        .insert("CriticalDamping.c0".to_string(), 0.0);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "c0" && subscripts.is_empty()
    ));
}

#[test]
fn does_not_substitute_array_shaped_scalar_constant_expr() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        var_ref("c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);
    ctx.constant_values.insert(
        "CriticalDamping.c0".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: rumoca_core::Span::DUMMY,
        },
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    assert!(matches!(
        model.equations[0].residual,
        rumoca_core::Expression::VarRef { ref name, ref subscripts, .. }
            if name.as_str() == "c0" && subscripts.is_empty()
    ));
}

#[test]
fn materializes_referenced_zero_sized_array_declaration() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        spanned_var_ref("CriticalDamping.c0"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "CriticalDamping".to_string(),
        },
    ));

    let mut ctx = Context::new();
    ctx.array_dimensions
        .insert("CriticalDamping.c0".to_string(), vec![0]);

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let var = model
        .variables
        .get(&rumoca_core::VarName::new("CriticalDamping.c0"))
        .expect("zero-sized referenced array should have a Flat declaration");
    assert_eq!(var.dims, vec![0]);
    assert!(var.is_primitive);
}

#[test]
fn materializes_unspanned_zero_sized_array_reference_from_dimension_provenance() {
    let mut model = flat::Model::new();
    model.equations.push(flat::Equation::new(
        var_ref("Modelica.Media.Water.WaterIF97_base.C_default"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "PumpingSystem".to_string(),
        },
    ));

    let mut ctx = Context::new();
    let name = "Modelica.Media.Water.WaterIF97_base.C_default";
    ctx.array_dimensions.insert(name.to_string(), vec![0]);
    ctx.array_dimension_spans
        .insert(name.to_string(), test_span());

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let var = model
        .variables
        .get(&rumoca_core::VarName::new(name))
        .expect("zero-sized referenced array should have a Flat declaration");
    assert_eq!(var.dims, vec![0]);
    assert_eq!(var.source_span, test_span());
    assert!(
        var.component_ref.is_some(),
        "materialized zero-sized array variable should carry structured metadata"
    );
}

#[test]
fn substitutes_field_access_on_zero_arg_constructor_constants() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.k", Span::DUMMY);
    function
        .body
        .push(simple_assignment(rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new(
                    "Modelica.Electrical.Batteries.ParameterRecords.ExampleData",
                ),
                args: vec![],
                is_constructor: true,
                span: rumoca_core::Span::DUMMY,
            }),
            field: "useLinearSOCDependency".to_string(),
            span: rumoca_core::Span::DUMMY,
        }));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.boolean_parameter_values.insert(
        "Modelica.Electrical.Batteries.ParameterRecords.ExampleData.useLinearSOCDependency"
            .to_string(),
        false,
    );

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.k"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(false),
                span: rumoca_core::Span::DUMMY
            }
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

#[test]
fn does_not_resolve_function_local_record_root_through_constant_alias() {
    let mut model = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.f", Span::DUMMY);
    function.add_output(rumoca_core::FunctionParam::new(
        "g",
        "Common.GibbsDerivs",
        test_span(),
    ));
    function.body.push(simple_assignment(var_ref("g.tau")));
    model.add_function(function);

    let mut ctx = Context::new();
    ctx.constant_values
        .insert("g".to_string(), var_ref("Modelica.Constants.g_n"));

    substitute_known_constants_in_flat(&mut model, &ctx).unwrap();

    let function = model
        .functions
        .get(&rumoca_core::VarName::new("Pkg.f"))
        .expect("function should exist");
    match &function.body[0] {
        rumoca_core::Statement::Assignment { value, .. } => assert!(matches!(
            value,
            rumoca_core::Expression::VarRef { name, .. } if name.as_str() == "g.tau"
        )),
        other => panic!("expected assignment statement, got {other:?}"),
    }
}

fn expr_contains_var_ref(expr: &rumoca_core::Expression, needle: &str) -> bool {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name.as_str() == needle,
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var_ref(lhs, needle) || expr_contains_var_ref(rhs, needle)
        }
        rumoca_core::Expression::Unary { rhs, .. } => expr_contains_var_ref(rhs, needle),
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            args.iter().any(|arg| expr_contains_var_ref(arg, needle))
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_var_ref(condition, needle) || expr_contains_var_ref(value, needle)
            }) || expr_contains_var_ref(else_branch, needle)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            expr_contains_var_ref(start, needle)
                || step
                    .as_ref()
                    .is_some_and(|step| expr_contains_var_ref(step, needle))
                || expr_contains_var_ref(end, needle)
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expr_contains_var_ref(expr, needle)
                || indices
                    .iter()
                    .any(|index| expr_contains_var_ref(&index.range, needle))
                || filter
                    .as_ref()
                    .is_some_and(|filter| expr_contains_var_ref(filter, needle))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            expr_contains_var_ref(base, needle)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        expr_contains_var_ref(expr, needle)
                    }
                    rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => {
                        false
                    }
                })
        }
        rumoca_core::Expression::FieldAccess { base, .. } => expr_contains_var_ref(base, needle),
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => false,
    }
}
