use super::*;
use rumoca_core::Span;

/// Build a VarRef expression.
fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn var_ref_with_expr_subscript(
    name: &str,
    subscript: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new(name).into(),
        subscripts: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(subscript),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    }
}

/// Build a binary subtraction: lhs - rhs.
fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn mul(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn div(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn int_lit(value: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Integer(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn der(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args: vec![expr],
        span: rumoca_core::Span::DUMMY,
    }
}

fn zeros(dim: i64) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Zeros,
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(dim),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    }
}

fn function_call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
    function_call_with_span(name, args, rumoca_core::Span::DUMMY)
}

fn function_call_with_span(
    name: &str,
    args: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(name).into(),
        args,
        is_constructor: false,
        span,
    }
}

fn parameter(name: &str, start: Option<rumoca_core::Expression>) -> dae::Variable {
    dae::Variable {
        name: rumoca_core::VarName::new(name),
        start,
        ..Default::default()
    }
}

/// Collect all VarRef names from an expression (for assertions).
fn all_var_names(expr: &rumoca_core::Expression) -> Vec<String> {
    let mut names = Vec::new();
    collect_var_names_rec(expr, &mut names);
    names
}

fn collect_var_names_rec(expr: &rumoca_core::Expression, names: &mut Vec<String>) {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                names.push(name.as_str().to_string());
            } else {
                // Format with subscripts for clarity
                let subs: Vec<String> = subscripts
                    .iter()
                    .map(|s| match s {
                        rumoca_core::Subscript::Index { value: i, .. } => format!("{i}"),
                        rumoca_core::Subscript::Expr { expr, .. } => {
                            collect_var_names_rec(expr, names);
                            "?".to_string()
                        }
                        _ => "?".to_string(),
                    })
                    .collect();
                names.push(format!("{}[{}]", name.as_str(), subs.join(",")));
            }
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_var_names_rec(lhs, names);
            collect_var_names_rec(rhs, names);
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            collect_var_names_rec(rhs, names);
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_var_names_rec(arg, names);
            }
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_var_names_rec(element, names);
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_var_names_rec(condition, names);
                collect_var_names_rec(value, names);
            }
            collect_var_names_rec(else_branch, names);
        }
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => {
            collect_var_names_rec(base, names);
        }
        _ => {}
    }
}

#[test]
fn test_parameter_start_dependency_sort_deduplicates_repeated_refs() {
    let mut dae = dae::Dae::default();
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("a"),
        parameter("a", Some(int_lit(1))),
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("b"),
        parameter("b", Some(sub(var_ref("a"), var_ref("a")))),
    );

    sort_parameters_by_start_dependency(&mut dae);

    let names: Vec<_> = dae
        .variables
        .parameters
        .keys()
        .map(|name| name.as_str())
        .collect();
    assert_eq!(names, vec!["a", "b"]);
}

#[test]
fn test_scalarize_phantom_vector_equations() {
    // Set up a DAE that mimics the TransformerYY pattern:
    // - sineVoltage.v is a declared 3-element array variable
    // - sineVoltage.plug_p.pin[1].v, [2].v, [3].v are individual scalars
    // - sineVoltage.plug_n.pin[1].v, [2].v, [3].v are individual scalars
    // - Equation: sineVoltage.v - (sineVoltage.plug_p.pin.v - sineVoltage.plug_n.pin.v) = 0
    //   with scalar_count = 3

    let mut dae = Dae::new();

    // Declared array variable
    let mut sv_v = dae::Variable::new(rumoca_core::VarName::new("sineVoltage.v"));
    sv_v.dims = vec![3];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("sineVoltage.v"), sv_v);

    // Scalarized connector pin variables (no dims — they're individual scalars)
    for k in 1..=3 {
        let name_p = format!("sineVoltage.plug_p.pin[{k}].v");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name_p),
            dae::Variable::new(rumoca_core::VarName::new(&name_p)),
        );
        let name_n = format!("sineVoltage.plug_n.pin[{k}].v");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name_n),
            dae::Variable::new(rumoca_core::VarName::new(&name_n)),
        );
    }

    // Vector equation: sineVoltage.v - (sineVoltage.plug_p.pin.v - sineVoltage.plug_n.pin.v) = 0
    let eq_rhs = sub(
        var_ref("sineVoltage.v"),
        sub(
            var_ref("sineVoltage.plug_p.pin.v"),
            var_ref("sineVoltage.plug_n.pin.v"),
        ),
    );
    let eq = dae::Equation::residual_array(eq_rhs, Span::DUMMY, "test equation", 3);
    dae.continuous.equations.push(eq);

    // Run scalarization
    scalarize_phantom_vector_equations(&mut dae).unwrap();

    // Should now have 3 scalar equations instead of 1 vector equation
    assert_eq!(
        dae.continuous.equations.len(),
        3,
        "expected 3 scalar equations, got {}",
        dae.continuous.equations.len()
    );

    for (k, eq) in dae.continuous.equations.iter().enumerate() {
        assert_eq!(eq.scalar_count, 1, "equation {k} should be scalar");

        let names = all_var_names(&eq.rhs);
        // Should NOT contain phantom base names
        assert!(
            !names.iter().any(|n| n == "sineVoltage.plug_p.pin.v"),
            "equation {k} still has phantom ref: {names:?}"
        );
        assert!(
            !names.iter().any(|n| n == "sineVoltage.plug_n.pin.v"),
            "equation {k} still has phantom ref: {names:?}"
        );
        // Should contain the indexed variants
        let expected_p = format!("sineVoltage.plug_p.pin[{}].v", k + 1);
        let expected_n = format!("sineVoltage.plug_n.pin[{}].v", k + 1);
        assert!(
            names.iter().any(|n| n == &expected_p),
            "equation {k} missing {expected_p}: {names:?}"
        );
        assert!(
            names.iter().any(|n| n == &expected_n),
            "equation {k} missing {expected_n}: {names:?}"
        );
        // sineVoltage.v should be subscripted with [k+1]
        let expected_sv = format!("sineVoltage.v[{}]", k + 1);
        assert!(
            names.iter().any(|n| n == &expected_sv),
            "equation {k} missing {expected_sv}: {names:?}"
        );
    }
}

#[test]
fn test_scalarize_phantom_vector_equations_visits_event_partitions() {
    let mut dae = Dae::new();
    let mut vector_var = dae::Variable::new(rumoca_core::VarName::new("sensor.v"));
    vector_var.dims = vec![2];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("sensor.v"), vector_var);

    for k in 1..=2 {
        let name = format!("sensor.pin[{k}].v");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name),
            dae::Variable::new(rumoca_core::VarName::new(&name)),
        );
    }

    let rhs = sub(var_ref("sensor.v"), var_ref("sensor.pin.v"));
    dae.discrete
        .real_updates
        .push(dae::Equation::residual_array(
            rhs.clone(),
            Span::DUMMY,
            "discrete real",
            2,
        ));
    dae.discrete
        .valued_updates
        .push(dae::Equation::residual_array(
            rhs.clone(),
            Span::DUMMY,
            "discrete valued",
            2,
        ));
    dae.conditions.equations.push(dae::Equation::residual_array(
        rhs,
        Span::DUMMY,
        "condition",
        2,
    ));

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    for equations in [
        &dae.discrete.real_updates,
        &dae.discrete.valued_updates,
        &dae.conditions.equations,
    ] {
        assert_eq!(equations.len(), 2);
        for (k, eq) in equations.iter().enumerate() {
            assert_eq!(eq.scalar_count, 1);
            let names = all_var_names(&eq.rhs);
            assert!(!names.iter().any(|name| name == "sensor.pin.v"));
            assert!(
                names
                    .iter()
                    .any(|name| name == &format!("sensor.pin[{}].v", k + 1))
            );
        }
    }
}

#[test]
fn test_scalarize_phantom_vector_equations_preserves_event_assignment_lhs() {
    let mut dae = Dae::new();
    let mut vector_var = dae::Variable::new(rumoca_core::VarName::new("switch.off"));
    vector_var.dims = vec![2];
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("switch.off"), vector_var);

    for k in 1..=2 {
        let name = format!("switch.idealSwitch[{k}].off");
        dae.variables.discrete_valued.insert(
            rumoca_core::VarName::new(&name),
            dae::Variable::new(rumoca_core::VarName::new(&name)),
        );
    }

    dae.discrete.valued_updates.push(dae::Equation::explicit(
        rumoca_core::VarName::new("switch.off"),
        var_ref("switch.idealSwitch.off"),
        Span::DUMMY,
        "discrete valued assignment",
    ));
    dae.discrete.valued_updates[0].scalar_count = 2;

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    assert_eq!(dae.discrete.valued_updates.len(), 2);
    for (k, eq) in dae.discrete.valued_updates.iter().enumerate() {
        assert_eq!(
            eq.lhs.as_ref().map(|lhs| lhs.as_str()),
            Some(format!("switch.off[{}]", k + 1).as_str())
        );
        let names = all_var_names(&eq.rhs);
        assert_eq!(names, vec![format!("switch.idealSwitch[{}].off", k + 1)]);
    }
}

#[test]
fn scalarize_phantom_vector_equations_reports_missing_function_shape_with_call_span() {
    let mut dae = Dae::new();
    for k in 1..=2 {
        let name = format!("plug.pin[{k}].v");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name),
            dae::Variable::new(rumoca_core::VarName::new(&name)),
        );
    }

    let call_span = Span::from_offsets(rumoca_core::SourceId(17), 23, 41);
    dae.continuous.equations.push(dae::Equation::residual_array(
        function_call_with_span("missingShape", vec![var_ref("plug.pin.v")], call_span),
        call_span,
        "missing function shape",
        2,
    ));

    let err = scalarize_phantom_vector_equations(&mut dae)
        .expect_err("missing function metadata should be a spanned DAE error");
    match err {
        ToDaeError::RuntimeContractViolation { detail, span } => {
            assert!(detail.contains("missing function output metadata for `missingShape`"));
            assert_eq!(span, rumoca_core::span_to_source_span(call_span));
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn test_scalarize_singleton_phantom_connector_reference() {
    let mut dae = Dae::new();
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("boundary.ports[1].C_outflow"),
        dae::Variable::new(rumoca_core::VarName::new("boundary.ports[1].C_outflow")),
    );
    dae.variables.parameters.insert(
        rumoca_core::VarName::new("boundary.C"),
        dae::Variable::new(rumoca_core::VarName::new("boundary.C")),
    );

    dae.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var_ref("boundary.ports.C_outflow")),
            rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args: vec![
                    var_ref("boundary.C"),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Integer(1),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: Span::DUMMY,
        origin: "equation from boundary".to_string(),
        scalar_count: 1,
    });

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(dae.continuous.equations[0].scalar_count, 1);
    let names = all_var_names(&dae.continuous.equations[0].rhs);
    assert!(names.contains(&"boundary.ports[1].C_outflow".to_string()));
    assert!(names.contains(&"boundary.C".to_string()));
    assert!(!names.contains(&"boundary.ports.C_outflow".to_string()));
}

#[test]
fn test_canonicalizes_trailing_embedded_subscript_to_var_ref_subscript() {
    let mut dae = Dae::new();
    let mut voltage = dae::Variable::new(rumoca_core::VarName::new("battery.pin.v"));
    voltage.dims = vec![1];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("battery.pin.v"), voltage);
    dae.continuous.equations.push(dae::Equation::residual(
        var_ref("battery.pin.v[1]"),
        Span::DUMMY,
        "connection equation: battery.pin.v[1] = other.v[1]",
    ));

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = &dae.continuous.equations[0].rhs
    else {
        panic!("expected canonical var ref");
    };
    assert_eq!(name.as_str(), "battery.pin.v");
    assert_eq!(subscripts.len(), 1);
}

#[test]
fn test_scalarize_vector_binding_preserves_array_comprehension_without_phantom_refs() {
    let mut dae = Dae::new();
    let mut vs = dae::Variable::new(rumoca_core::VarName::new("pipe.vs"));
    vs.dims = vec![2];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("pipe.vs"), vs);

    let rhs = sub(
        var_ref("pipe.vs"),
        div(
            rumoca_core::Expression::ArrayComprehension {
                expr: Box::new(var_ref_with_expr_subscript("pipe.m_flows", var_ref("i"))),
                indices: vec![rumoca_core::ComprehensionIndex {
                    name: "i".to_string(),
                    range: rumoca_core::Expression::Range {
                        start: Box::new(int_lit(1)),
                        step: None,
                        end: Box::new(var_ref("pipe.n")),
                        span: rumoca_core::Span::DUMMY,
                    },
                }],
                filter: None,
                span: rumoca_core::Span::DUMMY,
            },
            var_ref("pipe.nParallel"),
        ),
    );
    dae.continuous.equations.push(dae::Equation::residual_array(
        rhs,
        Span::DUMMY,
        "binding equation for pipe.vs",
        2,
    ));

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(dae.continuous.equations[0].scalar_count, 2);
    assert!(
        expr_has_array_comprehension(&dae.continuous.equations[0].rhs),
        "array comprehension should remain structured until an explicit scalar backend asks for selection"
    );
    let names = all_var_names(&dae.continuous.equations[0].rhs);
    assert!(
        names.iter().any(|name| name == "pipe.vs"),
        "lhs array variable should remain aggregate: {names:?}"
    );
    assert!(
        !names.iter().any(|name| name == "pipe.vs[1]"),
        "array variable should not be scalarized without phantom refs: {names:?}"
    );
}

#[test]
fn test_scalarize_phantom_vector_equations_selects_zeros() {
    let mut dae = Dae::new();

    for k in 1..=3 {
        let name_p = format!("sensor.plug_p.pin[{k}].i");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name_p),
            dae::Variable::new(rumoca_core::VarName::new(&name_p)),
        );
        let name_n = format!("sensor.plug_n.pin[{k}].i");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name_n),
            dae::Variable::new(rumoca_core::VarName::new(&name_n)),
        );
    }

    let eq_rhs = sub(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var_ref("sensor.plug_p.pin.i")),
            rhs: Box::new(var_ref("sensor.plug_n.pin.i")),
            span: rumoca_core::Span::DUMMY,
        },
        zeros(3),
    );
    dae.continuous.equations.push(dae::Equation::residual_array(
        eq_rhs,
        Span::DUMMY,
        "phantom connector current conservation",
        3,
    ));

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    assert_eq!(dae.continuous.equations.len(), 3);
    for (idx, eq) in dae.continuous.equations.iter().enumerate() {
        assert_eq!(eq.scalar_count, 1);
        let rumoca_core::Expression::Binary { rhs, .. } = &eq.rhs else {
            panic!("expected scalarized subtraction residual");
        };
        assert!(
            matches!(
                rhs.as_ref(),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    ..
                }
            ),
            "zeros() should be selected to scalar 0.0 in equation {idx}: {:?}",
            eq.rhs
        );
        let names = all_var_names(&eq.rhs);
        assert!(
            names
                .iter()
                .any(|name| name == &format!("sensor.plug_p.pin[{}].i", idx + 1)),
            "equation {idx} missing indexed positive pin: {names:?}"
        );
    }
}

#[test]
fn test_scalarize_preserves_vector_function_arguments_for_array_output() {
    let mut dae = Dae::new();

    let mut y = dae::Variable::new(rumoca_core::VarName::new("sensor.y"));
    y.dims = vec![2];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("sensor.y"), y);

    for k in 1..=3 {
        let name_p = format!("sensor.plug_p.pin[{k}].v");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name_p),
            dae::Variable::new(rumoca_core::VarName::new(&name_p)),
        );
        let name_n = format!("sensor.plug_n.pin[{k}].v");
        dae.variables.algebraics.insert(
            rumoca_core::VarName::new(&name_n),
            dae::Variable::new(rumoca_core::VarName::new(&name_n)),
        );
    }

    let mut function = rumoca_core::Function::new("Space.ToSpacePhasor", Span::DUMMY);
    function.outputs.push(rumoca_core::FunctionParam {
        def_id: None,
        name: "y".to_string(),
        span: Span::DUMMY,
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![2],
        shape_expr: Vec::new(),
        default: None,
        description: None,
    });
    dae.symbols
        .functions
        .insert(function.name.clone(), function);

    let vector_arg = sub(
        var_ref("sensor.plug_p.pin.v"),
        var_ref("sensor.plug_n.pin.v"),
    );
    let eq_rhs = sub(
        var_ref("sensor.y"),
        function_call("Space.ToSpacePhasor", vec![vector_arg]),
    );
    dae.continuous.equations.push(dae::Equation::residual_array(
        eq_rhs,
        Span::DUMMY,
        "space phasor equation",
        2,
    ));

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    assert_eq!(dae.continuous.equations.len(), 2);
    for (idx, eq) in dae.continuous.equations.iter().enumerate() {
        assert_eq!(eq.scalar_count, 1);
        let rumoca_core::Expression::Binary { rhs, .. } = &eq.rhs else {
            panic!("expected scalarized residual subtraction");
        };
        let rumoca_core::Expression::Index {
            base, subscripts, ..
        } = rhs.as_ref()
        else {
            panic!("array-output function call should be indexed");
        };
        assert_eq!(
            subscripts,
            &[rumoca_core::Subscript::generated_index(
                (idx + 1) as i64,
                rumoca_core::Span::DUMMY,
            )]
        );
        let rumoca_core::Expression::FunctionCall { args, .. } = base.as_ref() else {
            panic!("indexed expression should wrap the function call");
        };
        let rumoca_core::Expression::Array { elements, .. } = &args[0] else {
            panic!("phantom vector function argument should become an array literal");
        };
        assert_eq!(elements.len(), 3);
        let names = all_var_names(&args[0]);
        assert!(
            !names.iter().any(|name| name == "sensor.plug_p.pin.v"),
            "function argument still contains phantom positive pin: {names:?}"
        );
        assert!(
            names.iter().any(|name| name == "sensor.plug_p.pin[3].v"),
            "function argument did not preserve all vector elements: {names:?}"
        );
    }
}

#[test]
fn test_scalarize_leaves_scalar_equations_unchanged() {
    let mut dae = Dae::new();

    // A simple scalar equation: x - y = 0
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable::new(rumoca_core::VarName::new("x")),
    );
    dae.variables.algebraics.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y")),
    );

    let eq = dae::Equation::residual(sub(var_ref("x"), var_ref("y")), Span::DUMMY, "scalar eq");
    dae.continuous.equations.push(eq);

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    // Should remain 1 equation
    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(dae.continuous.equations[0].scalar_count, 1);
}

#[test]
fn test_scalarize_ignores_vector_equations_without_phantom_refs() {
    let mut dae = Dae::new();

    // Both variables are declared arrays — no phantom refs
    let mut var_a = dae::Variable::new(rumoca_core::VarName::new("a"));
    var_a.dims = vec![3];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("a"), var_a);

    let mut var_b = dae::Variable::new(rumoca_core::VarName::new("b"));
    var_b.dims = vec![3];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("b"), var_b);

    let eq =
        dae::Equation::residual_array(sub(var_ref("a"), var_ref("b")), Span::DUMMY, "vector eq", 3);
    dae.continuous.equations.push(eq);

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    // No phantom refs exist — both a and b are properly declared array vars.
    // The pass should leave the equation unchanged (vector ops are fine for backends).
    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(dae.continuous.equations[0].scalar_count, 3);
}

#[test]
fn test_scalarize_preserves_declared_matrix_vector_equations() {
    let mut dae = Dae::new();

    let mut j = dae::Variable::new(rumoca_core::VarName::new("J"));
    j.dims = vec![3, 3];
    dae.variables
        .parameters
        .insert(rumoca_core::VarName::new("J"), j);

    let mut omega = dae::Variable::new(rumoca_core::VarName::new("omega"));
    omega.dims = vec![3];
    dae.variables
        .states
        .insert(rumoca_core::VarName::new("omega"), omega);

    let mut m_body = dae::Variable::new(rumoca_core::VarName::new("M_body"));
    m_body.dims = vec![3];
    dae.variables
        .algebraics
        .insert(rumoca_core::VarName::new("M_body"), m_body);

    let eq = dae::Equation::residual_array(
        sub(mul(var_ref("J"), der(var_ref("omega"))), var_ref("M_body")),
        Span::DUMMY,
        "matrix vector equation",
        3,
    );
    dae.continuous.equations.push(eq);

    scalarize_phantom_vector_equations(&mut dae).unwrap();

    assert_eq!(dae.continuous.equations.len(), 1);
    assert_eq!(dae.continuous.equations[0].scalar_count, 3);
    assert_eq!(
        all_var_names(&dae.continuous.equations[0].rhs),
        vec!["J", "omega", "M_body"],
        "declared matrix/vector equation should remain symbolic for structural scalarization"
    );
}
