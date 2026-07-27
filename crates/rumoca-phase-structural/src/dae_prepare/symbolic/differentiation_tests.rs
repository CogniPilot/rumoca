//! Capability tests for [`super::symbolic_time_derivative`].
//!
//! Each case is one construct that MSL's `Modelica.Mechanics.MultiBody.Frames`
//! loop-closure constraints are written with, and that the differentiator has to
//! handle for a position-level constraint to reach velocity level.

use super::*;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("symbolic_differentiation_test.mo"),
        1,
        20,
    )
}

fn state(dae: &mut Dae, name: &str, dims: Vec<i64>) {
    let mut variable = Variable::new(VarName::new(name), test_span());
    variable.source_span = test_span();
    variable.dims = dims;
    dae.variables.states.insert(VarName::new(name), variable);
}

fn var(name: &str) -> Expression {
    Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: Vec::new(),
        span: test_span(),
    }
}

fn slice(name: &str, row: i64) -> Expression {
    Expression::Index {
        base: Box::new(var(name)),
        subscripts: vec![
            Subscript::generated_index(row, test_span()),
            Subscript::Colon { span: test_span() },
        ],
        span: test_span(),
    }
}

fn builtin(function: BuiltinFunction, args: Vec<Expression>) -> Expression {
    Expression::BuiltinCall {
        function,
        args,
        span: test_span(),
    }
}

fn der_leaf_names(expr: &Expression) -> Vec<String> {
    struct Collector {
        names: Vec<String>,
    }
    impl rumoca_core::ExpressionVisitor for Collector {
        fn visit_expression(&mut self, expr: &Expression) {
            if let Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } = expr
                && let Some(Expression::VarRef {
                    name, subscripts, ..
                }) = args.first()
            {
                let indices = subscripts
                    .iter()
                    .filter_map(|subscript| match subscript {
                        Subscript::Index { value, .. } => Some(value.to_string()),
                        _ => None,
                    })
                    .collect::<Vec<_>>()
                    .join(",");
                self.names.push(format!("{}[{indices}]", name.as_str()));
            }
            self.walk_expression(expr);
        }
    }
    let mut collector = Collector { names: Vec::new() };
    rumoca_core::ExpressionVisitor::visit_expression(&mut collector, expr);
    collector.names.sort();
    collector.names.dedup();
    collector.names
}

#[test]
fn matrix_row_slice_differentiates_element_by_element() {
    let mut dae = Dae::new();
    state(&mut dae, "T", vec![3, 3]);

    let derivative = symbolic_time_derivative(&slice("T", 2), &dae, &HashMap::new())
        .expect("a constant-subscript row slice is differentiable");

    assert_eq!(
        der_leaf_names(&derivative),
        vec![
            "T[2,1]".to_string(),
            "T[2,2]".to_string(),
            "T[2,3]".to_string()
        ],
        "each slice element must become its own indexed derivative leaf"
    );
    assert!(
        matches!(&derivative, Expression::Array { elements, .. } if elements.len() == 3),
        "the derivative of a 3-element row slice is a 3-element array"
    );
}

#[test]
fn slice_dot_product_keeps_every_operand_dependency() {
    let mut dae = Dae::new();
    state(&mut dae, "T", vec![3, 3]);
    state(&mut dae, "U", vec![3, 3]);

    let dot = Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(slice("T", 1)),
        rhs: Box::new(slice("U", 1)),
        span: test_span(),
    };
    let derivative = symbolic_time_derivative(&dot, &dae, &HashMap::new())
        .expect("a dot product of two row slices is differentiable");

    let leaves = der_leaf_names(&derivative);
    for column in 1..=3 {
        assert!(
            leaves.contains(&format!("T[1,{column}]"))
                && leaves.contains(&format!("U[1,{column}]")),
            "the product rule must retain both operands' component derivatives, got {leaves:?}"
        );
    }
}

#[test]
fn cross_product_follows_the_product_rule() {
    let mut dae = Dae::new();
    state(&mut dae, "a", vec![3]);
    state(&mut dae, "b", vec![3]);

    let der_map = HashMap::from([
        ("a".to_string(), var("a_dot")),
        ("b".to_string(), var("b_dot")),
    ]);
    let derivative = symbolic_time_derivative(
        &builtin(BuiltinFunction::Cross, vec![var("a"), var("b")]),
        &dae,
        &der_map,
    )
    .expect("cross(a, b) is differentiable");

    let Expression::Binary {
        op: OpBinary::Add,
        lhs,
        rhs,
        ..
    } = &derivative
    else {
        panic!("d(cross(a, b)) is a sum of two cross products, got {derivative:?}");
    };
    for term in [lhs.as_ref(), rhs.as_ref()] {
        assert!(
            matches!(
                term,
                Expression::BuiltinCall {
                    function: BuiltinFunction::Cross,
                    args,
                    ..
                } if args.len() == 2
            ),
            "each product-rule term stays a cross product, got {term:?}"
        );
    }
}

#[test]
fn cross_product_with_a_constant_operand_folds_to_zero() {
    let mut dae = Dae::new();
    state(&mut dae, "a", vec![3]);
    let mut constant = Variable::new(VarName::new("k"), test_span());
    constant.source_span = test_span();
    constant.dims = vec![3];
    dae.variables.parameters.insert(VarName::new("k"), constant);

    let derivative = symbolic_time_derivative(
        &builtin(BuiltinFunction::Cross, vec![var("k"), var("k")]),
        &dae,
        &HashMap::new(),
    )
    .expect("cross of two parameters is differentiable");

    assert!(
        zero_fold::is_zero_array(&derivative),
        "cross(k, k) has a structurally zero derivative, got {derivative:?}"
    );
}

#[test]
fn atan2_differentiates_with_the_quotient_rule() {
    let mut dae = Dae::new();
    state(&mut dae, "y", Vec::new());
    state(&mut dae, "x", Vec::new());

    let der_map = HashMap::from([
        ("y".to_string(), var("y_dot")),
        ("x".to_string(), var("x_dot")),
    ]);
    let derivative = symbolic_time_derivative(
        &builtin(BuiltinFunction::Atan2, vec![var("y"), var("x")]),
        &dae,
        &der_map,
    )
    .expect("atan2(y, x) is differentiable");

    let Expression::Binary {
        op: OpBinary::Div,
        lhs,
        rhs,
        ..
    } = &derivative
    else {
        panic!("d(atan2) is a quotient, got {derivative:?}");
    };
    let numerator = format!("{lhs:?}");
    let denominator = format!("{rhs:?}");
    assert!(
        numerator.contains("y_dot") && numerator.contains("x_dot"),
        "the numerator x*dy - y*dx uses both argument derivatives, got {numerator}"
    );
    assert!(
        !denominator.contains("_dot"),
        "the denominator x^2 + y^2 carries no derivative, got {denominator}"
    );
}

/// A connection-equation residual names its function through a generated
/// reference that carries the resolved function but no component reference —
/// the exact shape `zeros(3) = Frames.Orientation.equalityConstraint(R1, R2)`
/// reaches index reduction in. Resolution must not refuse it, or the whole
/// loop-closure row silently fails to differentiate.
#[test]
fn generated_function_reference_without_a_component_ref_resolves() {
    let mut dae = Dae::new();
    state(&mut dae, "q", Vec::new());

    let instance_id = rumoca_core::FunctionInstanceId::new(7_701);
    let mut function = rumoca_core::Function::new("Frames.equalityConstraint", test_span());
    function.def_id = Some(rumoca_core::DefId::new(7_701));
    function.instance_id = Some(instance_id);
    function
        .inputs
        .push(rumoca_core::FunctionParam::new("u", "Real", test_span()));
    function.outputs.push(rumoca_core::FunctionParam::new(
        "residue",
        "Real",
        test_span(),
    ));
    function.body.push(rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference {
            local: false,
            span: test_span(),
            parts: vec![rumoca_core::ComponentRefPart {
                ident: "residue".to_string(),
                span: test_span(),
                subs: Vec::new(),
            }],
            def_id: None,
        },
        value: Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(var("u")),
            rhs: Box::new(var("u")),
            span: test_span(),
        },
        span: test_span(),
    });
    dae.symbols
        .functions
        .insert(VarName::new("Frames.equalityConstraint"), function);

    let call = Expression::FunctionCall {
        name: rumoca_core::Reference::new("Frames.equalityConstraint").with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id,
                base_part_count: 0,
            },
        ),
        args: vec![var("q")],
        is_constructor: false,
        span: test_span(),
    };
    let der_map = HashMap::from([("q".to_string(), var("q_dot"))]);

    let derivative = symbolic_time_derivative(&call, &dae, &der_map)
        .expect("a generated function reference must still resolve to its body");

    assert!(
        format!("{derivative:?}").contains("q_dot"),
        "the inlined body's derivative must depend on the argument derivative, got {derivative:?}"
    );
}

#[test]
fn atan2_of_constants_has_a_zero_derivative() {
    let mut dae = Dae::new();
    for name in ["p", "q"] {
        let mut parameter = Variable::new(VarName::new(name), test_span());
        parameter.source_span = test_span();
        dae.variables
            .parameters
            .insert(VarName::new(name), parameter);
    }

    let derivative = symbolic_time_derivative(
        &builtin(BuiltinFunction::Atan2, vec![var("p"), var("q")]),
        &dae,
        &HashMap::new(),
    )
    .expect("atan2 of two parameters is differentiable");

    assert!(
        der_leaf_names(&derivative).is_empty(),
        "a parameter-only atan2 derivative carries no der() leaf, got {derivative:?}"
    );
}
