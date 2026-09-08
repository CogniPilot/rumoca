//! Function fixtures for direct Flat-to-DAE construction.
//!
//! A collected Flat function exposes exactly one source declaration, which it
//! carries as its exposure identity. These models are written directly rather
//! than resolved from a class tree, so the `63_1xx` band names the function
//! declarations this module writes: one value per declaration, so the two
//! declarations that share a model stay distinct and every fixture that
//! rebuilds one declaration reuses its value.

use rumoca_core::{Reference, ResolvedFunctionReference, TypeId};

use super::super::*;
use super::support::*;

fn assert_ed007_without_borrowed_span(error: &ToDaeError, expected_context: &str) {
    let diagnostic = rumoca_core::PhaseError::to_diagnostic(error);
    assert_eq!(
        diagnostic.code.as_deref(),
        Some("ED007"),
        "unexpected error: {error:?}"
    );
    assert!(
        diagnostic.labels.is_empty(),
        "missing occurrence provenance must not borrow an enclosing source label"
    );
    assert_eq!(error.source_span(), None);
    assert!(matches!(
        error,
        ToDaeError::MissingProvenance { owner } if owner.contains(expected_context)
    ));
}

fn identity_function(
    source: &TestSource,
    input: rumoca_core::FunctionParam,
    output: rumoca_core::FunctionParam,
) -> rumoca_core::Function {
    let assignment_span = source.span("y := u", 0);
    let mut function = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_101),
        source.span("function f", 0),
    );
    function.add_input(input);
    function.add_output(output);
    function.body.push(rumoca_core::Statement::Assignment {
        comp: test_component_reference("y", assignment_span),
        value: Expression::VarRef {
            name: Reference::new("u"),
            subscripts: Vec::new(),
            span: source.span("u", 1),
        },
        span: assignment_span,
    });
    function
}

fn add_function_call(model: &mut flat::Model, source: &TestSource, argument: Expression) {
    let call_span = source.span("f(", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: vec![argument],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
}

fn indexed_test_component_reference(
    name: &str,
    index: i64,
    span: Span,
) -> rumoca_core::ComponentReference {
    indexed_test_component_reference_with_def_id(
        name,
        index,
        rumoca_core::DefId::new(test_instance_id(name).index().max(1)),
        span,
    )
}

fn indexed_test_component_reference_with_def_id(
    name: &str,
    index: i64,
    def_id: rumoca_core::DefId,
    span: Span,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(
        false,
        span,
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: vec![rumoca_core::Subscript::Index { value: index, span }],
            def_id,
        }],
    )
    .expect("test indexed component reference has exact identity")
}

fn assert_function_scalar_types(dae: &dae::Dae, expected: dae::ScalarType) {
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let parameter = function.parameters().next().unwrap();
        assert_eq!(
            view.value_type(parameter.value_type())
                .unwrap()
                .scalar_type(),
            expected
        );
        let output = function
            .values()
            .find(|value| value.role() == dae::FunctionValueRole::Output)
            .unwrap();
        assert_eq!(
            view.value_type(output.value_type()).unwrap().scalar_type(),
            expected
        );
    });
}

fn scalar_array_element_function(
    source: &TestSource,
    indices: impl IntoIterator<Item = i64>,
) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_102),
        source.span("function f", 0),
    );
    let output = real_function_param("y", vec![4], source.span("output Real y[4]", 0)).with_def_id(
        rumoca_core::DefId::new(test_instance_id("y").index().max(1)),
    );
    function.add_output(output);
    for index in indices {
        let text = format!("y[{index}] := {index}.0");
        let assignment = source.span(&text, 0);
        function.body.push(rumoca_core::Statement::Assignment {
            comp: indexed_test_component_reference("y", index, assignment),
            value: Expression::Literal {
                value: Literal::Real(index as f64),
                span: source.span(&format!("{index}.0"), 0),
            },
            span: assignment,
        });
    }
    function
}

fn scalar_array_literal_function(
    source: &TestSource,
    members: &[(&str, Literal)],
) -> rumoca_core::Function {
    let mut function = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_103),
        source.span("function f", 0),
    );
    let output_text = format!("output Real y[{}]", members.len());
    let y_def = rumoca_core::DefId::new(test_instance_id("y").index().max(1));
    function.add_output(
        real_function_param(
            "y",
            vec![members.len() as i64],
            source.span(&output_text, 0),
        )
        .with_def_id(y_def),
    );
    for (ordinal, (literal_text, literal)) in members.iter().enumerate() {
        let index = ordinal as i64 + 1;
        let assignment_text = format!("y[{index}] := {literal_text}");
        let assignment = source.span(&assignment_text, 0);
        function.body.push(rumoca_core::Statement::Assignment {
            comp: indexed_test_component_reference_with_def_id("y", index, y_def, assignment),
            value: Expression::Literal {
                value: literal.clone(),
                span: source.span(literal_text, 0),
            },
            span: assignment,
        });
    }
    function
}

fn add_projected_scalar_array_call(model: &mut flat::Model, source: &TestSource) {
    let instance_id = model.functions[&VarName::new("f")]
        .instance_id
        .expect("Flat assigns the array function an exact instance");
    let call_span = source.span("f()", 0);
    let projection_span = source.span("f()[1]", 0);
    model.add_equation(flat::Equation::new(
        Expression::Index {
            base: Box::new(Expression::FunctionCall {
                name: Reference::new("f").with_resolved_function(ResolvedFunctionReference {
                    instance_id,
                    base_part_count: 1,
                    transitively_non_replaceable: true,
                }),
                args: Vec::new(),
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: call_span,
            }),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: projection_span,
            }],
            span: projection_span,
        },
        projection_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
}

#[test]
fn complete_scalar_array_element_definitions_construct_one_whole_assignment() {
    let source = TestSource::new(
        "function f output Real y[4]; algorithm y[1] := 1.0; y[2] := 2.0; \
         y[3] := 3.0; y[4] := 4.0; end f; f()[1];",
    );
    let mut model = test_model();
    model.add_function(scalar_array_element_function(&source, 1..=4));
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);

    let dae = construct(&model, source.map).expect("complete scalar array constructs");
    dae.dae().inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let output = function
            .values()
            .find(|value| value.name().as_str() == "y")
            .expect("the exact output reaches DAE");
        let output_type = view.value_type(output.value_type()).unwrap();
        assert_eq!(output.role(), dae::FunctionValueRole::Output);
        assert_eq!(output_type.scalar_type(), dae::ScalarType::Real);
        assert_eq!(output_type.dimensions(), &[4]);
        let statements = function.statements().collect::<Vec<_>>();
        let [dae::FunctionStatementView::Assignment { definition }] = statements.as_slice() else {
            panic!(
                "complete canonical element coverage must construct one whole assignment, got {}",
                statements.len()
            )
        };
        assert_eq!(definition.target(), output.id());
        assert_eq!(function.result_values().get(0).unwrap().id(), definition.id());
        assert_eq!(
            view.source_text(definition.provenance()),
            Some("y[1] := 1.0")
        );
        let rhs = view.expression(definition.rhs()).unwrap();
        assert_eq!(rhs.value_type().scalar_type(), dae::ScalarType::Real);
        assert_eq!(rhs.value_type().dimensions(), &[4]);
        let dae::ExpressionOperation::Array(elements) = rhs.operation() else {
            panic!("the sole definition must retain the complete rank-one aggregate")
        };
        assert_eq!(elements.len(), 4);
        for (ordinal, (expected, source_text)) in
            [(1.0, "1.0"), (2.0, "2.0"), (3.0, "3.0"), (4.0, "4.0")]
                .into_iter()
                .enumerate()
        {
            let element = view.expression(elements.get(ordinal).unwrap()).unwrap();
            assert!(
                matches!(element.operation(), dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) if *value == expected),
                "aggregate member {ordinal} must retain its exact literal"
            );
            assert_eq!(view.source_text(element.provenance()), Some(source_text));
        }
    });
}

#[test]
fn noncanonical_scalar_array_element_order_cannot_forge_one_aggregate() {
    let source = TestSource::new(
        "function f output Real y[4]; algorithm y[2] := 2.0; y[1] := 1.0; \
         y[3] := 3.0; y[4] := 4.0; end f; f()[1];",
    );
    let mut model = test_model();
    model.add_function(scalar_array_element_function(&source, [2, 1, 3, 4]));
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);

    let dae = construct(&model, source.map).expect("noncanonical scalar array constructs");
    dae.dae().inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let output = function
            .values()
            .find(|value| value.name().as_str() == "y")
            .expect("the exact output reaches DAE");
        let statements = function.statements().collect::<Vec<_>>();
        assert_eq!(
            statements.len(),
            5,
            "the checked seed plus every reordered element write must remain explicit"
        );
        let dae::FunctionStatementView::Assignment {
            definition: seed_definition,
        } = &statements[0]
        else {
            panic!("noncanonical element coverage must retain its checked seed")
        };
        assert_eq!(seed_definition.target(), output.id());
        assert_eq!(
            view.source_text(seed_definition.provenance()),
            Some("function f")
        );
        for (ordinal, (index, value, source_text)) in [
            (2, 2.0, "y[2] := 2.0"),
            (1, 1.0, "y[1] := 1.0"),
            (3, 3.0, "y[3] := 3.0"),
            (4, 4.0, "y[4] := 4.0"),
        ]
        .into_iter()
        .enumerate()
        {
            let dae::FunctionStatementView::Assignment { definition } = &statements[ordinal + 1]
            else {
                panic!("reordered element {ordinal} must remain an assignment")
            };
            assert_eq!(definition.target(), output.id());
            assert_eq!(view.source_text(definition.provenance()), Some(source_text));
            let rhs = view.expression(definition.rhs()).unwrap();
            let dae::ExpressionOperation::ArrayUpdate {
                value: updated,
                subscripts,
                ..
            } = rhs.operation()
            else {
                panic!("reordered element {ordinal} must remain one array update")
            };
            let updated = view.expression(updated).unwrap();
            assert!(
                matches!(updated.operation(), dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(found)) if *found == value)
            );
            let mut subscripts = subscripts.iter();
            let Some(dae::SubscriptView::Index { expression, .. }) = subscripts.next() else {
                panic!("reordered element {ordinal} must retain one exact index")
            };
            assert!(subscripts.next().is_none());
            let expression = view.expression(expression).unwrap();
            assert!(
                matches!(expression.operation(), dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(found)) if *found == index)
            );
        }
        let final_definition = match statements.last().unwrap() {
            dae::FunctionStatementView::Assignment { definition } => definition,
            _ => unreachable!("the four exact statements are assignments"),
        };
        assert_eq!(
            function.result_values().get(0).unwrap().id(),
            final_definition.id(),
            "the last source update remains the returned reaching definition"
        );
    });
}

#[test]
fn foreign_scalar_array_target_identity_is_rejected_before_assembly() {
    let source = TestSource::new(
        "function f output Real y[4]; algorithm y[1] := 1.0; y[2] := 2.0; \
         y[3] := 3.0; y[4] := 4.0; end f; f()[1];",
    );
    let expected = rumoca_core::DefId::new(test_instance_id("y").index().max(1));
    let foreign = rumoca_core::DefId::new(test_instance_id("foreign-y").index().max(1));
    assert_ne!(
        expected, foreign,
        "the mutation must change semantic identity"
    );

    let mut function = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_104),
        source.span("function f", 0),
    );
    function.add_output(
        real_function_param("y", vec![4], source.span("output Real y[4]", 0)).with_def_id(expected),
    );
    let mut foreign_span = Span::DUMMY;
    for index in 1..=4 {
        let text = format!("y[{index}] := {index}.0");
        let assignment = source.span(&text, 0);
        if index == 2 {
            foreign_span = assignment;
        }
        function.body.push(rumoca_core::Statement::Assignment {
            comp: indexed_test_component_reference_with_def_id(
                "y",
                index,
                if index == 2 { foreign } else { expected },
                assignment,
            ),
            value: Expression::Literal {
                value: Literal::Real(index as f64),
                span: source.span(&format!("{index}.0"), 0),
            },
            span: assignment,
        });
    }
    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);

    let error = construct(&model, source.map)
        .expect_err("a foreign target identity must not reach array assembly");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "function assignment target" && span == foreign_span
    ));
}

#[test]
fn same_spelled_function_value_cannot_redirect_scalar_array_assembly() {
    let source = TestSource::new(
        "function f output Real y[4]; protected Real y[2]; algorithm \
         y[1] := 1.0; y[2] := 2.0; y[3] := 3.0; y[4] := 4.0; end f; f()[1];",
    );
    let output_def = rumoca_core::DefId::new(test_instance_id("output-y").index().max(1));
    let local_def = rumoca_core::DefId::new(test_instance_id("local-y").index().max(1));
    assert_ne!(
        output_def, local_def,
        "the mutation needs distinct identities"
    );

    let mut function = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_105),
        source.span("function f", 0),
    );
    function.add_output(
        real_function_param("y", vec![4], source.span("output Real y[4]", 0))
            .with_def_id(output_def),
    );
    function.add_local(
        real_function_param("y", vec![2], source.span("protected Real y[2]", 0))
            .with_def_id(local_def),
    );
    for index in 1..=4 {
        let assignment_text = format!("y[{index}] := {index}.0");
        let assignment = source.span(&assignment_text, 0);
        function.body.push(rumoca_core::Statement::Assignment {
            comp: indexed_test_component_reference_with_def_id("y", index, output_def, assignment),
            value: Expression::Literal {
                value: Literal::Real(index as f64),
                span: source.span(&format!("{index}.0"), 0),
            },
            span: assignment,
        });
    }
    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);

    let local_span = source.span("protected Real y[2]", 0);
    let error = construct(&model, source.map)
        .expect_err("a duplicate spelling must fail before it can redirect the output plan");
    assert!(matches!(
        error,
        ToDaeError::Construction {
            source: dae::DaeConstructionError::DuplicateKey {
                kind: "function value",
                ref key,
                span,
            },
            ..
        } if key == "y" && span == local_span
    ));
}

fn sequential_callable_elements_model() -> (flat::Model, SourceMap) {
    let source = TestSource::new(
        "function next output Real v; external \"C\" v = next_value(); end next; \
         function f output Real y[2]; algorithm y[1] := next(); y[2] := next(); \
         end f; f()[1];",
    );
    let mut next = rumoca_core::Function::new(
        "next",
        rumoca_core::DefId::new(63_106),
        source.span("function next", 0),
    );
    next.pure = true;
    next.purity_declared = false;
    next.add_output(real_function_param(
        "v",
        Vec::new(),
        source.span("output Real v", 0),
    ));
    next.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("next_value".to_string()),
        output_name: Some("v".to_string()),
        args: Vec::new(),
        annotations: Vec::new(),
    });

    let mut model = test_model();
    model.add_function(next);
    let next_instance = model.functions[&VarName::new("next")]
        .instance_id
        .expect("Flat assigns the external function an exact instance");
    let next_reference = || {
        Reference::new("next").with_resolved_function(ResolvedFunctionReference {
            instance_id: next_instance,
            base_part_count: 1,
            transitively_non_replaceable: true,
        })
    };

    let y_def = rumoca_core::DefId::new(test_instance_id("y").index().max(1));
    let mut caller = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_107),
        source.span("function f", 0),
    );
    caller.add_output(
        real_function_param("y", vec![2], source.span("output Real y[2]", 0)).with_def_id(y_def),
    );
    for (index, occurrence) in [(1, 0), (2, 1)] {
        let assignment = source.span(&format!("y[{index}] := next()"), 0);
        caller.body.push(rumoca_core::Statement::Assignment {
            comp: indexed_test_component_reference_with_def_id("y", index, y_def, assignment),
            value: Expression::FunctionCall {
                name: next_reference(),
                args: Vec::new(),
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: source.span("next()", occurrence),
            },
            span: assignment,
        });
    }
    model.add_function(caller);
    let caller_instance = model.functions[&VarName::new("f")]
        .instance_id
        .expect("Flat assigns the caller an exact instance");
    model.is_partial = true;
    let call_span = source.span("f()", 0);
    let projection_span = source.span("f()[1]", 0);
    model.initial_equations.push(flat::Equation::new(
        Expression::Index {
            base: Box::new(Expression::FunctionCall {
                name: Reference::new("f").with_resolved_function(ResolvedFunctionReference {
                    instance_id: caller_instance,
                    base_part_count: 1,
                    transitively_non_replaceable: true,
                }),
                args: Vec::new(),
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: call_span,
            }),
            subscripts: vec![rumoca_core::Subscript::Index {
                value: 1,
                span: projection_span,
            }],
            span: projection_span,
        },
        projection_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    (model, source.map)
}

#[test]
fn callable_scalar_elements_retain_sequential_function_statements() {
    let (model, source_map) = sequential_callable_elements_model();
    let dae = construct(&model, source_map)
        .expect("a deprecated bare external call remains callable in an initial equation");
    dae.dae().inspect(|view| {
        let caller = (0..)
            .map_while(|index| view.function_id(index))
            .filter_map(|id| view.function(id))
            .find(|function| function.name().as_str() == "f")
            .expect("caller reaches the DAE");
        assert_ne!(
            caller.statements().count(),
            1,
            "callable elements must retain their exact source statement order"
        );
    });
}

#[test]
fn incompatible_scalar_array_member_reports_its_exact_expression() {
    let source = TestSource::new(
        "function f output Real y[4]; algorithm y[1] := 1.0; y[2] := 2.0; \
         y[3] := 3.0; y[4] := false; end f; f()[1];",
    );
    let y_def = rumoca_core::DefId::new(test_instance_id("y").index().max(1));
    let mut function = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_108),
        source.span("function f", 0),
    );
    function.add_output(
        real_function_param("y", vec![4], source.span("output Real y[4]", 0)).with_def_id(y_def),
    );
    for index in 1..=4 {
        let assignment_text = if index == 4 {
            "y[4] := false".to_string()
        } else {
            format!("y[{index}] := {index}.0")
        };
        let assignment = source.span(&assignment_text, 0);
        let value = if index == 4 {
            Expression::Literal {
                value: Literal::Boolean(false),
                span: source.span("false", 0),
            }
        } else {
            Expression::Literal {
                value: Literal::Real(index as f64),
                span: source.span(&format!("{index}.0"), 0),
            }
        };
        function.body.push(rumoca_core::Statement::Assignment {
            comp: indexed_test_component_reference_with_def_id("y", index, y_def, assignment),
            value,
            span: assignment,
        });
    }

    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);
    let bad_expression = source.span("false", 0);
    let first_assignment = source.span("y[1] := 1.0", 0);

    let error = construct(&model, source.map).expect_err("Boolean cannot define Real y[4]");
    assert_ne!(
        error.source_span(),
        Some(first_assignment),
        "the generated aggregate owner must not blame the first valid member"
    );
    assert_eq!(
        error.source_span(),
        Some(bad_expression),
        "the invalid member must retain its constructor-bound expression owner"
    );
}

#[test]
fn invalid_first_scalar_array_member_is_reported_before_later_valid_members() {
    let source = TestSource::new(
        "function f output Real y[2]; algorithm y[1] := false; y[2] := 2.0; end f; f()[1];",
    );
    let mut model = test_model();
    model.add_function(scalar_array_literal_function(
        &source,
        &[
            ("false", Literal::Boolean(false)),
            ("2.0", Literal::Real(2.0)),
        ],
    ));
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);
    let bad_member = source.span("false", 0);

    let error = construct(&model, source.map).expect_err("Boolean cannot define Real y[1]");
    assert_eq!(error.source_span(), Some(bad_member));
}

#[test]
fn homogeneous_wrong_scalar_array_reports_its_first_invalid_member() {
    let source = TestSource::new(
        "function f output Real y[2]; algorithm y[1] := false; y[2] := true; end f; f()[1];",
    );
    let mut model = test_model();
    model.add_function(scalar_array_literal_function(
        &source,
        &[
            ("false", Literal::Boolean(false)),
            ("true", Literal::Boolean(true)),
        ],
    ));
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);
    let bad_member = source.span("false", 0);

    let error = construct(&model, source.map).expect_err("Boolean array cannot define Real y");
    assert_eq!(error.source_span(), Some(bad_member));
}

#[test]
fn integer_scalar_array_members_widen_only_at_the_real_target_assignment() {
    let source = TestSource::new(
        "function f output Real y[2]; algorithm y[1] := 1; y[2] := 2; end f; f()[1];",
    );
    let mut model = test_model();
    model.add_function(scalar_array_literal_function(
        &source,
        &[("1", Literal::Integer(1)), ("2", Literal::Integer(2))],
    ));
    model.is_partial = true;
    add_projected_scalar_array_call(&mut model, &source);

    let dae = construct(&model, source.map).expect("Integer aggregate widens at Real assignment");
    dae.dae().inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let output = function
            .values()
            .find(|value| value.role() == dae::FunctionValueRole::Output)
            .unwrap();
        assert_eq!(
            view.value_type(output.value_type()).unwrap().scalar_type(),
            dae::ScalarType::Real
        );
        let definition = match function.statements().next().unwrap() {
            dae::FunctionStatementView::Assignment { definition } => definition,
            _ => panic!("the aggregate reaches one assignment"),
        };
        let rhs = view.expression(definition.rhs()).unwrap();
        assert_eq!(rhs.value_type().scalar_type(), dae::ScalarType::Integer);
        assert_eq!(rhs.value_type().dimensions(), &[2]);
    });
}

#[test]
fn real_alias_function_values_use_canonical_identity_not_display_name() {
    let source = TestSource::new(
        "function f input Voltage u; output Voltage y; algorithm y := u; end f; f(1.0);",
    );
    let alias = TypeId::new(901);
    let input = real_alias_function_param(
        "u",
        "Voltage",
        alias,
        Vec::new(),
        source.span("input Voltage u", 0),
    );
    let output = real_alias_function_param(
        "y",
        "Voltage",
        alias,
        Vec::new(),
        source.span("output Voltage y", 0),
    );
    assert_eq!(input.type_name, "Voltage");
    assert_ne!(
        input.effective_type.nominal_type(),
        input.effective_type.canonical_type()
    );
    let mut model = test_model();
    model.add_function(identity_function(&source, input, output));
    model.is_partial = true;
    add_function_call(
        &mut model,
        &source,
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
    );

    let dae = construct(&model, source.map).expect("canonical Real alias constructs");
    assert_function_scalar_types(dae.dae(), dae::ScalarType::Real);
}

#[test]
fn enumeration_function_values_use_registered_canonical_identity() {
    let source = TestSource::new(
        "input Color c; output Color d; function f input Color u; output Color y; algorithm y := u; end f; d = f(c);",
    );
    let enumeration = TypeId::new(902);
    let input = enumeration_function_param(
        "u",
        "Color",
        enumeration,
        Vec::new(),
        source.span("input Color u", 0),
    );
    let output = enumeration_function_param(
        "y",
        "Color",
        enumeration,
        Vec::new(),
        source.span("output Color y", 0),
    );
    let mut model = test_model();
    register_test_enumeration_type(&mut model, enumeration);
    let declaration_span = source.span("input Color c", 0);
    let mut variable = flat::Variable::empty_with_span(declaration_span);
    variable.name = VarName::new("c");
    variable.instance_id = test_instance_id("c");
    variable.component_ref = Some(test_component_reference("c", declaration_span));
    variable.type_id = enumeration;
    variable.variability = Variability::Discrete(Default::default());
    variable.causality = Causality::Input(Default::default());
    variable.is_discrete_type = true;
    variable.is_primitive = true;
    model.effective_types.insert(
        enumeration,
        rumoca_core::EffectiveType::new(enumeration, enumeration, Vec::new()).unwrap(),
    );
    model.enumeration_types.insert(enumeration);
    model.add_variable(variable.name.clone(), variable);
    let output_span = source.span("output Color d", 0);
    let mut output_variable = flat::Variable::empty_with_span(output_span);
    output_variable.name = VarName::new("d");
    output_variable.instance_id = test_instance_id("d");
    output_variable.component_ref = Some(test_component_reference("d", output_span));
    output_variable.type_id = enumeration;
    output_variable.variability = Variability::Discrete(Default::default());
    output_variable.causality = Causality::Output(Default::default());
    output_variable.is_discrete_type = true;
    output_variable.is_primitive = true;
    model.add_variable(output_variable.name.clone(), output_variable);
    model.add_function(identity_function(&source, input, output));
    model.is_partial = true;
    let equation_span = source.span("d = f(c)", 0);
    let call_span = source.span("f(c)", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(Expression::VarRef {
                name: test_reference("d"),
                subscripts: Vec::new(),
                span: source.span("d", 1),
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: Reference::new("f"),
                args: vec![Expression::VarRef {
                    name: test_reference("c"),
                    subscripts: Vec::new(),
                    span: source.span("c", 1),
                }],
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: call_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let dae = construct(&model, source.map).expect("registered enumeration constructs");
    assert_function_scalar_types(dae.dae(), dae::ScalarType::Enumeration);
}

#[test]
fn user_class_named_real_is_not_a_predefined_scalar() {
    let source =
        TestSource::new("function f input Real u; output Real y; algorithm y := u; end f; f(1.0);");
    let user_real = TypeId::new(903);
    let input_span = source.span("input Real u", 0);
    let input = function_param("u", "Real", user_real, user_real, Vec::new(), input_span);
    let output = function_param(
        "y",
        "Real",
        user_real,
        user_real,
        Vec::new(),
        source.span("output Real y", 0),
    );
    let mut model = test_model();
    model.add_function(identity_function(&source, input, output));
    model.is_partial = true;
    add_function_call(
        &mut model,
        &source,
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1.0", 0),
        },
    );

    let error = construct(&model, source.map).expect_err("display spelling cannot mint Real");
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "function value type"
            && detail.contains("unsupported type `Real`")
            && span == input_span
    ));
}

#[test]
fn executable_external_object_constructor_reaches_lifecycle_boundary() {
    let source = TestSource::new(
        "function constructor\n  input Real seed;\n  output Handle handle;\n  external \"C\" handle = make_handle(seed);\nend constructor;\nHandle(1.0);",
    );
    let function_span = source.span(
        "constructor\n  input Real seed;\n  output Handle handle;\n  external \"C\" handle = make_handle(seed);\nend constructor",
        0,
    );
    let input_span = source.span("input Real seed", 0);
    let output_span = source.span("output Handle handle", 0);
    let external_arg_span = source.span("seed", 1);
    let call_span = source.span("Handle(1.0)", 0);
    let literal_span = source.span("1.0", 0);

    let mut constructor =
        rumoca_core::Function::new("Handle", rumoca_core::DefId::new(63_109), function_span);
    constructor.add_input(real_function_param("seed", Vec::new(), input_span));
    constructor.add_output(function_param(
        "handle",
        "Handle",
        TypeId::new(900),
        TypeId::new(900),
        Vec::new(),
        output_span,
    ));
    constructor.external = Some(rumoca_core::ExternalFunction {
        language: "C".to_string(),
        function_name: Some("make_handle".to_string()),
        output_name: Some("handle".to_string()),
        args: vec![Expression::VarRef {
            name: Reference::new("seed"),
            subscripts: Vec::new(),
            span: external_arg_span,
        }],
        annotations: Vec::new(),
    });

    let mut model = test_model();
    model.add_function(constructor);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("Handle"),
            args: vec![Expression::Literal {
                value: Literal::Real(1.0),
                span: literal_span,
            }],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    // MLS §12.9 external interfaces are now constructible, so the rejection
    // moves to the exact boundary the ExternalObject actually lacks: `Handle`
    // has no checked DAE lifecycle value type. The declaration span is the
    // output that names it, not the enclosing function.
    let error = construct(&model, source.map).unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            detail,
            span,
        } if feature == "function value type"
            && detail == "`Handle.handle` has unsupported type `Handle`"
            && span == output_span
    ));
}

fn nested_assert_function_model(source: &TestSource, assertion_span: Span) -> flat::Model {
    let function_span = source.span("function f", 0);
    let output_span = source.span("output Real y", 0);
    let conditional_span = source.span("if true then assert(true, \"bad\"); end if", 0);
    let mut function =
        rumoca_core::Function::new("f", rumoca_core::DefId::new(63_110), function_span);
    function.add_output(real_function_param("y", Vec::new(), output_span));
    function.body = vec![rumoca_core::Statement::If {
        cond_blocks: vec![rumoca_core::StatementBlock {
            cond: Expression::Literal {
                value: Literal::Boolean(true),
                span: source.span("true", 0),
            },
            stmts: vec![rumoca_core::Statement::Assert {
                condition: Expression::Literal {
                    value: Literal::Boolean(true),
                    span: source.span("true", 1),
                },
                message: Box::new(Expression::Literal {
                    value: Literal::String("bad".to_string()),
                    span: source.span("\"bad\"", 0),
                }),
                level: None,
                span: assertion_span,
            }],
        }],
        else_block: None,
        span: conditional_span,
    }];

    let mut model = test_model();
    model.add_function(function);
    model.is_partial = true;
    let call_span = source.span("f()", 0);
    model.add_equation(flat::Equation::new(
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: Vec::new(),
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: call_span,
        },
        call_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
    model
}

fn integer_assertion_function(source: &TestSource) -> rumoca_core::Function {
    let assertion_span = source.span("assert(i >= 1, \"i must be positive\")", 0);
    let assignment_span = source.span("y := i", 0);
    let mut function = rumoca_core::Function::new(
        "positive",
        rumoca_core::DefId::new(63_111),
        source.span("function positive", 0),
    );
    function.add_input(integer_function_param(
        "i",
        Vec::new(),
        source.span("input Integer i", 0),
    ));
    function.add_output(integer_function_param(
        "y",
        Vec::new(),
        source.span("output Integer y", 0),
    ));
    function.body = vec![
        // Algorithm assert syntax is the predefined zero-output call shape
        // Flat production currently retains.
        rumoca_core::Statement::FunctionCall {
            comp: rumoca_core::Reference::from_component_reference(test_component_reference(
                "assert",
                assertion_span,
            )),
            args: vec![
                Expression::Binary {
                    op: OpBinary::Ge,
                    lhs: Box::new(Expression::VarRef {
                        name: Reference::new("i"),
                        subscripts: Vec::new(),
                        span: source.span("i", 2),
                    }),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: source.span("1", 0),
                    }),
                    span: source.span("i >= 1", 0),
                },
                Expression::Literal {
                    value: Literal::String("i must be positive".to_string()),
                    span: source.span("\"i must be positive\"", 0),
                },
            ],
            outputs: Vec::new(),
            span: assertion_span,
        },
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("y", assignment_span),
            value: Expression::VarRef {
                name: Reference::new("i"),
                subscripts: Vec::new(),
                span: source.span("i", 3),
            },
            span: assignment_span,
        },
    ];
    function
}

fn real_assertion_function(source: &TestSource) -> rumoca_core::Function {
    let assertion_span = source.span("assert(r >= 0.0, \"r must be positive\")", 0);
    let assignment_span = source.span("y := r", 0);
    let mut function = rumoca_core::Function::new(
        "positive",
        rumoca_core::DefId::new(63_112),
        source.span("function positive", 0),
    );
    function.add_input(real_function_param(
        "r",
        Vec::new(),
        source.span("input Real r", 0),
    ));
    function.add_output(real_function_param(
        "y",
        Vec::new(),
        source.span("output Real y", 0),
    ));
    function.body = vec![
        rumoca_core::Statement::FunctionCall {
            comp: rumoca_core::Reference::from_component_reference(test_component_reference(
                "assert",
                assertion_span,
            )),
            args: vec![
                Expression::Binary {
                    op: OpBinary::Ge,
                    lhs: Box::new(Expression::VarRef {
                        name: Reference::new("r"),
                        subscripts: Vec::new(),
                        span: source.span("r", 2),
                    }),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Real(0.0),
                        span: source.span("0.0", 0),
                    }),
                    span: source.span("r >= 0.0", 0),
                },
                Expression::Literal {
                    value: Literal::String("r must be positive".to_string()),
                    span: source.span("\"r must be positive\"", 0),
                },
            ],
            outputs: Vec::new(),
            span: assertion_span,
        },
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("y", assignment_span),
            value: Expression::VarRef {
                name: Reference::new("r"),
                subscripts: Vec::new(),
                span: source.span("r", 3),
            },
            span: assignment_span,
        },
    ];
    function
}

fn add_integer_assertion_call(model: &mut flat::Model, source: &TestSource, argument: Expression) {
    let call_span = source.span("positive(", 0);
    let equation_span = source.span("1.0 * positive(", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: Reference::new("positive"),
                args: vec![argument],
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: call_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));
}

#[test]
fn assertion_only_input_retains_its_call_scoped_action() {
    let source = TestSource::new(
        "function positive input Integer i; output Integer y; algorithm assert(i >= 1, \"i must be positive\"); y := i; end positive; 1.0 * positive(3);",
    );
    let mut model = test_model();
    model.add_function(integer_assertion_function(&source));
    model.is_partial = true;
    add_integer_assertion_call(
        &mut model,
        &source,
        Expression::Literal {
            value: Literal::Integer(3),
            span: source.span("3", 0),
        },
    );

    let dae = construct(&model, source.map).expect("the exact input proves the assertion true");
    dae.dae().inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        assert_eq!(statements.len(), 2);
        assert!(matches!(
            statements[0],
            dae::FunctionStatementView::Assertion { .. }
        ));
        assert!(matches!(
            statements[1],
            dae::FunctionStatementView::Assignment { .. }
        ));
    });
}

#[test]
fn proven_false_function_assertion_retains_its_call_scoped_action() {
    let source = TestSource::new(
        "function positive input Integer i; output Integer y; algorithm assert(i >= 1, \"i must be positive\"); y := i; end positive; 1.0 * positive(0);",
    );
    let assertion_span = source.span("assert(i >= 1, \"i must be positive\")", 0);
    let mut model = test_model();
    model.add_function(integer_assertion_function(&source));
    model.is_partial = true;
    add_integer_assertion_call(
        &mut model,
        &source,
        Expression::Literal {
            value: Literal::Integer(0),
            span: source.span("0", 0),
        },
    );

    let dae = construct(&model, source.map).expect("a false assertion has a runtime owner");
    dae.dae().inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        assert_eq!(statements.len(), 2);
        assert!(matches!(
            statements[0].clone(),
            dae::FunctionStatementView::Assertion { provenance, .. }
                if provenance.span() == assertion_span
        ));
    });
}

#[test]
fn unsettled_function_assertion_retains_its_call_scoped_action() {
    let source = TestSource::new(
        "function positive input Real r; output Real y; algorithm assert(r >= 0.0, \"r must be positive\"); y := r; end positive; 1.0 * positive(2.0);",
    );
    let assertion_span = source.span("assert(r >= 0.0, \"r must be positive\")", 0);
    let mut model = test_model();
    model.add_function(real_assertion_function(&source));
    model.is_partial = true;
    add_integer_assertion_call(
        &mut model,
        &source,
        Expression::Literal {
            value: Literal::Real(2.0),
            span: source.span("2.0", 0),
        },
    );

    let dae = construct(&model, source.map).expect("an unsettled assertion has a runtime owner");
    dae.dae().inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        assert_eq!(statements.len(), 2);
        assert!(matches!(
            statements[0].clone(),
            dae::FunctionStatementView::Assertion { provenance, .. }
                if provenance.span() == assertion_span
        ));
    });
}

#[test]
fn declared_function_named_assert_is_not_predefined_assertion_elision() {
    let source = TestSource::new(
        "function assert output Integer y; algorithm y := 1; end assert; function f output Integer y; algorithm assert(); y := 1; end f; 1.0 * f();",
    );
    let call_statement_span = source.span("assert()", 0);
    let mut user_assert = rumoca_core::Function::new(
        "assert",
        rumoca_core::DefId::new(63_113),
        source.span("function assert", 0),
    );
    user_assert.add_output(integer_function_param(
        "y",
        Vec::new(),
        source.span("output Integer y", 0),
    ));
    user_assert.body.push(rumoca_core::Statement::Assignment {
        comp: test_component_reference("y", source.span("y := 1", 0)),
        value: Expression::Literal {
            value: Literal::Integer(1),
            span: source.span("1", 0),
        },
        span: source.span("y := 1", 0),
    });
    let mut model = test_model();
    model.add_function(user_assert);
    let assert_instance = model.functions[&VarName::new("assert")]
        .instance_id
        .expect("Flat assigns the declared assert function an exact instance");
    let mut caller = rumoca_core::Function::new(
        "f",
        rumoca_core::DefId::new(63_114),
        source.span("function f", 0),
    );
    caller.add_output(integer_function_param(
        "y",
        Vec::new(),
        source.span("output Integer y", 1),
    ));
    caller.body = vec![
        rumoca_core::Statement::FunctionCall {
            comp: rumoca_core::Reference::from_component_reference(test_component_reference(
                "assert",
                call_statement_span,
            ))
            .with_resolved_function(ResolvedFunctionReference {
                instance_id: assert_instance,
                base_part_count: 1,
                transitively_non_replaceable: true,
            }),
            args: Vec::new(),
            outputs: Vec::new(),
            span: call_statement_span,
        },
        rumoca_core::Statement::Assignment {
            comp: test_component_reference("y", source.span("y := 1", 1)),
            value: Expression::Literal {
                value: Literal::Integer(1),
                span: source.span("1", 1),
            },
            span: source.span("y := 1", 1),
        },
    ];
    model.add_function(caller);
    model.is_partial = true;
    let call_span = source.span("f()", 0);
    let equation_span = source.span("1.0 * f()", 0);
    model.add_equation(flat::Equation::new(
        Expression::Binary {
            op: OpBinary::Mul,
            lhs: Box::new(Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1.0", 0),
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: Reference::new("f"),
                args: Vec::new(),
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: call_span,
            }),
            span: equation_span,
        },
        equation_span,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    let error = construct(&model, source.map)
        .expect_err("a declared function named assert remains an ordinary call");
    assert!(
        matches!(
        &error,
        ToDaeError::UnsupportedFlatSemantics { feature, detail, span }
            if feature == "function call statement"
                && detail.contains("without reading a result")
                && *span == call_statement_span
        ),
        "{error:?}"
    );
}

mod diagnostic_cases;
