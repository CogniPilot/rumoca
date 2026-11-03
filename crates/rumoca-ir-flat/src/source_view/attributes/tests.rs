use super::*;
use crate::Variable;
use rumoca_core::{EffectiveType, Literal, Reference, SourceId, TypeId, VarName};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("attributes.mo"), 2, 11)
}

fn literal(value: f64) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span: span(),
    }
}

fn reference(name: &str, instance: Option<InstanceId>) -> Expression {
    let mut name = Reference::new(name);
    if let Some(instance) = instance {
        name = name.with_instance_id(instance);
    }
    Expression::VarRef {
        name,
        subscripts: vec![],
        span: span(),
    }
}

fn model(dimensions: Vec<i64>, source: Expression, reference_dimensions: Vec<i64>) -> Model {
    let mut model = Model::default();
    for (index, (name, dimensions, start)) in [
        ("x", dimensions, Some(source)),
        ("p", reference_dimensions, None),
    ]
    .into_iter()
    .enumerate()
    {
        let type_id = TypeId::new(index as u32);
        model.effective_types.insert(
            type_id,
            EffectiveType::new(type_id, type_id, dimensions.clone()).unwrap(),
        );
        model.variables.insert(
            VarName::new(name),
            Variable {
                name: VarName::new(name),
                instance_id: InstanceId::new(index as u32 + 1),
                type_id,
                dims: dimensions,
                start,
                ..Variable::empty_with_span(span())
            },
        );
    }
    model
}

#[test]
fn source_attributes_scalar_values_issue_each_only_on_array_occurrences() {
    for dimensions in [vec![], vec![0], vec![2_000_000]] {
        let model = model(dimensions.clone(), literal(-0.0), vec![]);
        let catalog = model.source_attributes().unwrap();
        let values = catalog.variable(InstanceId::new(1)).unwrap();
        assert_eq!(values.len(), 1);
        assert_eq!(values[0].kind(), SourceAttributeKind::Start);
        assert_eq!(values[0].each(), !dimensions.is_empty());
        assert!(values[0].dimensions().is_empty());
        assert!(std::ptr::eq(
            values[0].expression(),
            model.variables[&VarName::new("x")].start.as_ref().unwrap()
        ));
        let Expression::Literal {
            value: Literal::Real(value),
            ..
        } = values[0].expression()
        else {
            panic!("original literal")
        };
        assert_eq!(value.to_bits(), (-0.0f64).to_bits());
    }
}

#[test]
fn source_attributes_reference_shape_comes_from_exact_finalized_occurrence() {
    for dimensions in [vec![], vec![0], vec![4], vec![2_000_000]] {
        let model = model(
            vec![2_000_000],
            reference("p", Some(InstanceId::new(2))),
            dimensions.clone(),
        );
        let catalog = model.source_attributes().unwrap();
        let values = catalog.variable(InstanceId::new(1));
        if dimensions.is_empty() || dimensions == [2_000_000] {
            let values = values.unwrap();
            assert_eq!(values[0].dimensions(), dimensions);
            assert_eq!(values[0].each(), dimensions.is_empty());
        } else {
            assert!(matches!(
                values,
                Err(SourceAttributeError::ShapeMismatch { .. })
            ));
        }
    }
}

#[test]
fn source_attributes_rendered_name_alone_or_mismatched_identity_cannot_issue_shape() {
    for expression in [
        reference("p", None),
        reference("p", Some(InstanceId::new(1))),
        reference("other", Some(InstanceId::new(2))),
    ] {
        let model = model(vec![4], expression, vec![4]);
        assert!(matches!(
            model
                .source_attributes()
                .unwrap()
                .variable(InstanceId::new(1)),
            Err(SourceAttributeError::UnknownShape { .. })
        ));
    }
}

#[test]
fn source_attributes_unknown_call_refuses_instead_of_guessing_scalar_or_partial_output() {
    let call = Expression::FunctionCall {
        name: Reference::new("opaque"),
        args: vec![],
        is_constructor: false,
        span: span(),
    };
    let mut model = model(vec![4], literal(1.0), vec![]);
    model.variables.get_mut(&VarName::new("x")).unwrap().min = Some(call);
    assert!(
        matches!(model.source_attributes().unwrap().variable(InstanceId::new(1)), Err(SourceAttributeError::UnknownShape { attribute: SourceAttributeKind::Min, span: at, .. }) if at == span())
    );
}

#[test]
fn source_attributes_catalog_rejects_stale_effective_shape_and_duplicate_occurrence() {
    let mut model = model(vec![4], literal(1.0), vec![]);
    model.variables.get_mut(&VarName::new("x")).unwrap().dims = vec![5];
    assert!(model.source_attributes().is_err());
    model.variables.get_mut(&VarName::new("x")).unwrap().dims = vec![4];
    model
        .variables
        .get_mut(&VarName::new("p"))
        .unwrap()
        .instance_id = InstanceId::new(1);
    assert!(model.source_attributes().is_err());
}

#[test]
fn source_attributes_original_order_values_and_wire_are_unchanged() {
    let mut model = model(vec![4], literal(f64::NAN), vec![]);
    let variable = model.variables.get_mut(&VarName::new("x")).unwrap();
    variable.min = Some(literal(-1.0));
    variable.max = Some(literal(1.0));
    variable.nominal = Some(literal(0.5));
    let before = serde_json::to_value(&model).unwrap();
    let catalog = model.source_attributes().unwrap();
    let values = catalog.variable(InstanceId::new(1)).unwrap();
    assert_eq!(
        values
            .iter()
            .map(SourceAttributeValue::kind)
            .collect::<Vec<_>>(),
        [
            SourceAttributeKind::Start,
            SourceAttributeKind::Min,
            SourceAttributeKind::Max,
            SourceAttributeKind::Nominal
        ]
    );
    assert!(matches!(
        catalog.variable(InstanceId::new(99)),
        Err(SourceAttributeError::MissingVariable(_))
    ));
    assert_eq!(serde_json::to_value(&model).unwrap(), before);
}

#[test]
fn source_attributes_array_construction_reuses_canonical_shape_algebra() {
    use rumoca_core::ArrayConstructor;
    let vector = |values: &[f64]| Expression::Array {
        elements: values.iter().map(|value| literal(*value)).collect(),
        kind: ArrayConstructor::Array,
        span: span(),
    };
    for (expression, dimensions) in [
        (vector(&[]), vec![0]),
        (vector(&[1.0, 2.0]), vec![2]),
        (
            Expression::Array {
                elements: vec![vector(&[1.0, 2.0]), vector(&[3.0, 4.0])],
                kind: ArrayConstructor::Array,
                span: span(),
            },
            vec![2, 2],
        ),
        (
            Expression::Array {
                elements: vec![vector(&[1.0, 2.0]), vector(&[3.0, 4.0])],
                kind: ArrayConstructor::Horizontal,
                span: span(),
            },
            vec![2, 2],
        ),
    ] {
        let model = model(dimensions.clone(), expression, vec![]);
        let catalog = model.source_attributes().unwrap();
        let attributes = catalog.variable(InstanceId::new(1)).unwrap();
        assert_eq!(attributes[0].dimensions(), dimensions);
        assert!(!attributes[0].each());
    }
    let malformed = Expression::Array {
        elements: vec![vector(&[1.0]), vector(&[1.0, 2.0])],
        kind: ArrayConstructor::Array,
        span: span(),
    };
    let model = model(vec![2, 2], malformed, vec![]);
    assert!(matches!(
        model
            .source_attributes()
            .unwrap()
            .variable(InstanceId::new(1)),
        Err(SourceAttributeError::UnknownShape { .. })
    ));
}

#[test]
fn source_attributes_empty_array_reference_does_not_become_scalar() {
    let model = model(
        vec![0, 2_000_000],
        reference("p", Some(InstanceId::new(2))),
        vec![0, 2_000_000],
    );
    let catalog = model.source_attributes().unwrap();
    let values = catalog.variable(InstanceId::new(1)).unwrap();
    assert_eq!(values[0].dimensions(), [0, 2_000_000]);
    assert!(!values[0].each());
}

#[test]
fn reviewer_structured_selection_is_not_whole_occurrence_shape() {
    use rumoca_core::{ComponentRefPart, ComponentReference, DefId, Subscript};
    let component = ComponentReference::construct(
        false,
        span(),
        vec![ComponentRefPart {
            ident: "p".into(),
            span: span(),
            def_id: DefId::new(22),
            subs: vec![Subscript::Index {
                value: 1,
                span: span(),
            }],
        }],
    )
    .unwrap();
    let expression = Expression::VarRef {
        name: Reference::with_component_reference("p", component)
            .with_instance_id(InstanceId::new(2)),
        subscripts: vec![],
        span: span(),
    };
    let model = model(vec![4], expression, vec![4]);
    assert!(model.validate().is_ok());
    assert!(matches!(
        model
            .source_attributes()
            .unwrap()
            .variable(InstanceId::new(1)),
        Err(SourceAttributeError::UnknownShape { .. })
    ));
}

#[test]
fn reviewer_outer_subscript_is_not_whole_occurrence_shape() {
    use rumoca_core::Subscript;
    let expression = Expression::VarRef {
        name: Reference::new("p").with_instance_id(InstanceId::new(2)),
        subscripts: vec![Subscript::Index {
            value: 1,
            span: span(),
        }],
        span: span(),
    };
    let model = model(vec![4], expression, vec![4]);
    assert!(matches!(
        model
            .source_attributes()
            .unwrap()
            .variable(InstanceId::new(1)),
        Err(SourceAttributeError::UnknownShape { .. })
    ));
}

// Structured-reference identity regression controls.
fn structured_reference(parts: &[(&str, u32)], cached: &str) -> Expression {
    use rumoca_core::{ComponentRefPart, ComponentReference, DefId};
    let component = ComponentReference::construct(
        false,
        span(),
        parts
            .iter()
            .map(|(ident, def)| ComponentRefPart {
                ident: (*ident).into(),
                span: span(),
                def_id: DefId::new(*def),
                subs: vec![],
            })
            .collect(),
    )
    .unwrap();
    Expression::VarRef {
        name: Reference::with_component_reference(cached, component)
            .with_instance_id(InstanceId::new(2)),
        subscripts: vec![],
        span: span(),
    }
}

#[test]
fn source_attributes_field_selection_is_not_whole_occurrence_shape() {
    let model = model(
        vec![4],
        structured_reference(&[("p", 22), ("q", 23)], "p"),
        vec![4],
    );
    assert!(model.validate().is_ok());
    let result = model
        .source_attributes()
        .unwrap()
        .variable(InstanceId::new(1));
    eprintln!("field-selection p.q cached p -> {result:?}");
    assert!(matches!(
        result,
        Err(SourceAttributeError::UnknownShape { .. })
    ));
}

#[test]
fn source_attributes_differently_named_structure_is_not_whole_occurrence_shape() {
    let model = model(vec![4], structured_reference(&[("q", 23)], "p"), vec![4]);
    assert!(model.validate().is_ok());
    let result = model
        .source_attributes()
        .unwrap()
        .variable(InstanceId::new(1));
    eprintln!("structure q cached p -> {result:?}");
    assert!(matches!(
        result,
        Err(SourceAttributeError::UnknownShape { .. })
    ));
}
