//! MLS §10.4/§10.7 and §12.2: a record container contributes its own
//! dimensions before a structural field projection appends the field shape.

use super::*;

fn record_reference(model: &mut flat::Model, name: &str, dims: Vec<i64>, span: Span) -> Expression {
    let ordinal = u32::try_from(model.record_instances.len()).expect("fixture count fits u32");
    let component_ref = rumoca_core::ComponentReference::construct(
        false,
        span,
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
            def_id: DefId::new(900 + ordinal),
        }],
    )
    .expect("fixture record has a declared component identity");
    model.record_instances.insert(
        VarName::new(name),
        flat::RecordInstance {
            instance_id: rumoca_core::InstanceId::new(100 + ordinal),
            component_ref: component_ref.clone(),
            source_span: span,
            effective_type_id: TypeId::new(100 + ordinal),
            type_name: "Pair".to_string(),
            type_def_id: DefId::new(40),
            dims,
        },
    );
    Expression::VarRef {
        name: Reference::with_component_reference(name, component_ref),
        subscripts: Vec::new(),
        span,
    }
}

fn record_source_span() -> Span {
    let mut sources = SourceMap::new();
    let text = "cat(1, {core}, arms, payloads)";
    let source = sources.add("record_container_shapes.mo", text);
    Span::from_offsets(source, 0, text.len())
}

#[test]
fn record_container_shapes_preserve_scalar_array_and_empty_domains() {
    let span = record_source_span();
    let (mut model, _, _) = pair_constructor_model(span);
    let cases = [
        ("core", vec![]),
        ("arms", vec![4]),
        ("payloads", vec![0]),
        ("empty_grid", vec![2, 0, 3]),
    ];
    let expressions = cases
        .iter()
        .map(|(name, dims)| record_reference(&mut model, name, dims.clone(), span))
        .collect::<Vec<_>>();
    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new())
        .expect("retained record declarations supply exact model shapes");
    for ((name, expected), expression) in cases.iter().zip(&expressions) {
        assert!(!model.variables.contains_key(&VarName::new(*name)));
        let shape = analysis
            .expression_shape(expression, analysis.model_values())
            .expect("a declared record reference has a shape without a primitive placeholder");
        assert_eq!(
            shape.iter().map(|dim| i64::from(*dim)).collect::<Vec<_>>(),
            *expected
        );
    }
}

#[test]
fn record_container_cat_field_shapes_include_container_and_field_axes() {
    let span = record_source_span();
    let (mut model, _, left) = pair_constructor_model(span);
    model.record_types.get_mut(&DefId::new(40)).unwrap().fields[1].dims = vec![3, 3];
    let mut right = real_param("right", vec![3, 3], span);
    right.def_id = Some(DefId::new(42));
    model
        .functions
        .get_mut(&VarName::new("Pair"))
        .unwrap()
        .inputs[1] = right;
    let core = record_reference(&mut model, "core", vec![], span);
    let arms = record_reference(&mut model, "arms", vec![4], span);
    let payloads = record_reference(&mut model, "payloads", vec![0], span);
    let cat = Expression::BuiltinCall {
        function: BuiltinFunction::Cat,
        args: vec![
            Expression::Literal {
                value: Literal::Integer(1),
                span,
            },
            Expression::Array {
                elements: vec![core],
                kind: rumoca_core::ArrayConstructor::Array,
                span,
            },
            arms,
            payloads,
        ],
        span,
    };
    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new())
        .expect("record shape environment is valid");
    for (field, field_def_id, expected) in [
        ("left", left, vec![5]),
        ("right", DefId::new(42), vec![5, 3, 3]),
    ] {
        let expression = Expression::FieldAccess {
            base: Box::new(cat.clone()),
            field: field.to_string(),
            field_def_id,
            span,
        };
        assert_eq!(
            analysis
                .expression_shape(&expression, analysis.model_values())
                .expect("cat of scalar, nonempty and empty record operands has exact field shape"),
            expected,
        );
    }
}

#[test]
fn record_container_shapes_reject_unknown_references_and_invalid_extents() {
    let span = record_source_span();
    let (mut model, _, _) = pair_constructor_model(span);
    let expression = record_reference(&mut model, "missing", vec![], span);
    model.record_instances.clear();
    let analysis = FunctionShapeAnalysis::analyze(&model, &EvalContext::new()).unwrap();
    assert!(matches!(
        analysis.expression_shape(&expression, analysis.model_values()),
        Err(ToDaeError::UnresolvedReference { name, .. }) if name == "missing"
    ));
    record_reference(&mut model, "invalid", vec![-1], span);
    assert!(matches!(
        FunctionShapeAnalysis::analyze(&model, &EvalContext::new()),
        Err(ToDaeError::UnsupportedFlatSemantics { feature, detail, .. })
            if feature == "function shape proof" && detail.contains("non-concrete extent `-1`")
    ));
}
