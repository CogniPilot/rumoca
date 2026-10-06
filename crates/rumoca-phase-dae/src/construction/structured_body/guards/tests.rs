use super::*;
use rumoca_core::{ComponentRefPart, ComponentReference, DefId, EffectiveType, Reference, TypeId};

fn declaration(value: i64) -> (ShapeEnvironment, Reference, Span, SourceMap) {
    let mut sources = SourceMap::new();
    let source = sources.add("guard.mo", "i > radius");
    let span = Span::from_offsets(source, 0, 10);
    let path = ComponentReference::construct(
        false,
        span,
        vec![ComponentRefPart {
            ident: "radius".into(),
            span,
            subs: Vec::new(),
            def_id: DefId::new(116),
        }],
    )
    .unwrap();
    let integer = TypeId::new(9);
    let mut flat = flat::Model::default();
    flat.predefined_types.integer = integer;
    flat.effective_types.insert(
        integer,
        EffectiveType::new(integer, integer, Vec::new()).unwrap(),
    );
    let variable = flat::Variable {
        name: VarName::new("radius"),
        instance_id: InstanceId::new(4),
        component_ref: Some(path.clone()),
        type_id: integer,
        variability: Variability::Parameter(Default::default()),
        ..flat::Variable::empty_with_span(span)
    };
    let mut values = EvalContext::new();
    values.add_instance_parameter(variable.instance_id, "radius", EvalValue::Integer(value));
    let mut shapes = ShapeEnvironment::default();
    shapes.bind_slice_constant(&flat, &variable, &values);
    shapes.bind_slice_binder(VarName::new("i"), 1, 90);
    let reference =
        Reference::with_component_reference("radius", path).with_instance_id(variable.instance_id);
    (shapes, reference, span, sources)
}

fn read(reference: Reference, span: Span) -> Expression {
    Expression::VarRef {
        name: reference,
        subscripts: Vec::new(),
        span,
    }
}

fn compare(radius: Reference, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Gt,
        lhs: Box::new(read(Reference::new("i"), span)),
        rhs: Box::new(read(radius, span)),
        span,
    }
}

fn with_binder(
    sources: SourceMap,
    span: Span,
    check: impl for<'dae> FnOnce(&HashMap<VarName, dae::DomainBinderId<'dae>>),
) {
    dae::Dae::construct(sources, |construction| {
        let provenance = dae::DaeProvenance::source(span)?;
        let domain = construction.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".into(),
                        lower: 1,
                        upper: 90,
                        step: 1,
                    }],
                },
                provenance,
            )
        })?;
        let binder = construction.domains(|domains| domains.binder(domain, 0, provenance))?;
        check(&HashMap::from([(VarName::new("i"), binder)]));
        Ok(())
    })
    .unwrap();
}

#[test]
fn settled_structural_guards_match_original_pointwise_parameter_evaluation() {
    for radius in [0, 3, 7, 90, i64::MAX] {
        let (shapes, reference, span, sources) = declaration(radius);
        let original = compare(reference, span);
        with_binder(sources, span, |binders| {
            let settled = predicate(&original, &shapes, binders).unwrap();
            for point in 1..=90 {
                let mut values = EvalContext::new();
                values.add_parameter("i", EvalValue::Integer(point));
                values.add_instance_parameter(
                    InstanceId::new(4),
                    "radius",
                    EvalValue::Integer(radius),
                );
                assert_eq!(
                    eval_expr(&original, &values).unwrap(),
                    eval_expr(&settled, &values).unwrap()
                );
            }
            assert_eq!(expression_span(&settled).unwrap(), span);
        });
    }
}

#[test]
fn structural_guard_proof_declines_complete_runtime_guard_and_wrong_declarations() {
    let (shapes, reference, span, sources) = declaration(3);
    let original = compare(reference.clone(), span);
    let runtime = Expression::Binary {
        op: OpBinary::And,
        lhs: Box::new(original.clone()),
        rhs: Box::new(read(Reference::new("inputFlag"), span)),
        span,
    };
    with_binder(sources, span, |binders| {
        assert!(predicate(&runtime, &shapes, binders).is_none());
        assert!(predicate(&compare(Reference::new("radius"), span), &shapes, binders).is_none());
        let wrong = reference.clone().with_instance_id(InstanceId::new(5));
        assert!(predicate(&compare(wrong, span), &shapes, binders).is_none());
        let mut shadowed = shapes.clone();
        shadowed.bind_integer_bounds(VarName::new("radius"), 3, 3);
        assert!(predicate(&original, &shadowed, binders).is_none());
        let body = Expression::If {
            branches: vec![(original.clone(), original.clone())],
            else_branch: Box::new(original.clone()),
            span,
        };
        assert!(settle(&body, &shapes.in_attribute_scope(), binders).is_none());
        assert!(settle(&body, &shapes, &HashMap::new()).is_none());
    });
}

#[test]
fn settled_guard_keeps_original_integer_overflow_and_operator_order() {
    let (shapes, reference, span, sources) = declaration(i64::MAX);
    let original = Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(read(reference, span)),
        rhs: Box::new(Expression::Literal {
            value: Literal::Integer(1),
            span,
        }),
        span,
    };
    with_binder(sources, span, |binders| {
        let settled = predicate(&original, &shapes, binders).unwrap();
        let mut values = EvalContext::new();
        values.add_instance_parameter(InstanceId::new(4), "radius", EvalValue::Integer(i64::MAX));
        assert_eq!(
            format!("{:?}", eval_expr(&original, &values)),
            format!("{:?}", eval_expr(&settled, &values))
        );
        assert!(eval_expr(&settled, &values).is_err());
    });
}
