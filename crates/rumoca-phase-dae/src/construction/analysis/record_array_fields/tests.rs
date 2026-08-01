use rumoca_core::{
    ComponentRefPart, ComponentReference, DefId, Expression, Reference, SourceMap, Span, Subscript,
    TypeId, VarName,
};

use super::super::PlannedRole;
use super::super::expression_validation::validate_expression_with_record_array_fields;
use super::*;

fn component_reference(parts: &[(&str, &[i64], DefId)], span: Span) -> ComponentReference {
    ComponentReference::construct(
        false,
        span,
        parts
            .iter()
            .map(|(ident, indices, def_id)| ComponentRefPart {
                ident: (*ident).to_string(),
                span,
                subs: indices
                    .iter()
                    .map(|value| Subscript::Index {
                        value: *value,
                        span,
                    })
                    .collect(),
                def_id: *def_id,
            })
            .collect(),
    )
    .expect("test component reference is nonempty")
}

/// Register one Flat coordinate together with the occurrence graph an
/// instantiated model really records for it: one component occurrence per part
/// of the reference, each separated from the next by the class occurrence that
/// declares it.
///
/// One deliberate departure from production: each call mints a *fresh* chain,
/// so two coordinates sharing a path prefix (`ac.pin[1].v` and `ac.pin[2].v`)
/// get two distinct `ac` occurrences where instantiation would share one. The
/// analysis walks each coordinate's own chain and compares declarations, so it
/// cannot tell the two apart — but a test that needs prefix *sharing* to be
/// observable must build its relations directly rather than through this
/// helper.
fn add_variable(
    model: &mut flat::Model,
    parts: &[(&str, &[i64], DefId)],
    instance_index: u32,
    value: (TypeId, &[i64]),
    span: Span,
) -> rumoca_core::InstanceId {
    add_variable_in_scope(
        model,
        rumoca_core::InstanceId::new(1),
        parts,
        instance_index,
        value,
        span,
    )
}

fn add_variable_in_scope(
    model: &mut flat::Model,
    scope: rumoca_core::InstanceId,
    parts: &[(&str, &[i64], DefId)],
    instance_index: u32,
    value: (TypeId, &[i64]),
    span: Span,
) -> rumoca_core::InstanceId {
    let (value_type, shape) = value;
    let reference = component_reference(parts, span);
    let name = reference.to_var_name();
    model
        .instance_relations
        .entry(scope)
        .or_insert(flat::InstanceRelation {
            owner: None,
            declaration: None,
            indices: Box::default(),
            kind: flat::InstanceKind::Class,
        });
    let mut owner = scope;
    let mut coordinate = scope;
    for (ordinal, (_, indices, def_id)) in parts.iter().enumerate() {
        let step = u32::try_from(ordinal).expect("test reference is short") * 1_000;
        let component = rumoca_core::InstanceId::new(instance_index + step);
        let is_target = ordinal + 1 == parts.len();
        model.instance_relations.insert(
            component,
            flat::InstanceRelation {
                owner: Some(owner),
                declaration: Some(*def_id),
                indices: indices.to_vec().into_boxed_slice(),
                kind: if is_target {
                    flat::InstanceKind::Materialized
                } else {
                    flat::InstanceKind::Aggregate
                },
            },
        );
        coordinate = component;
        if !is_target {
            let class = rumoca_core::InstanceId::new(instance_index + step + 500);
            model.instance_relations.insert(
                class,
                flat::InstanceRelation {
                    owner: Some(component),
                    declaration: None,
                    indices: Box::default(),
                    kind: flat::InstanceKind::Class,
                },
            );
            owner = class;
        }
    }
    model.variables.insert(
        name.clone(),
        flat::Variable {
            instance_id: coordinate,
            name: name.clone(),
            component_ref: Some(reference),
            type_id: value_type,
            dims: shape.to_vec(),
            ..flat::Variable::empty_with_span(span)
        },
    );
    coordinate
}

fn member_access(
    base: ComponentReference,
    subscript: Subscript,
    fields: &[(&str, DefId)],
    span: Span,
) -> Expression {
    fields.iter().fold(
        Expression::Index {
            base: Box::new(Expression::VarRef {
                name: Reference::from_component_reference(base)
                    .with_instance_id(rumoca_core::InstanceId::new(1)),
                subscripts: Vec::new(),
                span,
            }),
            subscripts: vec![subscript],
            span,
        },
        |base, (field, field_def_id)| Expression::FieldAccess {
            base: Box::new(base),
            field: (*field).to_string(),
            field_def_id: *field_def_id,
            span,
        },
    )
}

fn runtime_roles(model: &flat::Model) -> HashMap<VarName, PlannedRole> {
    model
        .variables
        .values()
        .map(|variable| (variable.name.clone(), PlannedRole::Algebraic))
        .collect()
}

fn projection(
    field_span: Span,
    base_span: Span,
    subscript: Subscript,
) -> (Expression, RecordArrayFieldPlans) {
    let reference = Reference::from_component_reference(
        ComponentReference::construct(
            false,
            base_span,
            vec![ComponentRefPart {
                ident: "pin".to_string(),
                span: base_span,
                subs: Vec::new(),
                def_id: DefId::new(1),
            }],
        )
        .expect("test projection base is nonempty"),
    )
    .with_instance_id(rumoca_core::InstanceId::new(1));
    let expression = Expression::FieldAccess {
        base: Box::new(Expression::Index {
            base: Box::new(Expression::VarRef {
                name: reference,
                subscripts: Vec::new(),
                span: base_span,
            }),
            subscripts: vec![subscript.clone()],
            span: base_span,
        }),
        field: "v".to_string(),
        field_def_id: DefId::new(3),
        span: field_span,
    };
    let plans = RecordArrayFieldPlans {
        by_occurrence: HashMap::from([(
            field_access_key(&expression).expect("projection has an exact key"),
            RecordArrayFieldPlan::Projection {
                coordinates: vec![rumoca_core::InstanceId::new(9)].into_boxed_slice(),
                target: DefId::new(3),
                value_type: TypeId::new(4),
                shape: Vec::new().into_boxed_slice(),
                subscripts: vec![subscript].into_boxed_slice(),
            },
        )]),
    };
    (expression, plans)
}

#[test]
fn dynamic_projection_subscript_fails_at_its_exact_occurrence() {
    let mut sources = SourceMap::new();
    let source = sources.add("record_projection.mo", "pin[k].v");
    let field_span = Span::from_offsets(source, 0, 8);
    let base_span = Span::from_offsets(source, 0, 6);
    let subscript_span = Span::from_offsets(source, 4, 5);
    let subscript = Subscript::Expr {
        expr: Box::new(Expression::VarRef {
            name: Reference::new("k"),
            subscripts: Vec::new(),
            span: subscript_span,
        }),
        span: subscript_span,
    };
    let (expression, plans) = projection(field_span, base_span, subscript);
    let roles = HashMap::from([(VarName::new("pin[1].v"), PlannedRole::Algebraic)]);

    let error =
        validate_expression_with_record_array_fields(&expression, &roles, &HashSet::new(), &plans)
            .expect_err("an undefined dynamic subscript must fail before DAE construction");

    assert!(matches!(
        error,
        ToDaeError::UnresolvedReference { name, span }
            if name == "k" && span == subscript_span
    ));
}

#[test]
fn absent_materialized_coordinate_fails_at_field_occurrence() {
    let mut sources = SourceMap::new();
    let source = sources.add("record_projection.mo", "pin[:].v");
    let field_span = Span::from_offsets(source, 0, 8);
    let colon_span = Span::from_offsets(source, 4, 5);
    let root = DefId::new(1);
    let field = DefId::new(3);
    let mut model = flat::Model::new();
    add_variable(
        &mut model,
        &[("pin", &[1], root), ("v", &[], field)],
        9,
        (TypeId::new(4), &[]),
        field_span,
    );
    let expression = member_access(
        component_reference(&[("pin", &[], root)], field_span),
        Subscript::Colon { span: colon_span },
        &[("v", field)],
        field_span,
    );

    let error = match analyze_record_array_fields(&model, [&expression], &HashMap::new()) {
        Ok(_) => {
            panic!("a selected Flat occurrence without a runtime role cannot mint a certificate")
        }
        Err(error) => error,
    };

    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "record-array member slice" && span == field_span
    ));
}

#[test]
fn nested_component_array_member_is_one_exact_materialized_coordinate() {
    let mut sources = SourceMap::new();
    let source = sources.add("nested_component.mo", "FF[1].RS1.Nor1.auxiliary");
    let span = Span::from_offsets(source, 0, 28);
    let base_root = DefId::new(10);
    let field_target = DefId::new(12);
    let value_type = TypeId::new(13);
    let mut model = flat::Model::new();
    let coordinate = add_variable(
        &mut model,
        &[
            ("FF", &[1], base_root),
            ("RS1", &[], DefId::new(11)),
            ("Nor1", &[], DefId::new(14)),
            ("auxiliary", &[], field_target),
        ],
        100,
        (value_type, &[2]),
        span,
    );
    let expression = member_access(
        component_reference(&[("FF", &[], base_root)], span),
        Subscript::Index { value: 1, span },
        &[
            ("RS1", DefId::new(11)),
            ("Nor1", DefId::new(14)),
            ("auxiliary", field_target),
        ],
        span,
    );

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("materialized path should plan");

    assert!(matches!(
        plans.get(&expression),
        Some(RecordArrayFieldPlan::MaterializedCoordinate {
            coordinate: planned,
            target,
            value_type: planned_type,
        }) if planned == &coordinate && *target == field_target && *planned_type == value_type
    ));
}

#[test]
fn same_span_component_instances_keep_distinct_materialized_coordinates() {
    let mut sources = SourceMap::new();
    let source = sources.add("replicated_component.mo", "FF[i].child.value");
    let span = Span::from_offsets(source, 0, 17);
    let base_root = DefId::new(20);
    let field_target = DefId::new(22);
    let value_type = TypeId::new(23);
    let mut model = flat::Model::new();
    let first = add_variable(
        &mut model,
        &[
            ("FF", &[1], base_root),
            ("child", &[], DefId::new(21)),
            ("value", &[], field_target),
        ],
        101,
        (value_type, &[]),
        span,
    );
    let second = add_variable(
        &mut model,
        &[
            ("FF", &[2], base_root),
            ("child", &[], DefId::new(21)),
            ("value", &[], field_target),
        ],
        102,
        (value_type, &[]),
        span,
    );
    let base = component_reference(&[("FF", &[], base_root)], span);
    let first_expression = member_access(
        base.clone(),
        Subscript::Index { value: 1, span },
        &[("child", DefId::new(21)), ("value", field_target)],
        span,
    );
    let second_expression = member_access(
        base,
        Subscript::Index { value: 2, span },
        &[("child", DefId::new(21)), ("value", field_target)],
        span,
    );

    let plans = analyze_record_array_fields(
        &model,
        [&first_expression, &second_expression],
        &runtime_roles(&model),
    )
    .expect("same-span replicated expressions should have compact distinct keys");

    assert!(matches!(
        plans.get(&first_expression),
        Some(RecordArrayFieldPlan::MaterializedCoordinate { coordinate, .. })
            if coordinate == &first
    ));
    assert!(matches!(
        plans.get(&second_expression),
        Some(RecordArrayFieldPlan::MaterializedCoordinate { coordinate, .. })
            if coordinate == &second
    ));
}

#[test]
fn block_array_slice_projects_the_exact_nested_value_member() {
    let mut sources = SourceMap::new();
    let source = sources.add("block_slice.mo", "blocks[:].child.value");
    let span = Span::from_offsets(source, 0, 21);
    let base_root = DefId::new(30);
    let field_target = DefId::new(32);
    let value_type = TypeId::new(33);
    let mut model = flat::Model::new();
    let first = add_variable(
        &mut model,
        &[
            ("blocks", &[1], base_root),
            ("child", &[], DefId::new(31)),
            ("value", &[], field_target),
        ],
        110,
        (value_type, &[]),
        span,
    );
    let second = add_variable(
        &mut model,
        &[
            ("blocks", &[2], base_root),
            ("child", &[], DefId::new(31)),
            ("value", &[], field_target),
        ],
        111,
        (value_type, &[]),
        span,
    );
    let expression = member_access(
        component_reference(&[("blocks", &[], base_root)], span),
        Subscript::Colon { span },
        &[("child", DefId::new(31)), ("value", field_target)],
        span,
    );

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("block slice should plan");

    assert!(matches!(
        plans.get(&expression),
        Some(RecordArrayFieldPlan::Projection {
            coordinates,
            target,
            value_type: planned_type,
            shape,
            ..
        }) if coordinates.as_ref() == [first, second]
            && *target == field_target
            && *planned_type == value_type
            && shape.is_empty()
    ));
}

/// MLS §10.5: a subscript belongs to the part it is written on, so `ac.pin[:].v`
/// slices `pin` — the connector array declared inside `ac` — and denotes the
/// member array `v`. This is the MSL `Interfaces.ACDC.ACplug` shape.
#[test]
fn nested_component_array_slice_projects_the_subscripted_part() {
    let mut sources = SourceMap::new();
    let source = sources.add("plug_slice.mo", "ac.pin[:].v");
    let span = Span::from_offsets(source, 0, 11);
    let plug = DefId::new(90);
    let pin = DefId::new(91);
    let potential = DefId::new(92);
    let value_type = TypeId::new(93);
    let mut model = flat::Model::new();
    let coordinates = [
        add_variable(
            &mut model,
            &[("ac", &[], plug), ("pin", &[1], pin), ("v", &[], potential)],
            190,
            (value_type, &[]),
            span,
        ),
        add_variable(
            &mut model,
            &[("ac", &[], plug), ("pin", &[2], pin), ("v", &[], potential)],
            191,
            (value_type, &[]),
            span,
        ),
    ];
    let expression = member_access(
        component_reference(&[("ac", &[], plug), ("pin", &[], pin)], span),
        Subscript::Colon { span },
        &[("v", potential)],
        span,
    );

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("a slice on a nested component array has an exact projection proof");

    assert!(
        matches!(
            plans.get(&expression),
            Some(RecordArrayFieldPlan::Projection { coordinates: planned, target, .. })
                if planned.as_ref() == coordinates && *target == potential
        ),
        "the projection must select the members of the subscripted part, in index order"
    );
}

/// Two sibling instances of one class spell the same nested slice, and both
/// spellings resolve to the same declarations. Only the occurrence graph
/// separates them, so a coordinate owned by the other instance must not join
/// this projection.
#[test]
fn a_sibling_instance_cannot_join_a_nested_slice() {
    let mut sources = SourceMap::new();
    let source = sources.add("sibling_slice.mo", "ac.pin[:].v");
    let span = Span::from_offsets(source, 0, 11);
    let plug = DefId::new(110);
    let pin = DefId::new(111);
    let potential = DefId::new(112);
    let value_type = TypeId::new(113);
    let sibling_scope = rumoca_core::InstanceId::new(2);
    let mut model = flat::Model::new();
    let selected = [
        add_variable(
            &mut model,
            &[("ac", &[], plug), ("pin", &[1], pin), ("v", &[], potential)],
            196,
            (value_type, &[]),
            span,
        ),
        add_variable(
            &mut model,
            &[("ac", &[], plug), ("pin", &[2], pin), ("v", &[], potential)],
            197,
            (value_type, &[]),
            span,
        ),
    ];
    // Same declarations, other instance. The idents differ only so the Flat map
    // can hold both rendered names; the analysis never reads them.
    for (index, instance_index) in [(1, 198), (2, 199)] {
        add_variable_in_scope(
            &mut model,
            sibling_scope,
            &[
                ("sibling_ac", &[], plug),
                ("sibling_pin", &[index], pin),
                ("sibling_v", &[], potential),
            ],
            instance_index,
            (value_type, &[]),
            span,
        );
    }
    let expression = member_access(
        component_reference(&[("ac", &[], plug), ("pin", &[], pin)], span),
        Subscript::Colon { span },
        &[("v", potential)],
        span,
    );

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("a nested slice plans from the instance that wrote it");

    assert!(
        matches!(
            plans.get(&expression),
            Some(RecordArrayFieldPlan::Projection { coordinates, .. })
                if coordinates.as_ref() == selected
        ),
        "the sibling instance's coordinates must stay out of this projection"
    );
}

/// A coordinate reached through a different path prefix shares no projection,
/// even when the sliced part and the member declaration are the same ones.
#[test]
fn a_foreign_path_prefix_cannot_join_a_nested_slice() {
    let mut sources = SourceMap::new();
    let source = sources.add("prefix_identity.mo", "ac.pin[:].v");
    let span = Span::from_offsets(source, 0, 11);
    let plug = DefId::new(94);
    let other_plug = DefId::new(95);
    let pin = DefId::new(96);
    let potential = DefId::new(97);
    let value_type = TypeId::new(98);
    let mut model = flat::Model::new();
    let selected = add_variable(
        &mut model,
        &[("ac", &[], plug), ("pin", &[1], pin), ("v", &[], potential)],
        192,
        (value_type, &[]),
        span,
    );
    add_variable(
        &mut model,
        &[
            ("dc", &[], other_plug),
            ("pin", &[1], pin),
            ("v", &[], potential),
        ],
        193,
        (value_type, &[]),
        span,
    );
    let expression = member_access(
        component_reference(&[("ac", &[], plug), ("pin", &[], pin)], span),
        Subscript::Colon { span },
        &[("v", potential)],
        span,
    );

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("a nested slice plans from its own declaration chain");

    assert!(
        matches!(
            plans.get(&expression),
            Some(RecordArrayFieldPlan::Projection { coordinates, .. })
                if coordinates.as_ref() == [selected]
        ),
        "only the coordinates reached through the written path prefix may be projected"
    );
}

/// One `Index` node carries one part's subscripts. When a second part of the
/// path is itself an array occurrence the expression denotes a higher-rank
/// array (MLS §10.5) that this certificate cannot describe, so it is rejected
/// by name rather than silently projecting one of the two extents.
///
/// This arm is a DEFENSIVE GUARD, not a reachable source rejection: the only
/// spelling that reaches it, `leaf[1].ac.pin[:].v`, is legal under §10.5 but
/// never becomes a `ProjectionPattern`, because Flat lowering roots such a
/// reference in a `FieldAccess` rather than a bare `VarRef` and
/// `projection_pattern` declines those. The guard is what keeps the certificate
/// honest if that ever changes, so the test drives the analysis directly and
/// asserts the arm's own `detail` — a `feature`/`span` match alone also passes
/// against the dense-rectangular rejection below it, which would let the arm be
/// deleted unnoticed.
#[test]
fn a_second_array_part_on_the_slice_path_is_rejected_by_name() {
    let mut sources = SourceMap::new();
    let source = sources.add("two_array_parts.mo", "ac.pin[:].v");
    let span = Span::from_offsets(source, 0, 11);
    let plug = DefId::new(99);
    let pin = DefId::new(100);
    let potential = DefId::new(101);
    let value_type = TypeId::new(102);
    let mut model = flat::Model::new();
    for (plug_index, instance_index) in [(1, 194), (2, 195)] {
        add_variable(
            &mut model,
            &[
                ("ac", &[plug_index], plug),
                ("pin", &[1], pin),
                ("v", &[], potential),
            ],
            instance_index,
            (value_type, &[]),
            span,
        );
    }
    let expression = member_access(
        component_reference(&[("ac", &[], plug), ("pin", &[], pin)], span),
        Subscript::Colon { span },
        &[("v", potential)],
        span,
    );

    let error = match analyze_record_array_fields(&model, [&expression], &runtime_roles(&model)) {
        Ok(_) => panic!("two array parts on one path cannot mint a rank-one projection"),
        Err(error) => error,
    };

    let ToDaeError::UnsupportedFlatSemantics {
        feature,
        detail,
        span: error_span,
    } = error
    else {
        panic!("a second array part must be an unsupported-semantics rejection");
    };
    assert_eq!(feature, "record-array member slice");
    assert_eq!(error_span, span);
    assert!(
        detail.contains("another part of this path is itself an array occurrence"),
        "the rejection must name the second array part, not fall through to the \
         dense-rectangular check; got: {detail}"
    );
}

/// A written slice of rank two or more denotes a rank-`n` member array. The
/// projection certificate carries one linear coordinate run and the lowering
/// indexes it as a rank-one array, so accepting the certificate would mis-index
/// it. MLS §10.5 gives the construct a meaning; this compiler abstains from it
/// by name instead of minting a shape it cannot realize.
#[test]
fn a_multi_dimensional_member_slice_abstains_by_name() {
    let mut sources = SourceMap::new();
    let source = sources.add("multi_rank_slice.mo", "ac.pin[:,:].v");
    let span = Span::from_offsets(source, 0, 13);
    let plug = DefId::new(120);
    let pin = DefId::new(121);
    let potential = DefId::new(122);
    let value_type = TypeId::new(123);
    let mut model = flat::Model::new();
    for (row, column, instance_index) in [(1, 1, 200), (1, 2, 201), (2, 1, 202), (2, 2, 203)] {
        add_variable(
            &mut model,
            &[
                ("ac", &[], plug),
                ("pin", &[row, column], pin),
                ("v", &[], potential),
            ],
            instance_index,
            (value_type, &[]),
            span,
        );
    }
    let expression = Expression::FieldAccess {
        base: Box::new(Expression::Index {
            base: Box::new(Expression::VarRef {
                name: Reference::from_component_reference(component_reference(
                    &[("ac", &[], plug), ("pin", &[], pin)],
                    span,
                ))
                .with_instance_id(rumoca_core::InstanceId::new(1)),
                subscripts: Vec::new(),
                span,
            }),
            subscripts: vec![Subscript::Colon { span }, Subscript::Colon { span }],
            span,
        }),
        field: "v".to_string(),
        field_def_id: potential,
        span,
    };

    let error = match analyze_record_array_fields(&model, [&expression], &runtime_roles(&model)) {
        Ok(_) => panic!("a rank-two written slice has no rank-preserving certificate"),
        Err(error) => error,
    };

    let ToDaeError::UnsupportedFlatSemantics {
        feature,
        detail,
        span: error_span,
    } = error
    else {
        panic!("a multi-dimensional member slice must be an unsupported-semantics rejection");
    };
    assert_eq!(feature, "record-array member slice");
    assert_eq!(error_span, span);
    assert!(
        detail.contains("multi-dimensional member slice"),
        "the abstention must name the multi-dimensional slice; got: {detail}"
    );
}

#[test]
fn same_shaped_sibling_fields_keep_distinct_projection_identities() {
    let mut sources = SourceMap::new();
    let source = sources.add("sibling_fields.mo", "records[:].left");
    let span = Span::from_offsets(source, 0, 15);
    let root = DefId::new(70);
    let left = DefId::new(71);
    let right = DefId::new(72);
    let value_type = TypeId::new(73);
    let mut model = flat::Model::new();
    let left_coordinates = [
        add_variable(
            &mut model,
            &[("records", &[1], root), ("left", &[], left)],
            170,
            (value_type, &[]),
            span,
        ),
        add_variable(
            &mut model,
            &[("records", &[2], root), ("left", &[], left)],
            171,
            (value_type, &[]),
            span,
        ),
    ];
    let right_coordinates = [
        add_variable(
            &mut model,
            &[("records", &[1], root), ("right", &[], right)],
            270,
            (value_type, &[]),
            span,
        ),
        add_variable(
            &mut model,
            &[("records", &[2], root), ("right", &[], right)],
            271,
            (value_type, &[]),
            span,
        ),
    ];
    let base = component_reference(&[("records", &[], root)], span);
    let left_expression = member_access(
        base.clone(),
        Subscript::Colon { span },
        &[("left", left)],
        span,
    );
    let right_expression =
        member_access(base, Subscript::Colon { span }, &[("right", right)], span);

    let plans = analyze_record_array_fields(
        &model,
        [&left_expression, &right_expression],
        &runtime_roles(&model),
    )
    .expect("exact sibling declaration chains produce independent certificates");

    assert!(matches!(
        plans.get(&left_expression),
        Some(RecordArrayFieldPlan::Projection { coordinates, target, .. })
            if coordinates.as_ref() == left_coordinates && *target == left
    ));
    assert!(matches!(
        plans.get(&right_expression),
        Some(RecordArrayFieldPlan::Projection { coordinates, target, .. })
            if coordinates.as_ref() == right_coordinates && *target == right
    ));
}

#[test]
fn rendered_name_mismatch_cannot_redirect_materialized_identity() {
    let mut sources = SourceMap::new();
    let source = sources.add("rendered_alias.mo", "records[1].value");
    let span = Span::from_offsets(source, 0, 16);
    let root = DefId::new(80);
    let field = DefId::new(81);
    let mut model = flat::Model::new();
    let coordinate = add_variable(
        &mut model,
        &[("records", &[1], root), ("value", &[], field)],
        180,
        (TypeId::new(82), &[]),
        span,
    );
    let structured_name = VarName::new("records[1].value");
    let mut variable = model
        .variables
        .shift_remove(&structured_name)
        .expect("test coordinate uses its rendered structured name");
    let display_alias = VarName::new("display.alias");
    variable.name = display_alias.clone();
    model.variables.insert(display_alias, variable);
    let expression = member_access(
        component_reference(&[("records", &[], root)], span),
        Subscript::Index { value: 1, span },
        &[("value", field)],
        span,
    );

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("exact identity is independent of rendered storage spelling");

    assert!(matches!(
        plans.get(&expression),
        Some(RecordArrayFieldPlan::MaterializedCoordinate {
            coordinate: planned,
            target,
            ..
        }) if *planned == coordinate && *target == field
    ));
}

#[test]
fn same_spelling_foreign_identity_cannot_join_projection_family() {
    let mut sources = SourceMap::new();
    let source = sources.add("shadowed_slice.mo", "records[:].value");
    let span = Span::from_offsets(source, 0, 16);
    let base_root = DefId::new(40);
    let mut model = flat::Model::new();
    add_variable(
        &mut model,
        &[
            ("records", &[1], DefId::new(140)),
            ("value", &[], DefId::new(42)),
        ],
        120,
        (TypeId::new(43), &[]),
        span,
    );
    let expression = member_access(
        component_reference(&[("records", &[], base_root)], span),
        Subscript::Colon { span },
        &[("value", DefId::new(42))],
        span,
    );

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("foreign identity is ignored, not repaired by spelling");

    assert!(
        plans.get(&expression).is_none(),
        "same spelling without shared DefId ancestry must not mint a projection proof"
    );
}

#[test]
fn inconsistent_projected_member_shape_fails_before_construction() {
    let mut sources = SourceMap::new();
    let source = sources.add("shape_mismatch.mo", "records[:].value");
    let span = Span::from_offsets(source, 0, 16);
    let base_root = DefId::new(50);
    let field_target = DefId::new(52);
    let value_type = TypeId::new(53);
    let mut model = flat::Model::new();
    add_variable(
        &mut model,
        &[("records", &[1], base_root), ("value", &[], field_target)],
        130,
        (value_type, &[]),
        span,
    );
    add_variable(
        &mut model,
        &[("records", &[2], base_root), ("value", &[], field_target)],
        131,
        (value_type, &[2]),
        span,
    );
    let expression = member_access(
        component_reference(&[("records", &[], base_root)], span),
        Subscript::Colon { span },
        &[("value", field_target)],
        span,
    );

    let error = match analyze_record_array_fields(&model, [&expression], &runtime_roles(&model)) {
        Ok(_) => panic!("non-uniform member shapes must violate MLS ARR-004"),
        Err(error) => error,
    };

    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span: error_span, .. }
            if feature == "record-array member slice" && error_span == span
    ));
}

#[test]
fn unproven_empty_slice_cannot_fabricate_scalar_elements() {
    let mut sources = SourceMap::new();
    let source = sources.add("empty_slice.mo", "records[:].value");
    let span = Span::from_offsets(source, 0, 16);
    let expression = member_access(
        component_reference(&[("records", &[], DefId::new(60))], span),
        Subscript::Colon { span },
        &[("value", DefId::new(61))],
        span,
    );
    let model = flat::Model::new();

    let plans = analyze_record_array_fields(&model, [&expression], &runtime_roles(&model))
        .expect("absence of elements cannot create a projection proof");
    let error = validate_expression_with_record_array_fields(
        &expression,
        &HashMap::new(),
        &HashSet::new(),
        &plans,
    )
    .expect_err("an untyped empty slice must fail rather than inventing a value");

    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { span: error_span, .. } if error_span == span
    ));
}
