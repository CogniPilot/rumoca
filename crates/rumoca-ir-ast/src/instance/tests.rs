use super::*;
use crate::{ComponentRefPart, ComponentReference, Location, Token};

const EQ_RECORD: DefId = DefId(101);
const EQ_OTHER_RECORD: DefId = DefId(102);
const EQ_FUNCTION: DefId = DefId(103);
const EQ_INPUT_A: DefId = DefId(104);
const EQ_INPUT_B: DefId = DefId(105);
const EQ_OUTPUT: DefId = DefId(106);
const EQ_REAL: DefId = DefId(107);

fn equality_token(text: &str, offset: u32) -> Token {
    Token {
        text: std::sync::Arc::from(text),
        location: Location {
            source: rumoca_core::SourceId::from_source_name("instance_equality_constraint_test.mo"),
            start: offset,
            end: offset + u32::try_from(text.len()).expect("test token length"),
            ..Default::default()
        },
        ..Default::default()
    }
}

fn equality_extent(value: usize) -> Expression {
    Expression::Terminal {
        terminal_type: crate::TerminalType::UnsignedInteger,
        token: equality_token(&value.to_string(), 80),
        span: Span::from_offsets(
            rumoca_core::SourceId::from_source_name("instance_equality_constraint_test.mo"),
            80,
            81,
        ),
    }
}

fn equality_symbolic_extent(name: &str) -> Expression {
    let token = equality_token(name, 81);
    Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![ComponentRefPart {
            ident: token,
            ..Default::default()
        }],
        span: Span::from_offsets(
            rumoca_core::SourceId::from_source_name("instance_equality_constraint_test.mo"),
            81,
            82,
        ),
        qualified_display_name: None,
    })
}

fn equality_negative_extent(value: &str) -> Expression {
    Expression::Unary {
        op: rumoca_core::OpUnary::Minus,
        rhs: std::sync::Arc::new(Expression::Terminal {
            terminal_type: crate::TerminalType::UnsignedInteger,
            token: equality_token(value, 82),
            span: Span::from_offsets(
                rumoca_core::SourceId::from_source_name("instance_equality_constraint_test.mo"),
                82,
                83,
            ),
        }),
        span: Span::from_offsets(
            rumoca_core::SourceId::from_source_name("instance_equality_constraint_test.mo"),
            81,
            83,
        ),
    }
}

fn equality_component(
    def_id: DefId,
    name: &str,
    type_def_id: DefId,
    causality: Causality,
) -> crate::Component {
    crate::Component {
        def_id: Some(def_id),
        type_def_id: Some(type_def_id),
        name: name.to_string(),
        name_token: equality_token(name, def_id.index()),
        type_name: crate::Name::from_string(if type_def_id == EQ_REAL { "Real" } else { "R" }),
        causality,
        location: equality_token(name, def_id.index()).location,
        ..crate::Component::empty_with_span(Span::from_offsets(
            rumoca_core::SourceId::from_source_name("instance_equality_constraint_test.mo"),
            1,
            2,
        ))
    }
}

#[test]
fn instance_data_wire_requires_declaration_def_id_but_accepts_null() {
    let complete = serde_json::to_value(InstanceData::default())
        .expect("serialize current instance-data wire");
    assert!(complete["declaration_def_id"].is_null());
    serde_json::from_value::<InstanceData>(complete.clone())
        .expect("explicit null is valid declaration identity absence");

    let mut missing = complete;
    missing
        .as_object_mut()
        .expect("instance-data wire is an object")
        .remove("declaration_def_id")
        .expect("current instance-data wire contains declaration_def_id");
    assert!(
        serde_json::from_value::<InstanceData>(missing).is_err(),
        "current instance-data wire must reject an omitted declaration_def_id"
    );
}

#[test]
fn current_instance_wire_rejects_deleted_scope_and_override_keys() {
    let complete = serde_json::to_value(ClassInstanceData::default())
        .expect("serialize current class-instance wire");
    assert!(complete["source_scope"].is_null());
    assert!(complete["source_scope_id"].is_null());
    assert!(complete["class_overrides"].is_object());
    for key in ["source_scope", "source_scope_id", "class_overrides"] {
        let mut missing = complete.clone();
        missing
            .as_object_mut()
            .expect("class-instance wire is an object")
            .remove(key)
            .unwrap_or_else(|| panic!("class-instance wire contains `{key}`"));
        assert!(
            serde_json::from_value::<ClassInstanceData>(missing).is_err(),
            "deleted `{key}` must not invent class-instance semantics"
        );
    }

    let override_value = ClassOverride::new("Alias", DefId::new(41), DefId::new(42), None);
    let complete = serde_json::to_value(override_value).expect("class override serializes");
    assert!(complete["target_ref"].is_null());
    assert!(complete["modifier_args"].is_array());
    for key in ["target_ref", "modifier_args"] {
        let mut missing = complete.clone();
        missing
            .as_object_mut()
            .expect("class-override wire is an object")
            .remove(key)
            .unwrap_or_else(|| panic!("class-override wire contains `{key}`"));
        assert!(
            serde_json::from_value::<ClassOverride>(missing).is_err(),
            "deleted `{key}` must not invent class-override semantics"
        );
    }
}

fn equality_constraint_tree(extent: usize) -> ClassTree {
    let mut function = ClassDef {
        def_id: Some(EQ_FUNCTION),
        name: equality_token("equalityConstraint", 30),
        class_type: ClassType::Function,
        ..Default::default()
    };
    function.components.insert(
        "a".to_string(),
        equality_component(
            EQ_INPUT_A,
            "a",
            EQ_RECORD,
            Causality::Input(equality_token("input", 50)),
        ),
    );
    function.components.insert(
        "b".to_string(),
        equality_component(
            EQ_INPUT_B,
            "b",
            EQ_RECORD,
            Causality::Input(equality_token("input", 55)),
        ),
    );
    let mut output = equality_component(
        EQ_OUTPUT,
        "residue",
        EQ_REAL,
        Causality::Output(equality_token("output", 60)),
    );
    output.shape = vec![extent];
    output.shape_expr = vec![Subscript::Expression(equality_extent(extent))];
    function.components.insert("residue".to_string(), output);

    let mut record = ClassDef {
        def_id: Some(EQ_RECORD),
        name: equality_token("R", 10),
        class_type: ClassType::Record,
        ..Default::default()
    };
    record
        .classes
        .insert("equalityConstraint".to_string(), function);
    let other_record = ClassDef {
        def_id: Some(EQ_OTHER_RECORD),
        // Deliberately the same local spelling as the effective record.
        name: equality_token("R", 20),
        class_type: ClassType::Record,
        ..Default::default()
    };

    let mut tree = ClassTree::new();
    tree.scope_tree
        .add_predefined_member(rumoca_core::ComponentPath::from_flat_path("Real"), EQ_REAL);
    tree.definitions.classes.insert("A.R".to_string(), record);
    tree.definitions
        .classes
        .insert("B.R".to_string(), other_record);
    for (def_id, name) in [
        (EQ_RECORD, "A.R"),
        (EQ_OTHER_RECORD, "B.R"),
        (EQ_FUNCTION, "A.R.equalityConstraint"),
        (EQ_INPUT_A, "A.R.equalityConstraint.a"),
        (EQ_INPUT_B, "A.R.equalityConstraint.b"),
        (EQ_OUTPUT, "A.R.equalityConstraint.residue"),
        (EQ_REAL, "Real"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
    tree
}

fn equality_record_and_function(tree: &ClassTree) -> (ClassDef, ClassDef) {
    let record = tree.definitions.classes["A.R"].clone();
    let function = record.classes["equalityConstraint"].clone();
    (record, function)
}

fn equality_function_mut(tree: &mut ClassTree) -> &mut ClassDef {
    tree.definitions
        .classes
        .get_mut("A.R")
        .expect("record fixture")
        .classes
        .get_mut("equalityConstraint")
        .expect("equalityConstraint fixture")
}

fn reidentified_equality_function(
    mut function: ClassDef,
    function_def_id: DefId,
    interface_def_ids: [DefId; 3],
    name: &str,
) -> ClassDef {
    function.def_id = Some(function_def_id);
    function.name = equality_token(name, function_def_id.index());
    for (component, def_id) in function.components.values_mut().zip(interface_def_ids) {
        component.def_id = Some(def_id);
    }
    function
}

fn equality_selection(
    tree: &ClassTree,
    record: &ClassDef,
) -> (
    EqualityConstraintDeclarationIndex,
    EqualityConstraintSelectionProof,
) {
    let index = EqualityConstraintDeclarationIndex::new(tree);
    let selection = index
        .prove_equality_constraint_selection(tree, record.def_id.expect("record fixture identity"))
        .expect("selection proof is decidable")
        .expect("record exposes equalityConstraint");
    (index, selection)
}

#[test]
fn equality_constraint_exposure_distinguishes_vacuous_and_nonempty() {
    let tree = equality_constraint_tree(0);
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    let prototype = index
        .check_equality_constraint_prototype(&tree, selection)
        .expect("exact interface is valid");
    assert_eq!(prototype.slot_def_id(), EQ_FUNCTION);
    assert_eq!(prototype.selected_function_def_id(), EQ_FUNCTION);
    assert_eq!(prototype.output_def_id(), EQ_OUTPUT);
    let vacuous = index
        .check_equality_constraint_cardinality(&tree, prototype)
        .expect("Real[0] is a valid vacuous result");
    assert_eq!(vacuous, EqualityConstraintCardinality::Vacuous);

    let tree = equality_constraint_tree(3);
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    let prototype = index
        .check_equality_constraint_prototype(&tree, selection)
        .expect("exact interface is valid");
    let nonempty = index
        .check_equality_constraint_cardinality(&tree, prototype)
        .expect("Real[3] is valid");
    assert_eq!(nonempty.scalar_count(), 3);
}

#[test]
fn equality_constraint_uses_exact_record_and_predefined_real_identities() {
    let mut tree = equality_constraint_tree(1);
    equality_function_mut(&mut tree).components["a"].type_def_id = Some(EQ_OTHER_RECORD);
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    assert_eq!(
        index.check_equality_constraint_prototype(&tree, selection),
        Err(EqualityConstraintExposureError::InputHasWrongRecordIdentity { index: 0 })
    );

    let mut tree = equality_constraint_tree(1);
    equality_function_mut(&mut tree).components["residue"].type_def_id = Some(DefId(999));
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    assert_eq!(
        index.check_equality_constraint_prototype(&tree, selection),
        Err(EqualityConstraintExposureError::OutputIsNotPredefinedReal)
    );
}

#[test]
fn equality_constraint_selection_cannot_substitute_foreign_slot_or_callable() {
    let mut tree = equality_constraint_tree(1);
    let (_, function) = equality_record_and_function(&tree);
    let foreign_slot = reidentified_equality_function(
        function.clone(),
        DefId(110),
        [DefId(111), DefId(112), DefId(113)],
        "equalityConstraint",
    );
    tree.definitions
        .classes
        .get_mut("B.R")
        .expect("foreign record fixture")
        .classes
        .insert("equalityConstraint".to_string(), foreign_slot.clone());
    let unrelated = reidentified_equality_function(
        function,
        DefId(120),
        [DefId(121), DefId(122), DefId(123)],
        "Unrelated",
    );
    tree.definitions
        .classes
        .insert("Unrelated".to_string(), unrelated.clone());

    let index = EqualityConstraintDeclarationIndex::new(&tree);
    let selection = index
        .prove_equality_constraint_selection(&tree, EQ_RECORD)
        .expect("direct selection is decidable")
        .expect("A.R exposes its own slot");
    assert_eq!(selection.slot_def_id(), EQ_FUNCTION);
    let foreign_record = &tree.definitions.classes["B.R"];
    let foreign_selection = index
        .prove_equality_constraint_selection(
            &tree,
            foreign_record.def_id.expect("foreign record identity"),
        )
        .expect("foreign selection is decidable")
        .expect("B.R exposes its own slot");
    assert_eq!(foreign_selection.slot_def_id(), DefId(110));
    let prototype = index
        .check_equality_constraint_prototype(&tree, selection)
        .expect("A.R's exact tree-owned declaration remains valid");
    let mut forged_clone = unrelated;
    forged_clone.def_id = Some(EQ_FUNCTION);
    forged_clone.components["residue"].shape = vec![999];
    forged_clone.components["residue"].shape_expr =
        vec![Subscript::Expression(equality_extent(999))];
    assert_eq!(forged_clone.components["residue"].shape, vec![999]);
    assert_eq!(
        index
            .check_equality_constraint_cardinality(&tree, prototype)
            .expect("caller-owned clone is not accepted by any constructor"),
        EqualityConstraintCardinality::NonEmpty(
            std::num::NonZeroUsize::new(1).expect("one is nonzero")
        )
    );
}

#[test]
fn equality_constraint_selection_derives_inherited_slot_and_rejects_bad_graphs() {
    let mut inherited_tree = equality_constraint_tree(1);
    let inherited_record = ClassDef {
        def_id: Some(DefId(129)),
        name: equality_token("Inherited", 129),
        class_type: ClassType::Record,
        extends: vec![crate::Extend {
            base_def_id: Some(EQ_RECORD),
            ..Default::default()
        }],
        ..Default::default()
    };
    inherited_tree
        .definitions
        .classes
        .insert("Inherited".to_string(), inherited_record.clone());
    let inherited_index = EqualityConstraintDeclarationIndex::new(&inherited_tree);
    let inherited_selection = inherited_index
        .prove_equality_constraint_selection(&inherited_tree, DefId(129))
        .expect("single-base inherited selection is decidable")
        .expect("the base exposes one exact inherited slot");
    assert_eq!(inherited_selection.slot_def_id(), EQ_FUNCTION);

    let mut cyclic_tree = equality_constraint_tree(1);
    let cyclic_record = cyclic_tree
        .definitions
        .classes
        .get_mut("A.R")
        .expect("cyclic record fixture");
    cyclic_record.classes.clear();
    cyclic_record.extends.push(crate::Extend {
        base_def_id: Some(EQ_RECORD),
        ..Default::default()
    });
    let cyclic_record = cyclic_record.clone();
    let cyclic_index = EqualityConstraintDeclarationIndex::new(&cyclic_tree);
    assert_eq!(
        cyclic_index.prove_equality_constraint_selection(
            &cyclic_tree,
            cyclic_record.def_id.expect("cyclic record identity"),
        ),
        Err(EqualityConstraintExposureError::CyclicRecordInheritance(
            EQ_RECORD
        ))
    );

    let mut ambiguous_tree = equality_constraint_tree(1);
    let (_, function) = equality_record_and_function(&ambiguous_tree);
    let foreign_slot = reidentified_equality_function(
        function,
        DefId(110),
        [DefId(111), DefId(112), DefId(113)],
        "equalityConstraint",
    );
    ambiguous_tree
        .definitions
        .classes
        .get_mut("B.R")
        .expect("foreign record fixture")
        .classes
        .insert("equalityConstraint".to_string(), foreign_slot);
    let derived = ClassDef {
        def_id: Some(DefId(130)),
        name: equality_token("Derived", 130),
        class_type: ClassType::Record,
        extends: vec![
            crate::Extend {
                base_def_id: Some(EQ_RECORD),
                ..Default::default()
            },
            crate::Extend {
                base_def_id: Some(EQ_OTHER_RECORD),
                ..Default::default()
            },
        ],
        ..Default::default()
    };
    ambiguous_tree
        .definitions
        .classes
        .insert("Derived".to_string(), derived.clone());
    let ambiguous_index = EqualityConstraintDeclarationIndex::new(&ambiguous_tree);
    assert_eq!(
        ambiguous_index.prove_equality_constraint_selection(
            &ambiguous_tree,
            derived.def_id.expect("derived record identity"),
        ),
        Err(EqualityConstraintExposureError::AmbiguousInheritedSlot)
    );
}

#[test]
fn equality_constraint_rejects_rank_identity_and_extent_mutations() {
    let mut tree = equality_constraint_tree(1);
    equality_function_mut(&mut tree).components["residue"]
        .shape_expr
        .push(Subscript::Expression(equality_extent(2)));
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    assert_eq!(
        index.check_equality_constraint_prototype(&tree, selection),
        Err(EqualityConstraintExposureError::OutputIsNotRankOne)
    );

    let mut tree = equality_constraint_tree(1);
    equality_function_mut(&mut tree).components["b"].def_id = Some(EQ_INPUT_A);
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    assert_eq!(
        index.check_equality_constraint_prototype(&tree, selection),
        Err(
            EqualityConstraintExposureError::NonUniqueDeclarationIdentity {
                def_id: EQ_INPUT_A,
                occurrences: 2,
            }
        )
    );

    let mut tree = equality_constraint_tree(1);
    equality_function_mut(&mut tree).components["residue"].shape = vec![2];
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    let prototype = index
        .check_equality_constraint_prototype(&tree, selection)
        .expect("base prototype is valid");
    assert_eq!(
        index.check_equality_constraint_cardinality(&tree, prototype),
        Err(EqualityConstraintExposureError::OutputExtentContradictsLiteral)
    );
}

#[test]
fn equality_constraint_literal_completion_rejects_unproved_or_invalid_extents() {
    let mut tree = equality_constraint_tree(1);
    equality_function_mut(&mut tree).components["residue"].shape_expr =
        vec![Subscript::Expression(equality_symbolic_extent("n"))];
    equality_function_mut(&mut tree).components["residue"]
        .shape
        .clear();
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    let prototype = index
        .check_equality_constraint_prototype(&tree, selection)
        .expect("symbolic rank-one extent remains a valid unresolved prototype");
    assert_eq!(
        index.check_equality_constraint_cardinality(&tree, prototype),
        Err(EqualityConstraintExposureError::SymbolicExtentCertificateNotImplemented)
    );

    let mut tree = equality_constraint_tree(1);
    equality_function_mut(&mut tree).components["residue"].shape_expr =
        vec![Subscript::Expression(equality_negative_extent("1"))];
    equality_function_mut(&mut tree).components["residue"]
        .shape
        .clear();
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    let prototype = index
        .check_equality_constraint_prototype(&tree, selection)
        .expect("negative rank-one extent is diagnosed during completion");
    assert_eq!(
        index.check_equality_constraint_cardinality(&tree, prototype),
        Err(EqualityConstraintExposureError::OutputExtentIsNegative(
            "-1".to_string()
        ))
    );

    let mut tree = equality_constraint_tree(1);
    let overflow = format!("{}0", usize::MAX);
    equality_function_mut(&mut tree).components["residue"].shape_expr =
        vec![Subscript::Expression(Expression::Terminal {
            terminal_type: crate::TerminalType::UnsignedInteger,
            token: equality_token(&overflow, 84),
            span: Span::from_offsets(
                rumoca_core::SourceId::from_source_name("instance_equality_constraint_test.mo"),
                84,
                85,
            ),
        })];
    equality_function_mut(&mut tree).components["residue"]
        .shape
        .clear();
    let (record, _) = equality_record_and_function(&tree);
    let (index, selection) = equality_selection(&tree, &record);
    let prototype = index
        .check_equality_constraint_prototype(&tree, selection)
        .expect("overflowing rank-one extent is diagnosed during completion");
    assert_eq!(
        index.check_equality_constraint_cardinality(&tree, prototype),
        Err(EqualityConstraintExposureError::OutputExtentExceedsUsize(
            overflow
        ))
    );
}

#[test]
fn occurrence_finalization_issues_keys_and_derives_exact_innermost_owners() {
    let tree = equality_constraint_tree(0);
    let index = EqualityConstraintDeclarationIndex::new(&tree);
    check_colliding_occurrence_rejected(&tree, &index);
    let (mut overlay, record_instance, primitive, foreign_record, foreign_primitive) =
        build_occurrence_overlay(&tree, &index);
    finalize_occurrence_overlay(
        &mut overlay,
        record_instance,
        primitive,
        foreign_record,
        foreign_primitive,
    );
}

fn check_colliding_occurrence_rejected(
    tree: &ClassTree,
    index: &EqualityConstraintDeclarationIndex,
) {
    let mut overlay = InstanceOverlay::new();
    let record = overlay.alloc_id();
    let mut overrides = ClassOverrideMap::default();
    overrides.insert(
        EQ_FUNCTION,
        ClassOverride::new("equalityConstraint", EQ_FUNCTION, EQ_OTHER_RECORD, None),
    );
    overlay
        .add_component(InstanceData {
            instance_id: record,
            declaration_def_id: Some(EQ_INPUT_A),
            qualified_name: QualifiedName::from_ident("collision"),
            type_def_id: Some(EQ_RECORD),
            class_overrides: overrides,
            ..Default::default()
        })
        .expect("colliding overlay fixture is allocated independently");
    assert_eq!(
        overlay.construct_and_register_equality_constraint_occurrence(index, tree, record),
        Err(EqualityConstraintOccurrenceError::InvalidExposure(
            EqualityConstraintExposureError::SelectedDeclarationIsNotFunction,
        )),
        "a colliding occurrence selection cannot publish a default-slot prototype",
    );
    assert_eq!(overlay.overconstrained_construction_counts(), (0, 0));
}

fn build_occurrence_overlay(
    tree: &ClassTree,
    index: &EqualityConstraintDeclarationIndex,
) -> (
    InstanceOverlay,
    InstanceId,
    InstanceId,
    InstanceId,
    InstanceId,
) {
    let mut overlay = InstanceOverlay::new();
    assert_eq!(
        overlay.finalized_overconstrained().err(),
        Some(EqualityConstraintOccurrenceError::OwnerCatalogNotFinalized),
        "an empty unfinished overlay is not semantic absence"
    );
    let record_instance = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: record_instance,
            declaration_def_id: Some(EQ_INPUT_A),
            qualified_name: QualifiedName::from_ident("r"),
            type_def_id: Some(EQ_RECORD),
            ..Default::default()
        })
        .expect("record occurrence is unique");
    let record_class = overlay.alloc_id();
    overlay
        .add_class(ClassInstanceData {
            instance_id: record_class,
            owner_component_id: Some(record_instance),
            class_def_id: Some(EQ_RECORD),
            qualified_name: QualifiedName::from_ident("r"),
            ..Default::default()
        })
        .expect("record class occurrence is unique");
    overlay
        .construct_and_register_equality_constraint_occurrence(index, tree, record_instance)
        .expect("Real[0] is proved and registered atomically")
        .expect("the fixture record exposes equalityConstraint");
    assert_eq!(
        overlay.construct_and_register_equality_constraint_occurrence(index, tree, record_instance),
        Err(EqualityConstraintOccurrenceError::DuplicateRecordExposure(
            record_instance,
        ))
    );
    let primitive = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: primitive,
            qualified_name: QualifiedName::from_ident("r.x"),
            owner_class_id: Some(record_class),
            is_primitive: true,
            ..Default::default()
        })
        .expect("primitive occurrence is unique");
    let foreign_record = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: foreign_record,
            declaration_def_id: Some(EQ_INPUT_A),
            qualified_name: QualifiedName::from_ident("other"),
            type_def_id: Some(EQ_RECORD),
            ..Default::default()
        })
        .expect("foreign record occurrence is unique");
    let foreign_class = overlay.alloc_id();
    overlay
        .add_class(ClassInstanceData {
            instance_id: foreign_class,
            owner_component_id: Some(foreign_record),
            class_def_id: Some(EQ_RECORD),
            qualified_name: QualifiedName::from_ident("other"),
            ..Default::default()
        })
        .expect("foreign record class occurrence is unique");
    let foreign_primitive = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: foreign_primitive,
            qualified_name: QualifiedName::from_ident("other.x"),
            owner_class_id: Some(foreign_class),
            is_primitive: true,
            ..Default::default()
        })
        .expect("foreign primitive occurrence is unique");
    overlay
        .construct_and_register_equality_constraint_occurrence(index, tree, foreign_record)
        .expect("same type foreign record gets its own atomic proof")
        .expect("the foreign fixture record exposes equalityConstraint");
    (
        overlay,
        record_instance,
        primitive,
        foreign_record,
        foreign_primitive,
    )
}

fn finalize_occurrence_overlay(
    overlay: &mut InstanceOverlay,
    record_instance: InstanceId,
    primitive: InstanceId,
    foreign_record: InstanceId,
    foreign_primitive: InstanceId,
) {
    overlay
        .finalize_overconstrained_record_owners()
        .expect("complete exact ancestry finalizes atomically");
    assert_eq!(
        overlay.finalized_overconstrained().err(),
        Some(EqualityConstraintOccurrenceError::EffectiveTypeCatalogNotFinalized),
        "owner ancestry alone cannot expose pre-typecheck semantics"
    );
    let nominal_record_type = TypeId::new(10);
    let nominal_real_type = TypeId::new(11);
    overlay
        .type_ids_by_def_id
        .insert(EQ_RECORD, nominal_record_type);
    overlay
        .type_roots
        .insert(nominal_record_type, nominal_record_type);
    overlay
        .type_roots
        .insert(nominal_real_type, nominal_real_type);
    for data in overlay.components.values_mut() {
        data.type_id = if data.type_def_id == Some(EQ_RECORD) {
            nominal_record_type
        } else {
            nominal_real_type
        };
    }
    overlay
        .finalize_effective_type_publication(test_semantic_catalog_projection())
        .expect("effective record identities finalize atomically");
    let finalized = overlay
        .finalized_overconstrained()
        .expect("both owner and type proofs were finalized");
    assert_eq!(finalized.record_owner(primitive), Some(record_instance));
    assert_eq!(
        finalized.record_owner(foreign_primitive),
        Some(foreign_record),
        "a same-type foreign primitive cannot be associated with the first record"
    );
    assert_eq!(
        overlay.finalize_overconstrained_record_owners(),
        Err(EqualityConstraintOccurrenceError::OwnerCatalogAlreadyFinalized)
    );
}

#[test]
fn test_from_dotted_simple() {
    let qn = QualifiedName::from_dotted("x.start");
    assert_eq!(qn.parts.len(), 2);
    assert_eq!(qn.parts[0].0, "x");
    assert_eq!(qn.parts[1].0, "start");
    assert_eq!(qn.to_flat_string(), "x.start");
}

#[test]
fn test_from_dotted_single() {
    let qn = QualifiedName::from_dotted("x");
    assert_eq!(qn.parts.len(), 1);
    assert_eq!(qn.parts[0].0, "x");
}

#[test]
fn test_from_dotted_empty() {
    let qn = QualifiedName::from_dotted("");
    assert!(qn.is_empty());
}

#[test]
fn test_from_dotted_trailing_dot() {
    // Trailing dots should be filtered out
    let qn = QualifiedName::from_dotted("x.y.");
    assert_eq!(qn.parts.len(), 2);
    assert_eq!(qn.to_flat_string(), "x.y");
}

#[test]
fn test_from_dotted_leading_dot() {
    // Leading dots should be filtered out
    let qn = QualifiedName::from_dotted(".x.y");
    assert_eq!(qn.parts.len(), 2);
    assert_eq!(qn.to_flat_string(), "x.y");
}

#[test]
fn test_from_dotted_consecutive_dots() {
    // Consecutive dots should be filtered out
    let qn = QualifiedName::from_dotted("x..y");
    assert_eq!(qn.parts.len(), 2);
    assert_eq!(qn.to_flat_string(), "x.y");
}

#[test]
fn test_starts_with_match() {
    let qn = QualifiedName::from_dotted("l2.x.start");
    assert!(qn.starts_with("l2"));
}

#[test]
fn test_starts_with_no_match() {
    let qn = QualifiedName::from_dotted("l2.x.start");
    assert!(!qn.starts_with("l1"));
    assert!(!qn.starts_with("x"));
}

#[test]
fn test_starts_with_empty() {
    let qn = QualifiedName::new();
    assert!(!qn.starts_with("anything"));
}

#[test]
fn test_strip_prefix_success() {
    let qn = QualifiedName::from_dotted("l2.x.start");
    let stripped = qn.strip_prefix("l2").unwrap();
    assert_eq!(stripped.to_flat_string(), "x.start");
}

#[test]
fn test_strip_prefix_no_match() {
    let qn = QualifiedName::from_dotted("l2.x.start");
    assert!(qn.strip_prefix("l1").is_none());
}

#[test]
fn test_strip_prefix_single_part() {
    // Cannot strip if only one part remains
    let qn = QualifiedName::from_dotted("x");
    assert!(qn.strip_prefix("x").is_none());
}

#[test]
fn test_strip_prefix_preserves_subscripts() {
    // Ensure subscripts on remaining parts are preserved
    let mut qn = QualifiedName::new();
    qn.push("comp".to_string(), vec![]);
    qn.push("array".to_string(), vec![1, 2]);
    qn.push("x".to_string(), vec![]);

    let stripped = qn.strip_prefix("comp").unwrap();
    assert_eq!(stripped.parts.len(), 2);
    assert_eq!(stripped.parts[0].0, "array");
    assert_eq!(stripped.parts[0].1, vec![1, 2]);
    assert_eq!(stripped.to_flat_string(), "array[1,2].x");
}

#[test]
fn test_to_component_path_preserves_structured_subscripts() {
    let mut qn = QualifiedName::new();
    qn.push("sys".to_string(), vec![]);
    qn.push("arr".to_string(), vec![1, 2]);
    qn.push("state".to_string(), vec![]);

    let path = qn.to_component_path();
    assert_eq!(
        path.parts(),
        &[
            "sys".to_string(),
            "arr[1,2]".to_string(),
            "state".to_string(),
        ]
    );
    assert_eq!(path.to_flat_string(), "sys.arr[1,2].state");
}

#[test]
fn test_starts_with_component_path_matches_structured_subscripts() {
    let mut qn = QualifiedName::new();
    qn.push("sys".to_string(), vec![]);
    qn.push("arr".to_string(), vec![1, 2]);
    qn.push("state".to_string(), vec![]);

    let prefix = ComponentPath::from_flat_path("sys.arr[1,2]");

    assert!(qn.starts_with_component_path(&prefix));
}

#[test]
fn test_starts_with_component_path_rejects_subscript_mismatch() {
    let mut qn = QualifiedName::new();
    qn.push("sys".to_string(), vec![]);
    qn.push("arr".to_string(), vec![1, 2]);
    qn.push("state".to_string(), vec![]);

    let prefix = ComponentPath::from_flat_path("sys.arr[1,3]");

    assert!(!qn.starts_with_component_path(&prefix));
}

#[test]
fn test_subscripted_part_match_uses_canonical_integer_text() {
    assert!(subscripted_part_matches_rendered("arr", &[0], "arr[0]"));
    assert!(subscripted_part_matches_rendered("arr", &[-2], "arr[-2]"));
    assert!(!subscripted_part_matches_rendered("arr", &[1], "arr[01]"));
    assert!(!subscripted_part_matches_rendered("arr", &[0], "arr[-0]"));
}

#[test]
fn test_first_name() {
    let qn = QualifiedName::from_dotted("a.b.c");
    assert_eq!(qn.first_name(), Some("a"));

    let empty = QualifiedName::new();
    assert_eq!(empty.first_name(), None);
}

#[test]
fn test_child() {
    let qn = QualifiedName::from_ident("x");
    let child = qn.child("start");
    assert_eq!(child.to_flat_string(), "x.start");
}

#[test]
fn test_parent_join_use_structured_parts() {
    let qn = QualifiedName::from_dotted("system.medium.nXi");
    assert_eq!(qn.parent().unwrap().to_flat_string(), "system.medium");
    assert_eq!(
        QualifiedName::from_dotted("system").join(&QualifiedName::from_dotted("medium.nXi")),
        qn
    );
}

#[test]
fn test_display_with_subscripts() {
    let mut qn = QualifiedName::new();
    qn.push("matrix".to_string(), vec![1, 2]);
    qn.push("element".to_string(), vec![]);
    assert_eq!(format!("{}", qn), "matrix[1,2].element");
}

/// Helper to create a distinguishable expression for testing.
/// Uses ComponentReference with a marker name to identify values.
fn test_expr(marker: &str) -> Expression {
    Expression::ComponentReference(ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        qualified_display_name: None,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: std::sync::Arc::from(marker),
                location: Location::default(),
                token_number: 0,
                token_type: 0,
            },
            subs: None,
            def_id: None,
        }],
    })
}

/// Check if an expression matches our test marker.
fn is_test_expr(expr: &Expression, marker: &str) -> bool {
    match expr {
        Expression::ComponentReference(cr) => {
            cr.parts.first().map(|p| &*p.ident.text) == Some(marker)
        }
        _ => false,
    }
}

#[test]
fn test_mod_env_add_and_get() {
    let mut env = ModificationEnvironment::new();
    let path = QualifiedName::from_dotted("x.start");
    let value = ModificationValue::simple(test_expr("value_1"));

    env.add(path.clone(), value);

    let retrieved = env.get(&path);
    assert!(retrieved.is_some());
    assert!(is_test_expr(&retrieved.unwrap().value, "value_1"));
}

#[test]
fn test_mod_env_outer_precedence() {
    // MLS §7.2.4: Outer modifications take precedence
    let mut env = ModificationEnvironment::new();
    let path = QualifiedName::from_dotted("x.start");

    // First add (simulating outer modification)
    let outer_value = ModificationValue::simple(test_expr("outer_10"));
    env.add(path.clone(), outer_value);

    // Second add (simulating inner modification) - should NOT overwrite
    let inner_value = ModificationValue::simple(test_expr("inner_5"));
    env.add(path.clone(), inner_value);

    // Should still have the outer value
    let retrieved = env.get(&path).unwrap();
    assert!(is_test_expr(&retrieved.value, "outer_10"));
}

#[test]
fn test_mod_env_get_attr() {
    let mut env = ModificationEnvironment::new();

    // Add x.start modification
    let path = QualifiedName::from_ident("x").child("start");
    let value = ModificationValue::simple(test_expr("start_42"));
    env.add(path, value);

    // Look up via get_attr
    let start = env.get_attr("x", "start");
    assert!(start.is_some());
    assert!(is_test_expr(start.unwrap(), "start_42"));

    // Non-existent attribute
    assert!(env.get_attr("x", "min").is_none());
    assert!(env.get_attr("y", "start").is_none());
}

#[test]
fn test_mod_env_remove_with_prefix() {
    let mut env = ModificationEnvironment::new();

    // Add modifications for different components
    env.add(
        QualifiedName::from_dotted("comp1.x.start"),
        ModificationValue::simple(test_expr("c1_x")),
    );
    env.add(
        QualifiedName::from_dotted("comp1.y.start"),
        ModificationValue::simple(test_expr("c1_y")),
    );
    env.add(
        QualifiedName::from_dotted("comp2.x.start"),
        ModificationValue::simple(test_expr("c2_x")),
    );

    assert_eq!(env.active.len(), 3);

    env.remove_with_prefix("comp1");

    assert_eq!(env.active.len(), 1);
    assert!(
        env.get(&QualifiedName::from_dotted("comp2.x.start"))
            .is_some()
    );
    assert!(
        env.get(&QualifiedName::from_dotted("comp1.x.start"))
            .is_none()
    );
}

#[test]
fn test_modification_value_simple() {
    let value = ModificationValue::simple(Expression::Empty {
        span: rumoca_core::Span::DUMMY,
    });
    assert!(!value.each);
    assert!(!value.final_);
    assert!(matches!(value.value, Expression::Empty { .. }));
}

#[test]
fn test_instance_overlay_component_lookup_by_instance_id() {
    let mut overlay = InstanceOverlay::new();
    let id_a = overlay.alloc_id();
    let id_b = overlay.alloc_id();

    overlay
        .add_component(InstanceData {
            instance_id: id_a,
            qualified_name: QualifiedName::from_dotted("a"),
            ..Default::default()
        })
        .expect("component a is unique");
    overlay
        .add_component(InstanceData {
            instance_id: id_b,
            qualified_name: QualifiedName::from_dotted("b"),
            ..Default::default()
        })
        .expect("component b is unique");

    let component_a = overlay
        .get_component(id_a)
        .expect("component for id_a should exist");
    let component_b = overlay
        .get_component(id_b)
        .expect("component for id_b should exist");

    assert_eq!(component_a.qualified_name.to_flat_string(), "a");
    assert_eq!(component_b.qualified_name.to_flat_string(), "b");
}

#[test]
fn instance_overlay_refuses_duplicate_component_and_preserves_record_type() {
    let mut overlay = InstanceOverlay::new();
    let instance = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: instance,
            qualified_name: QualifiedName::from_dotted("record"),
            type_def_id: Some(EQ_RECORD),
            ..Default::default()
        })
        .expect("first component occurrence is unique");

    assert_eq!(
        overlay.add_component(InstanceData {
            instance_id: instance,
            qualified_name: QualifiedName::from_dotted("forged"),
            type_def_id: Some(EQ_OTHER_RECORD),
            ..Default::default()
        }),
        Err(InstanceOverlayInsertError::DuplicateComponent(instance))
    );
    let preserved = overlay
        .get_component(instance)
        .expect("original component remains registered");
    assert_eq!(preserved.type_def_id, Some(EQ_RECORD));
    assert_eq!(preserved.qualified_name.to_flat_string(), "record");
}

#[test]
fn instance_overlay_refuses_duplicate_class_and_preserves_connection_sources() {
    let mut overlay = InstanceOverlay::new();
    let instance = overlay.alloc_id();
    let connection = InstanceConnection::scalar(
        QualifiedName::from_dotted("a"),
        QualifiedName::from_dotted("b"),
        None,
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("overlay_insert_test.mo"),
            1,
            2,
        ),
        String::new(),
    )
    .expect("fixture connection is valid");
    overlay
        .add_class(ClassInstanceData {
            instance_id: instance,
            qualified_name: QualifiedName::from_dotted("M"),
            connections: vec![connection.clone()],
            ..Default::default()
        })
        .expect("first class occurrence is unique");

    assert_eq!(
        overlay.add_class(ClassInstanceData {
            instance_id: instance,
            qualified_name: QualifiedName::from_dotted("forged"),
            connections: Vec::new(),
            ..Default::default()
        }),
        Err(InstanceOverlayInsertError::DuplicateClass(instance))
    );
    assert_eq!(overlay.classes[&instance].connections, vec![connection]);
}

#[test]
fn instance_overlay_checks_legal_shared_id_pair_and_rejects_mismatch() {
    let mut overlay = InstanceOverlay::new();
    let shared = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: shared,
            qualified_name: QualifiedName::from_dotted("record"),
            type_def_id: Some(EQ_RECORD),
            ..Default::default()
        })
        .expect("structured component is unique");
    overlay
        .add_class(ClassInstanceData {
            instance_id: shared,
            owner_component_id: Some(shared),
            qualified_name: QualifiedName::from_dotted("record"),
            class_def_id: Some(EQ_RECORD),
            ..Default::default()
        })
        .expect("matching structured component/class pair is legal");

    let mut overlay = InstanceOverlay::new();
    let mismatched = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: mismatched,
            qualified_name: QualifiedName::from_dotted("record"),
            type_def_id: Some(EQ_RECORD),
            ..Default::default()
        })
        .expect("structured component is unique");
    assert_eq!(
        overlay.add_class(ClassInstanceData {
            instance_id: mismatched,
            owner_component_id: Some(mismatched),
            qualified_name: QualifiedName::from_dotted("other"),
            class_def_id: Some(EQ_RECORD),
            ..Default::default()
        }),
        Err(InstanceOverlayInsertError::MismatchedComponentClassPair(
            mismatched
        ))
    );
    assert!(!overlay.classes.contains_key(&mismatched));

    let mut overlay = InstanceOverlay::new();
    let mismatched_type = overlay.alloc_id();
    overlay
        .add_component(InstanceData {
            instance_id: mismatched_type,
            qualified_name: QualifiedName::from_dotted("record"),
            type_def_id: Some(EQ_RECORD),
            ..Default::default()
        })
        .expect("structured component is unique");
    assert_eq!(
        overlay.add_class(ClassInstanceData {
            instance_id: mismatched_type,
            owner_component_id: Some(mismatched_type),
            qualified_name: QualifiedName::from_dotted("record"),
            class_def_id: Some(EQ_OTHER_RECORD),
            ..Default::default()
        }),
        Err(InstanceOverlayInsertError::MismatchedComponentClassPair(
            mismatched_type
        ))
    );
}

#[test]
fn instance_overlay_refuses_foreign_unallocated_occurrence_ids() {
    let mut overlay = InstanceOverlay::new();
    let allocated = overlay.alloc_id();
    let foreign = InstanceId::new(allocated.index() + 1);

    assert_eq!(
        overlay.add_component(InstanceData {
            instance_id: foreign,
            ..Default::default()
        }),
        Err(InstanceOverlayInsertError::UnallocatedComponentIdentity(
            foreign
        ))
    );
    assert_eq!(
        overlay.add_class(ClassInstanceData {
            instance_id: foreign,
            ..Default::default()
        }),
        Err(InstanceOverlayInsertError::UnallocatedClassIdentity(
            foreign
        ))
    );
}

#[test]
fn allocated_occurrence_identities_are_one_based_and_never_unset() {
    let mut overlay = InstanceOverlay::new();

    let first = overlay.alloc_id();
    let second = overlay.alloc_id();

    assert!(
        !first.is_unset(),
        "the reserved identity is not allocatable"
    );
    assert!(!second.is_unset());
    assert_eq!(first, InstanceId::new(1));
    assert_eq!(second, InstanceId::new(2));
    assert_eq!(overlay.allocated_instance_count(), 2);
}

#[test]
fn connection_wire_has_one_disjoint_scalar_or_family_payload() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("instance_connection_family.mo"),
        1,
        2,
    );
    let family = InstanceConnection::family(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 0,
                step: 1,
            }],
        },
        InstanceConnectionEndpoint::new(vec![(
            "a".to_string(),
            vec![rumoca_core::AffineForm::unit_binder(0, 1)],
        )])
        .expect("valid endpoint"),
        InstanceConnectionEndpoint::new(vec![(
            "b".to_string(),
            vec![rumoca_core::AffineForm::unit_binder(0, 1)],
        )])
        .expect("valid endpoint"),
        None,
        span,
        String::new(),
    )
    .expect("valid family");

    let encoded = serde_json::to_value(&family).expect("serialize family connection");
    assert_eq!(encoded["kind"], "family");
    assert!(encoded["connection"].get("domain").is_some());
    assert!(encoded.get("family").is_none());
    assert_eq!(
        serde_json::from_value::<InstanceConnection>(encoded.clone())
            .expect("round-trip family connection"),
        family
    );

    let mut contradictory = encoded;
    contradictory["kind"] = serde_json::Value::String("scalar".to_string());
    assert!(
        serde_json::from_value::<InstanceConnection>(contradictory).is_err(),
        "a family payload must not deserialize as a scalar connection"
    );

    let mut invalid_domain = serde_json::to_value(&family).expect("serialize valid family");
    invalid_domain["connection"]["domain"]["binders"][0]["step"] = serde_json::Value::from(0);
    assert!(serde_json::from_value::<InstanceConnection>(invalid_domain).is_err());

    let mut invalid_rank = serde_json::to_value(&family).expect("serialize valid family");
    invalid_rank["connection"]["a"]["parts"][0][1][0]["coeffs"] = serde_json::json!([]);
    assert!(serde_json::from_value::<InstanceConnection>(invalid_rank).is_err());

    let mut empty_endpoint = serde_json::to_value(&family).expect("serialize valid family");
    empty_endpoint["connection"]["a"]["parts"] = serde_json::json!([]);
    assert!(serde_json::from_value::<InstanceConnection>(empty_endpoint).is_err());

    let mut empty_part = serde_json::to_value(&family).expect("serialize valid family");
    empty_part["connection"]["b"]["parts"][0][0] = serde_json::json!("");
    assert!(serde_json::from_value::<InstanceConnection>(empty_part).is_err());

    let mut noncanonical_binder = serde_json::to_value(&family).expect("serialize valid family");
    noncanonical_binder["connection"]["domain"]["binders"][0]["id"] = serde_json::json!(1);
    assert!(serde_json::from_value::<InstanceConnection>(noncanonical_binder).is_err());

    let mut missing_span = serde_json::to_value(&family).expect("serialize valid family");
    missing_span["connection"]["span"] =
        serde_json::to_value(Span::DUMMY).expect("serialize dummy span");
    assert!(serde_json::from_value::<InstanceConnection>(missing_span).is_err());

    let mut positive_overflow = serde_json::to_value(&family).expect("serialize valid family");
    positive_overflow["connection"]["domain"]["binders"][0]["upper"] = serde_json::json!(1);
    positive_overflow["connection"]["a"]["parts"][0][1][0]["constant"] =
        serde_json::json!(i64::MAX);
    assert!(serde_json::from_value::<InstanceConnection>(positive_overflow).is_err());

    let mut negative_overflow = serde_json::to_value(&family).expect("serialize valid family");
    negative_overflow["connection"]["domain"]["binders"][0]["upper"] = serde_json::json!(1);
    negative_overflow["connection"]["a"]["parts"][0][1][0]["constant"] =
        serde_json::json!(i64::MIN);
    negative_overflow["connection"]["a"]["parts"][0][1][0]["coeffs"] = serde_json::json!([-1]);
    assert!(serde_json::from_value::<InstanceConnection>(negative_overflow).is_err());

    let mut empty_domain_large_form =
        serde_json::to_value(&family).expect("serialize valid family");
    empty_domain_large_form["connection"]["a"]["parts"][0][1][0]["constant"] =
        serde_json::json!(i64::MAX);
    assert!(
        serde_json::from_value::<InstanceConnection>(empty_domain_large_form).is_ok(),
        "an empty family has no scalar member whose affine value can overflow"
    );
}

#[test]
fn scalar_connection_wire_replays_checked_construction() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("instance_scalar_connection.mo"),
        1,
        2,
    );
    let scalar = InstanceConnection::scalar(
        QualifiedName::from_ident("a"),
        QualifiedName::from_ident("b"),
        None,
        span,
        String::new(),
    )
    .expect("valid scalar connection");
    let encoded = serde_json::to_value(&scalar).expect("serialize scalar connection");
    assert_eq!(
        serde_json::from_value::<InstanceConnection>(encoded.clone())
            .expect("checked scalar round trip"),
        scalar
    );

    for side in ["a", "b"] {
        let mut empty = encoded.clone();
        empty["connection"][side]["parts"] = serde_json::json!([]);
        assert!(
            serde_json::from_value::<InstanceConnection>(empty).is_err(),
            "wire must reject an empty {side} endpoint"
        );
    }

    let mut missing_span = encoded;
    missing_span["connection"]["span"] =
        serde_json::to_value(Span::DUMMY).expect("serialize dummy span");
    assert!(serde_json::from_value::<InstanceConnection>(missing_span).is_err());
}
