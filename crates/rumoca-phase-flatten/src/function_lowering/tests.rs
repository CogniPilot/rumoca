use super::*;
use rumoca_core::{ClassType, Literal, Span, VarName};

const RECORD_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7001);
const FIELD_A_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7010);
const FIELD_TYPE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7011);
const OUTPUT_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7020);
const STATE_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7033);
const STATE_FIELD_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7034);
const STATE_RECORD_DEF_ID: rumoca_core::DefId = rumoca_core::DefId(7035);

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_lowering_test.mo"),
        1,
        2,
    )
}

fn assignment_to(
    name: &str,
    def_id: rumoca_core::DefId,
    value: rumoca_core::Expression,
) -> rumoca_core::Statement {
    rumoca_core::Statement::Assignment {
        comp: rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id,
            }],
        )
        .expect("assignment target is resolved"),
        value,
        span: test_span(),
    }
}

#[test]
fn record_field_normalization_preserves_root_identity_span_and_locality() {
    let root_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_lowering_test.mo"),
        7,
        12,
    );
    let reference_span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_lowering_test.mo"),
        7,
        14,
    );
    let root = rumoca_core::ComponentRefPart {
        ident: "state".to_string(),
        span: root_span,
        subs: Vec::new(),
        def_id: STATE_DEF_ID,
    };
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(60_001), test_span());
    function.add_input(
        crate::test_support::aggregate_param("state", "Pkg.State", Vec::new(), test_span())
            .with_def_id(STATE_DEF_ID)
            .with_type_def_id(STATE_RECORD_DEF_ID)
            .with_type_class(ClassType::Record),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    let reference = rumoca_core::ComponentReference::construct(
        true,
        reference_span,
        vec![
            root.clone(),
            rumoca_core::ComponentRefPart {
                ident: "x".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: STATE_FIELD_DEF_ID,
            },
        ],
    )
    .expect("record field reference is resolved");
    function.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(reference),
            subscripts: Vec::new(),
            span: reference_span,
        },
    ));

    rewrite_record_field_access_in_body(&mut function, &record_syntax_model())
        .expect("exact field syntax normalizes");

    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("expected assignment");
    };
    let rumoca_core::Expression::FieldAccess {
        base,
        field,
        field_def_id,
        ..
    } = value
    else {
        panic!("expected aggregate field access, got {value:?}");
    };
    assert_eq!(field, "x");
    assert_eq!(*field_def_id, STATE_FIELD_DEF_ID);
    let rumoca_core::Expression::VarRef { name, span, .. } = base.as_ref() else {
        panic!("expected aggregate record base");
    };
    let base = name
        .component_ref()
        .expect("base retains structured identity");
    assert!(base.local());
    assert_eq!(base.span(), reference_span);
    assert_eq!(base.parts(), std::slice::from_ref(&root));
    assert_eq!(*span, root_span);
    assert!(!name.is_generated());
}

#[test]
fn complete_record_field_defaults_seed_one_aggregate_function_local() {
    let mut flat = flat::Model::new();
    let mut constructor = rumoca_core::Function::new("Pkg.Record", RECORD_DEF_ID, test_span());
    constructor.def_id = Some(RECORD_DEF_ID);
    constructor.is_constructor = true;
    constructor.add_input(
        crate::test_support::real_param("a", Vec::new(), test_span())
            .with_def_id(FIELD_A_DEF_ID)
            .with_type_def_id(FIELD_TYPE_DEF_ID)
            .with_default(rumoca_core::Expression::Literal {
                value: Literal::Real(1.0),
                span: test_span(),
            }),
    );
    install_constructor_layout(&mut flat, &constructor);
    flat.add_function(constructor);
    let constructor_instance = flat.functions[&VarName::new("Pkg.Record")]
        .instance_id
        .expect("constructor instance");

    let mut function =
        rumoca_core::Function::new("Pkg.useLocal", rumoca_core::DefId::new(60_002), test_span());
    function.locals.push(
        crate::test_support::aggregate_param("localRecord", "Pkg.Record", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    flat.add_function(function);

    materialize_complete_record_value_defaults(&mut flat)
        .expect("the exact constructor owns complete defaults");

    let default = flat.functions[&VarName::new("Pkg.useLocal")].locals[0]
        .default
        .as_ref()
        .expect("complete field defaults seed the record local");
    assert!(matches!(
        default,
        rumoca_core::Expression::FunctionCall { name, args, is_constructor: true, .. }
            if args.is_empty()
                && name.resolved_function().map(|resolved| resolved.instance_id)
                    == Some(constructor_instance)
    ));
}

#[test]
fn incomplete_record_field_defaults_do_not_invent_a_value() {
    let mut flat = flat::Model::new();
    let mut constructor = rumoca_core::Function::new("Pkg.Record", RECORD_DEF_ID, test_span());
    constructor.def_id = Some(RECORD_DEF_ID);
    constructor.is_constructor = true;
    constructor.add_input(
        crate::test_support::real_param("a", Vec::new(), test_span())
            .with_def_id(FIELD_A_DEF_ID)
            .with_type_def_id(FIELD_TYPE_DEF_ID),
    );
    install_constructor_layout(&mut flat, &constructor);
    flat.add_function(constructor);
    let mut function =
        rumoca_core::Function::new("Pkg.useLocal", rumoca_core::DefId::new(60_003), test_span());
    function.locals.push(
        crate::test_support::aggregate_param("localRecord", "Pkg.Record", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    flat.add_function(function);

    materialize_complete_record_value_defaults(&mut flat)
        .expect("incomplete field defaults are a checked absence");

    assert!(
        flat.functions[&VarName::new("Pkg.useLocal")].locals[0]
            .default
            .is_none()
    );
}

#[test]
fn record_field_normalization_refuses_missing_exact_field_evidence() {
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(60_004), test_span());
    function.add_input(
        crate::test_support::aggregate_param("state", "Pkg.State", Vec::new(), test_span())
            .with_def_id(STATE_DEF_ID)
            .with_type_def_id(STATE_RECORD_DEF_ID)
            .with_type_class(ClassType::Record),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    let reference = rumoca_core::ComponentReference::construct(
        true,
        test_span(),
        vec![
            rumoca_core::ComponentRefPart {
                ident: "state".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: STATE_DEF_ID,
            },
            rumoca_core::ComponentRefPart {
                ident: "x".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: rumoca_core::DefId::new(999_999),
            },
        ],
    )
    .expect("the foreign semantic field identity remains structurally representable");
    function.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(reference),
            subscripts: Vec::new(),
            span: test_span(),
        },
    ));

    let before = function.body.clone();
    let error = rewrite_record_field_access_in_body(&mut function, &record_syntax_model())
        .expect_err("an exact record formal cannot silently keep an unidentified field");
    assert!(error.to_string().contains("field identity"), "{error}");
    assert_eq!(function.body, before, "failed normalization is atomic");
}

#[test]
fn same_spelling_nonmatching_root_is_not_rewritten_as_the_record_formal() {
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(60_005), test_span());
    function.add_input(
        crate::test_support::aggregate_param("state", "Pkg.State", Vec::new(), test_span())
            .with_def_id(STATE_DEF_ID)
            .with_type_def_id(STATE_RECORD_DEF_ID)
            .with_type_class(ClassType::Record),
    );
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(OUTPUT_DEF_ID),
    );
    let alias_def_id = rumoca_core::DefId::new(STATE_DEF_ID.index() + 100);
    let reference = rumoca_core::ComponentReference::construct(
        true,
        test_span(),
        vec![
            rumoca_core::ComponentRefPart {
                ident: "state".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: alias_def_id,
            },
            rumoca_core::ComponentRefPart {
                ident: "x".to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: STATE_FIELD_DEF_ID,
            },
        ],
    )
    .unwrap();
    function.body.push(assignment_to(
        "y",
        OUTPUT_DEF_ID,
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::from_component_reference(reference),
            subscripts: Vec::new(),
            span: test_span(),
        },
    ));

    rewrite_record_field_access_in_body(&mut function, &record_syntax_model())
        .expect("a different declaration with colliding spelling is a genuine nonmatch");
    let rumoca_core::Statement::Assignment { value, .. } = &function.body[0] else {
        panic!("fixture keeps one assignment");
    };
    assert!(matches!(value, rumoca_core::Expression::VarRef { .. }));
}

#[test]
fn record_default_materialization_rejects_missing_constructor_evidence_atomically() {
    let mut flat = flat::Model::new();
    install_record_type_evidence(&mut flat, RECORD_DEF_ID, "Pkg.Record", &[]);
    let mut function =
        rumoca_core::Function::new("Pkg.useLocal", rumoca_core::DefId::new(60_006), test_span());
    function.locals.push(
        crate::test_support::aggregate_param("localRecord", "Pkg.Record", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID),
    );
    flat.add_function(function);
    let before = flat.functions[&VarName::new("Pkg.useLocal")].locals[0]
        .default
        .clone();
    let error = materialize_complete_record_value_defaults(&mut flat)
        .expect_err("a record local cannot silently skip an absent constructor");
    assert!(
        error.to_string().contains("constructor identity"),
        "{error}"
    );
    assert_eq!(
        flat.functions[&VarName::new("Pkg.useLocal")].locals[0].default,
        before
    );
}

#[test]
fn record_default_materialization_rejects_wrong_effective_identity_atomically() {
    let mut flat = flat::Model::new();
    let mut constructor = rumoca_core::Function::new("Pkg.Record", RECORD_DEF_ID, test_span());
    constructor.def_id = Some(RECORD_DEF_ID);
    constructor.is_constructor = true;
    constructor.add_input(
        crate::test_support::real_param("a", Vec::new(), test_span())
            .with_def_id(FIELD_A_DEF_ID)
            .with_type_def_id(FIELD_TYPE_DEF_ID)
            .with_default(rumoca_core::Expression::Literal {
                value: Literal::Real(1.0),
                span: test_span(),
            }),
    );
    install_constructor_layout(&mut flat, &constructor);
    flat.add_function(constructor);
    let mut function =
        rumoca_core::Function::new("Pkg.useLocal", rumoca_core::DefId::new(60_007), test_span());
    let mut valid =
        crate::test_support::aggregate_param("validRecord", "Pkg.Record", Vec::new(), test_span())
            .with_type_class(ClassType::Record)
            .with_type_def_id(RECORD_DEF_ID);
    let mut invalid = valid.clone();
    invalid.name = "invalidRecord".to_string();
    invalid.effective_type =
        crate::test_support::real_param("wrong", Vec::new(), test_span()).effective_type;
    valid.default = None;
    function.locals.extend([valid, invalid]);
    flat.add_function(function);
    let before = flat.functions[&VarName::new("Pkg.useLocal")]
        .locals
        .iter()
        .map(|value| value.default.clone())
        .collect::<Vec<_>>();

    let error = materialize_complete_record_value_defaults(&mut flat)
        .expect_err("one contradictory record value prevents every default installation");
    assert!(error.to_string().contains("exact type identity"), "{error}");
    assert_eq!(
        flat.functions[&VarName::new("Pkg.useLocal")]
            .locals
            .iter()
            .map(|value| value.default.clone())
            .collect::<Vec<_>>(),
        before,
    );
}

fn install_constructor_layout(flat: &mut flat::Model, constructor: &rumoca_core::Function) {
    let fields = constructor
        .inputs
        .iter()
        .map(|input| flat::RecordField {
            name: input.name.clone(),
            def_id: input.def_id.expect("field declaration"),
            type_def_id: input.type_def_id.expect("field type declaration"),
            effective_type: input.effective_type.clone(),
            dims: input.dimensions().to_vec(),
        })
        .collect::<Vec<_>>();
    install_record_type_evidence(flat, RECORD_DEF_ID, constructor.name.as_str(), &fields);
}

fn install_record_type_evidence(
    flat: &mut flat::Model,
    declaration: rumoca_core::DefId,
    name: &str,
    fields: &[flat::RecordField],
) {
    let record_type = crate::test_support::aggregate_param("record", name, Vec::new(), test_span())
        .effective_type;
    flat.type_ids_by_def_id
        .insert(declaration, record_type.nominal_type());
    flat.type_roots
        .insert(record_type.nominal_type(), record_type.canonical_type());
    flat.type_roots
        .insert(record_type.canonical_type(), record_type.canonical_type());
    for field in fields {
        flat.type_ids_by_def_id
            .insert(field.type_def_id, field.effective_type.nominal_type());
        flat.type_roots.insert(
            field.effective_type.nominal_type(),
            field.effective_type.canonical_type(),
        );
        flat.type_roots.insert(
            field.effective_type.canonical_type(),
            field.effective_type.canonical_type(),
        );
    }
    flat.record_types.insert(
        declaration,
        flat::RecordType {
            name: name.to_string(),
            fields: fields.to_vec(),
        },
    );
}

fn record_syntax_model() -> flat::Model {
    let mut flat = flat::Model::new();
    let field = crate::test_support::real_param("x", Vec::new(), test_span())
        .with_def_id(STATE_FIELD_DEF_ID)
        .with_type_def_id(FIELD_TYPE_DEF_ID);
    install_record_type_evidence(
        &mut flat,
        STATE_RECORD_DEF_ID,
        "Pkg.State",
        &[flat::RecordField {
            name: field.name,
            def_id: field.def_id.unwrap(),
            type_def_id: field.type_def_id.unwrap(),
            effective_type: field.effective_type,
            dims: Vec::new(),
        }],
    );
    flat
}
