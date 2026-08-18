use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("postprocess_record_alias_test.mo"),
        1,
        2,
    )
}

fn fixture_def_id(name: &str) -> rumoca_core::DefId {
    let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(hash.max(1))
}

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::from_component_reference(component_ref_path(name)),
        subscripts: vec![],
        span: test_span(),
    }
}

fn component_ref(name: &str) -> rumoca_core::ComponentReference {
    component_ref_path(name)
}

fn reference_parts(path: &str) -> Vec<rumoca_core::ComponentRefPart> {
    rumoca_core::ComponentPath::from_flat_path(path)
        .parts()
        .iter()
        .map(|ident| rumoca_core::ComponentRefPart {
            ident: ident.clone(),
            span: test_span(),
            subs: vec![],
            def_id: fixture_def_id(ident),
        })
        .collect()
}

fn component_ref_path(path: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(false, test_span(), reference_parts(path))
        .expect("fixture reference has an exact identity for every part")
}

fn context_with_alias() -> Context {
    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("pipe.flowModel"),
        rumoca_core::ComponentPath::from_flat_path("pipe"),
    );
    ctx
}

#[test]
fn record_alias_canonicalization_visits_when_chains_and_algorithms() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.port_a.p"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.port_a.p"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut when_branch = flat::WhenBranch::new(
        rumoca_core::Expression::Empty { span: Span::DUMMY },
        Span::DUMMY,
    );
    when_branch.add_equation(flat::WhenEquation::Assign {
        target: rumoca_core::VarName::new("y"),
        value: var_ref("pipe.flowModel.port_a.p"),
        span: Span::DUMMY,
        origin: "test".to_string(),
    });
    let when_chain = flat::WhenChain::new(when_branch, Span::DUMMY);
    model.when_chains.push(when_chain);
    model.algorithms.push(flat::Algorithm::new(
        vec![rumoca_core::Statement::Assignment {
            comp: component_ref("y"),
            value: var_ref("pipe.flowModel.port_a.p"),
            span: Span::DUMMY,
        }],
        Span::DUMMY,
        "test",
    ));

    canonicalize_varrefs_via_record_aliases(&mut model, &context_with_alias());

    let flat::WhenEquation::Assign { value, .. } = &model.when_chains[0].first().equations[0]
    else {
        panic!("expected when assignment");
    };
    let rumoca_core::Expression::VarRef { name, .. } = value else {
        panic!("expected when var ref");
    };
    assert_eq!(name.as_str(), "pipe.port_a.p");

    let rumoca_core::Statement::Assignment { value, .. } = &model.algorithms[0].statements[0]
    else {
        panic!("expected algorithm assignment");
    };
    let rumoca_core::Expression::VarRef { name, .. } = value else {
        panic!("expected algorithm var ref");
    };
    assert_eq!(name.as_str(), "pipe.port_a.p");
}

#[test]
fn invalid_field_access_drop_handles_indexed_bases() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("someArray[1].existing"),
        flat::Variable {
            name: rumoca_core::VarName::new("someArray[1].existing"),
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_variable(
        rumoca_core::VarName::new("y"),
        flat::Variable {
            name: rumoca_core::VarName::new("y"),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(rumoca_core::Expression::Index {
                    base: Box::new(var_ref("someArray")),
                    subscripts: vec![rumoca_core::Subscript::Index {
                        value: 1,
                        span: test_span(),
                    }],
                    span: test_span(),
                }),
                field: "missing".to_string(),
                field_def_id: fixture_def_id("missing"),
                span: test_span(),
            }),
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    drop_invalid_field_access_bindings(&mut model);

    assert!(
        model
            .variables
            .get(&rumoca_core::VarName::new("y"))
            .and_then(|var| var.binding.as_ref())
            .is_none()
    );
}
