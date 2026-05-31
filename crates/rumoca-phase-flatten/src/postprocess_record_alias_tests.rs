use super::*;

fn var_ref(name: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

fn component_ref(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![],
        }],
        def_id: None,
    }
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
fn record_alias_canonicalization_visits_when_clauses_and_algorithms() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.port_a.p"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.port_a.p"),
            is_primitive: true,
            ..Default::default()
        },
    );
    let mut when_clause = flat::WhenClause::new(
        rumoca_core::Expression::Empty { span: Span::DUMMY },
        Span::DUMMY,
    );
    when_clause.add_equation(flat::WhenEquation::Assign {
        target: rumoca_core::VarName::new("y"),
        value: var_ref("pipe.flowModel.port_a.p"),
        span: Span::DUMMY,
        origin: "test".to_string(),
    });
    model.when_clauses.push(when_clause);
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

    let flat::WhenEquation::Assign { value, .. } = &model.when_clauses[0].equations[0] else {
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
            ..Default::default()
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
                        span: Span::DUMMY,
                    }],
                    span: Span::DUMMY,
                }),
                field: "missing".to_string(),
                span: Span::DUMMY,
            }),
            ..Default::default()
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
