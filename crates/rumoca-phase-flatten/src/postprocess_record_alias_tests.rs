use super::*;
use rumoca_core::Span;

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("postprocess_record_alias_test.mo"),
        1,
        2,
    )
}

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

fn component_ref_path(path: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: rumoca_core::ComponentPath::from_flat_path(path)
            .parts()
            .iter()
            .map(|ident| rumoca_core::ComponentRefPart {
                ident: ident.clone(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![],
            })
            .collect(),
        def_id: None,
    }
}

fn component_ref_with_def_id(
    path: &str,
    def_id: rumoca_core::DefId,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: rumoca_core::ComponentPath::from_flat_path(path)
            .parts()
            .iter()
            .map(|ident| rumoca_core::ComponentRefPart {
                ident: ident.clone(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![],
            })
            .collect(),
        def_id: Some(def_id),
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
fn def_id_canonicalization_rewrites_class_qualified_ref_by_owner_scope() {
    let mut model = flat::Model::new();
    let def_id = rumoca_core::DefId::new(42);
    for name in [
        "inertia1.rotorWith3DEffects.e",
        "inertia2.rotorWith3DEffects.e",
    ] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                component_ref: Some(component_ref_with_def_id(name, def_id)),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    model.add_variable(
        rumoca_core::VarName::new("inertia1.rotorWith3DEffects.cylinder.r_shape"),
        flat::Variable {
            name: rumoca_core::VarName::new("inertia1.rotorWith3DEffects.cylinder.r_shape"),
            binding: Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "Modelica.Mechanics.MultiBody.Parts.Rotor1D.e",
                    component_ref_with_def_id(
                        "Modelica.Mechanics.MultiBody.Parts.Rotor1D.e",
                        def_id,
                    ),
                ),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    canonicalize_varrefs_via_instantiated_def_ids(&mut model);

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new(
            "inertia1.rotorWith3DEffects.cylinder.r_shape",
        ))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected varref binding");
    };
    assert_eq!(name.as_str(), "inertia1.rotorWith3DEffects.e");
}

#[test]
fn def_id_canonicalization_resolves_descendant_from_component_equation_owner() {
    let mut model = flat::Model::new();
    for name in [
        "tank.medium.state.p",
        "tank.heatTransfer.states[1].p",
        "pump.medium.state.p",
    ] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                component_ref: Some(component_ref_path(name)),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "Modelica.Media.CompressibleLiquids.LinearWater_pT_Ambient.state.p",
                component_ref_path(
                    "Modelica.Media.CompressibleLiquids.LinearWater_pT_Ambient.state.p",
                ),
            ),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        rumoca_core::Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "tank".to_string(),
        },
    ));

    canonicalize_varrefs_via_instantiated_def_ids(&mut model);

    let rumoca_core::Expression::VarRef { name, .. } = &model.equations[0].residual else {
        panic!("expected varref residual");
    };
    assert_eq!(name.as_str(), "tank.medium.state.p");
}

#[test]
fn def_id_canonicalization_preserves_resolved_package_constant_refs() {
    let mut model = flat::Model::new();
    let instantiated_constant_def = rumoca_core::DefId::new(3651);
    let package_constant_def = rumoca_core::DefId::new(86);

    model.add_variable(
        rumoca_core::VarName::new("sine.pi"),
        flat::Variable {
            name: rumoca_core::VarName::new("sine.pi"),
            component_ref: Some(component_ref_with_def_id(
                "sine.pi",
                instantiated_constant_def,
            )),
            binding: Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "Modelica.Constants.pi",
                    component_ref_with_def_id("Modelica.Constants.pi", package_constant_def),
                ),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    canonicalize_varrefs_via_instantiated_def_ids(&mut model);

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("sine.pi"))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected varref binding");
    };
    assert_eq!(name.as_str(), "Modelica.Constants.pi");
    assert_eq!(name.target_def_id(), Some(package_constant_def));
}

#[test]
fn def_id_canonicalization_uses_symbol_ancestry_for_inherited_attribute_refs() {
    let mut model = flat::Model::new();
    let source_def = rumoca_core::DefId::new(27726);
    let valve_instance_def = rumoca_core::DefId::new(93015);
    let other_instance_def = rumoca_core::DefId::new(93016);
    for (name, instance_def) in [
        ("val.m_flow_nominal_pos", valve_instance_def),
        ("other.m_flow_nominal_pos", other_instance_def),
    ] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                component_ref: Some(component_ref_with_def_id(name, instance_def)),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        model.symbol_ancestry.insert(instance_def, vec![source_def]);
    }
    model.add_variable(
        rumoca_core::VarName::new("val.m_flow"),
        flat::Variable {
            name: rumoca_core::VarName::new("val.m_flow"),
            nominal: Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "Buildings.Fluid.BaseClasses.PartialResistance.m_flow_nominal_pos",
                    component_ref_with_def_id(
                        "Buildings.Fluid.BaseClasses.PartialResistance.m_flow_nominal_pos",
                        source_def,
                    ),
                ),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    canonicalize_varrefs_via_instantiated_def_ids(&mut model);

    let nominal = model
        .variables
        .get(&rumoca_core::VarName::new("val.m_flow"))
        .and_then(|var| var.nominal.as_ref())
        .expect("nominal should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = nominal else {
        panic!("expected nominal varref");
    };
    assert_eq!(name.as_str(), "val.m_flow_nominal_pos");
}

#[test]
fn def_id_canonicalization_resolves_inherited_bare_binding_to_owner_sibling() {
    let mut model = flat::Model::new();
    let source_def = rumoca_core::DefId::new(19372);
    let sibling_instance_def = rumoca_core::DefId::new(39979);
    model.add_variable(
        rumoca_core::VarName::new("material.B_rRef"),
        flat::Variable {
            name: rumoca_core::VarName::new("material.B_rRef"),
            component_ref: Some(component_ref_with_def_id(
                "material.B_rRef",
                sibling_instance_def,
            )),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model
        .symbol_ancestry
        .insert(sibling_instance_def, vec![source_def]);
    model.add_variable(
        rumoca_core::VarName::new("material.B_r"),
        flat::Variable {
            name: rumoca_core::VarName::new("material.B_r"),
            binding: Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "B_rRef",
                    component_ref_with_def_id("B_rRef", source_def),
                ),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    canonicalize_varrefs_via_instantiated_def_ids(&mut model);

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("material.B_r"))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected varref binding");
    };
    assert_eq!(name.as_str(), "material.B_rRef");
}

#[test]
fn def_id_canonicalization_prefers_owner_instance_before_enclosing_fallback() {
    let mut model = flat::Model::new();
    let source_def = rumoca_core::DefId::new(610);
    let nested_instance_def = rumoca_core::DefId::new(40065);
    let owner_instance_def = rumoca_core::DefId::new(40206);
    for (name, def_id) in [
        ("cell.cell.limIntegrator.y", nested_instance_def),
        ("cell.limIntegrator.y", owner_instance_def),
    ] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                component_ref: Some(component_ref_with_def_id(name, def_id)),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
        model.symbol_ancestry.insert(def_id, vec![source_def]);
    }
    model.equations.push(flat::Equation {
        residual: rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "y",
                component_ref_with_def_id("y", source_def),
            ),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: flat::EquationOrigin::ComponentEquation {
            component: "cell.limIntegrator".to_string(),
        },
        scalar_count: 1,
    });

    canonicalize_varrefs_via_instantiated_def_ids(&mut model);

    let rumoca_core::Expression::VarRef { name, .. } = &model.equations[0].residual else {
        panic!("expected varref residual");
    };
    assert_eq!(name.as_str(), "cell.limIntegrator.y");
}

#[test]
fn def_id_canonicalization_prefers_known_structured_path_over_rendered_name() {
    let mut model = flat::Model::new();
    let source_def = rumoca_core::DefId::new(621);
    let instance_def = rumoca_core::DefId::new(40206);
    model.add_variable(
        rumoca_core::VarName::new("cell.limIntegrator.local_reset"),
        flat::Variable {
            name: rumoca_core::VarName::new("cell.limIntegrator.local_reset"),
            component_ref: Some(component_ref_with_def_id(
                "cell.limIntegrator.local_reset",
                instance_def,
            )),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.symbol_ancestry.insert(instance_def, vec![source_def]);
    model.equations.push(flat::Equation {
        residual: rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "local_reset",
                component_ref_with_def_id("cell.limIntegrator.local_reset", source_def),
            ),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
        origin: flat::EquationOrigin::ComponentEquation {
            component: "cell.limIntegrator".to_string(),
        },
        scalar_count: 1,
    });

    canonicalize_varrefs_via_instantiated_def_ids(&mut model);

    let rumoca_core::Expression::VarRef { name, .. } = &model.equations[0].residual else {
        panic!("expected varref residual");
    };
    assert_eq!(name.as_str(), "cell.limIntegrator.local_reset");
}

#[test]
fn record_alias_canonicalization_visits_when_clauses_and_algorithms() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("pipe.port_a.p"),
        flat::Variable {
            name: rumoca_core::VarName::new("pipe.port_a.p"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
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
fn record_alias_canonicalization_visits_variable_bindings_and_projects_array_record_fields() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("bank.per[1].Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.per[1].Q"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_variable(
        rumoca_core::VarName::new("bank.ch[1].per.Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.ch[1].per.Q"),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("bank.ch[1].per")),
                field: "Q".to_string(),
                span: Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("bank.ch[1].per"),
        rumoca_core::ComponentPath::from_flat_path("bank.per"),
    );

    canonicalize_varrefs_via_record_aliases(&mut model, &ctx);

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("bank.ch[1].per.Q"))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected field access to collapse to varref");
    };
    assert_eq!(name.as_str(), "bank.per[1].Q");
}

#[test]
fn record_alias_canonicalization_uses_variable_owner_to_project_unindexed_record_binding() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("bank.per[1].Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.per[1].Q"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_variable(
        rumoca_core::VarName::new("bank.ch[1].per.Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.ch[1].per.Q"),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("bank.dat")),
                field: "Q".to_string(),
                span: Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("bank.dat"),
        rumoca_core::ComponentPath::from_flat_path("bank.per"),
    );

    canonicalize_varrefs_via_record_aliases(&mut model, &ctx);

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("bank.ch[1].per.Q"))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected field access to collapse to owner-indexed target");
    };
    assert_eq!(name.as_str(), "bank.per[1].Q");
}

#[test]
fn record_alias_canonicalization_projects_owner_record_field_without_direct_alias() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("bank.per[1].Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.per[1].Q"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_variable(
        rumoca_core::VarName::new("bank.ch[1].per.Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.ch[1].per.Q"),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("bank.dat")),
                field: "Q".to_string(),
                span: Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let ctx = Context::new();

    canonicalize_varrefs_via_record_aliases(&mut model, &ctx);

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("bank.ch[1].per.Q"))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected owner-projected field access to collapse");
    };
    assert_eq!(name.as_str(), "bank.per[1].Q");
}

#[test]
fn record_alias_canonicalization_projects_nested_owner_record_leaf() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("bank.per[1].Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.per[1].Q"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_variable(
        rumoca_core::VarName::new("bank.ch[1].unit.per.Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.ch[1].unit.per.Q"),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("bank.dat")),
                field: "Q".to_string(),
                span: Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let mut ctx = Context::new();
    ctx.record_aliases.insert(
        rumoca_core::ComponentPath::from_flat_path("other.alias"),
        rumoca_core::ComponentPath::from_flat_path("other.target"),
    );

    canonicalize_varrefs_via_record_aliases(&mut model, &ctx);

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("bank.ch[1].unit.per.Q"))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected nested owner projection to collapse");
    };
    assert_eq!(name.as_str(), "bank.per[1].Q");
}

#[test]
fn record_alias_canonicalization_projects_to_exact_indexed_owner_sibling() {
    let mut model = flat::Model::new();
    for name in [
        "bank.ch[1,2].per.Q",
        "bank.unrelated.per[1,2].Q",
        "bank.ch[1,2].unit.per.Q",
    ] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                binding: (name == "bank.ch[1,2].unit.per.Q").then(|| {
                    rumoca_core::Expression::FieldAccess {
                        base: Box::new(var_ref("bank.per")),
                        field: "Q".to_string(),
                        span: Span::DUMMY,
                    }
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    canonicalize_varrefs_via_record_aliases(&mut model, &Context::new());

    let binding = model
        .variables
        .get(&rumoca_core::VarName::new("bank.ch[1,2].unit.per.Q"))
        .and_then(|var| var.binding.as_ref())
        .expect("binding should remain present");
    let rumoca_core::Expression::VarRef { name, .. } = binding else {
        panic!("expected exact indexed owner sibling");
    };
    assert_eq!(name.as_str(), "bank.ch[1,2].per.Q");
}

#[test]
fn record_alias_canonicalization_does_not_project_to_unrelated_unique_descendant() {
    let mut model = flat::Model::new();
    model.add_variable(
        rumoca_core::VarName::new("bank.unrelated.per[1,2].Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.unrelated.per[1,2].Q"),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    model.add_variable(
        rumoca_core::VarName::new("bank.ch[1,2].unit.per.Q"),
        flat::Variable {
            name: rumoca_core::VarName::new("bank.ch[1,2].unit.per.Q"),
            binding: Some(rumoca_core::Expression::FieldAccess {
                base: Box::new(var_ref("bank.per")),
                field: "Q".to_string(),
                span: Span::DUMMY,
            }),
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    canonicalize_varrefs_via_record_aliases(&mut model, &Context::new());

    assert!(matches!(
        model
            .variables
            .get(&rumoca_core::VarName::new("bank.ch[1,2].unit.per.Q"))
            .and_then(|var| var.binding.as_ref()),
        Some(rumoca_core::Expression::FieldAccess { .. })
    ));
}

#[test]
fn record_alias_canonicalization_requires_field_to_match_owner_leaf() {
    let mut model = flat::Model::new();
    for name in ["bank.ch[1].per.Q", "bank.ch[1].unit.per.Q"] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                binding: (name == "bank.ch[1].unit.per.Q").then(|| {
                    rumoca_core::Expression::FieldAccess {
                        base: Box::new(var_ref("bank.per")),
                        field: "P".to_string(),
                        span: Span::DUMMY,
                    }
                }),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    canonicalize_varrefs_via_record_aliases(&mut model, &Context::new());

    assert!(matches!(
        model
            .variables
            .get(&rumoca_core::VarName::new("bank.ch[1].unit.per.Q"))
            .and_then(|var| var.binding.as_ref()),
        Some(rumoca_core::Expression::FieldAccess { field, .. }) if field == "P"
    ));
}

#[test]
fn record_alias_canonicalization_preserves_distinct_connector_record_endpoints() {
    let mut model = flat::Model::new();
    for (index, name) in [
        "device.i",
        "device.pin_p.i.re",
        "device.pin_p.i.im",
        "device.pin_n.i.re",
        "device.pin_n.i.im",
    ]
    .into_iter()
    .enumerate()
    {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                component_ref: Some(component_ref_with_def_id(
                    name,
                    rumoca_core::DefId::new(800 + index as u32),
                )),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let pin_p_def_id = rumoca_core::DefId::new(901);
    let pin_n_def_id = rumoca_core::DefId::new(902);
    let pin_p_ref = component_ref_with_def_id("device.pin_p.i", pin_p_def_id);
    let pin_n_ref = component_ref_with_def_id("device.pin_n.i", pin_n_def_id);
    model.add_equation(flat::Equation::new(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "device.pin_p.i",
                    pin_p_ref.clone(),
                ),
                subscripts: vec![],
                span: Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "device.pin_n.i",
                    pin_n_ref.clone(),
                ),
                subscripts: vec![],
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        },
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: "device".to_string(),
        },
    ));

    canonicalize_varrefs_via_record_aliases(&mut model, &Context::new());

    let rumoca_core::Expression::Binary { lhs, rhs, .. } = &model.equations[0].residual else {
        panic!("expected binary residual");
    };
    let rumoca_core::Expression::VarRef {
        name: pin_p_name, ..
    } = lhs.as_ref()
    else {
        panic!("expected pin_p aggregate reference");
    };
    let rumoca_core::Expression::VarRef {
        name: pin_n_name, ..
    } = rhs.as_ref()
    else {
        panic!("expected pin_n aggregate reference");
    };
    assert_eq!(
        [
            (pin_p_name.as_str(), pin_p_name.target_def_id()),
            (pin_n_name.as_str(), pin_n_name.target_def_id()),
        ],
        [
            ("device.pin_p.i", Some(pin_p_def_id)),
            ("device.pin_n.i", Some(pin_n_def_id)),
        ]
    );
    assert_eq!(pin_p_name.component_ref(), Some(&pin_p_ref));
    assert_eq!(pin_n_name.component_ref(), Some(&pin_n_ref));
}

#[test]
fn record_alias_canonicalization_preserves_decomposed_record_field_without_alias() {
    let mut model = flat::Model::new();
    for name in ["h", "state.phase"] {
        model.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                name: rumoca_core::VarName::new(name),
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    model.add_equation(flat::Equation::new(
        var_ref("state.h"),
        Span::DUMMY,
        flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
    ));

    canonicalize_varrefs_via_record_aliases(&mut model, &Context::new());

    let rumoca_core::Expression::VarRef { name, .. } = &model.equations[0].residual else {
        panic!("expected preserved var ref");
    };
    assert_eq!(name.as_str(), "state.h");
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
                        span: Span::DUMMY,
                    }],
                    span: Span::DUMMY,
                }),
                field: "missing".to_string(),
                span: Span::DUMMY,
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
