use super::*;
use std::sync::Arc;

fn scope_key(name: &str) -> rumoca_core::ComponentPath {
    rumoca_core::ComponentPath::from_flat_path(name)
}

fn make_class_def(
    name: &str,
    class_type: rumoca_core::ClassType,
    def_id: rumoca_core::DefId,
    scope_id: rumoca_core::ScopeId,
) -> ast::ClassDef {
    ast::ClassDef {
        def_id: Some(def_id),
        scope_id: Some(scope_id),
        name: Token {
            text: Arc::from(name.to_string()),
            ..Default::default()
        },
        class_type,
        ..Default::default()
    }
}

fn make_comp_ref(names: &[&str]) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: names
            .iter()
            .map(|n| ComponentRefPart {
                ident: Token {
                    text: std::sync::Arc::from(*n),
                    ..Default::default()
                },
                subs: None,
            })
            .collect(),
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_name(parts: &[&str]) -> ast::Name {
    ast::Name {
        name: parts
            .iter()
            .map(|part| Token {
                text: Arc::from(*part),
                ..Default::default()
            })
            .collect(),
        def_id: None,
    }
}

#[test]
fn test_source_scope_imports_include_current_class_imports() {
    let (tree, source_scope) = source_scope_import_tree();
    let mut imports = ImportMap::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    collect_imports_for_source_scope(&class_index, &source_scope, &mut imports);

    assert_eq!(
        imports.get("pi").map(String::as_str),
        Some("Modelica.Constants.pi")
    );
    assert_eq!(
        imports.get("eps").map(String::as_str),
        Some("Modelica.Constants.eps")
    );
}

fn source_scope_import_tree() -> (ast::ClassTree, QualifiedName) {
    let mut tree = ast::ClassTree::new();
    let modelica_id = rumoca_core::DefId::new(1);
    let constants_id = rumoca_core::DefId::new(2);
    let electrical_id = rumoca_core::DefId::new(3);
    let machines_id = rumoca_core::DefId::new(4);
    let space_phasors_id = rumoca_core::DefId::new(5);
    let components_id = rumoca_core::DefId::new(6);
    let space_phasor_id = rumoca_core::DefId::new(7);

    let modelica_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ast::ScopeKind::Package);
    let constants_scope = tree
        .scope_tree
        .create_scope(modelica_scope, ast::ScopeKind::Package);
    let electrical_scope = tree
        .scope_tree
        .create_scope(modelica_scope, ast::ScopeKind::Package);
    let machines_scope = tree
        .scope_tree
        .create_scope(electrical_scope, ast::ScopeKind::Package);
    let space_phasors_scope = tree
        .scope_tree
        .create_scope(machines_scope, ast::ScopeKind::Package);
    let components_scope = tree
        .scope_tree
        .create_scope(space_phasors_scope, ast::ScopeKind::Package);
    let space_phasor_scope = tree
        .scope_tree
        .create_scope(components_scope, ast::ScopeKind::Class);

    let modelica = source_scope_modelica_class(SourceScopeClassIds {
        modelica_id,
        constants_id,
        electrical_id,
        machines_id,
        space_phasors_id,
        components_id,
        space_phasor_id,
        modelica_scope,
        constants_scope,
        electrical_scope,
        machines_scope,
        space_phasors_scope,
        components_scope,
        space_phasor_scope,
    });
    tree.definitions.classes.insert("Modelica".into(), modelica);

    register_source_scope_import_names(
        &mut tree,
        [
            (modelica_id, "Modelica"),
            (constants_id, "Modelica.Constants"),
            (electrical_id, "Modelica.Electrical"),
            (machines_id, "Modelica.Electrical.Machines"),
            (
                space_phasors_id,
                "Modelica.Electrical.Machines.SpacePhasors",
            ),
            (
                components_id,
                "Modelica.Electrical.Machines.SpacePhasors.Components",
            ),
            (
                space_phasor_id,
                "Modelica.Electrical.Machines.SpacePhasors.Components.SpacePhasor",
            ),
        ],
    );

    let source_scope = QualifiedName {
        parts: vec![
            ("Modelica".to_string(), Vec::new()),
            ("Electrical".to_string(), Vec::new()),
            ("Machines".to_string(), Vec::new()),
            ("SpacePhasors".to_string(), Vec::new()),
            ("Components".to_string(), Vec::new()),
            ("SpacePhasor".to_string(), Vec::new()),
        ],
    };
    (tree, source_scope)
}

struct SourceScopeClassIds {
    modelica_id: rumoca_core::DefId,
    constants_id: rumoca_core::DefId,
    electrical_id: rumoca_core::DefId,
    machines_id: rumoca_core::DefId,
    space_phasors_id: rumoca_core::DefId,
    components_id: rumoca_core::DefId,
    space_phasor_id: rumoca_core::DefId,
    modelica_scope: rumoca_core::ScopeId,
    constants_scope: rumoca_core::ScopeId,
    electrical_scope: rumoca_core::ScopeId,
    machines_scope: rumoca_core::ScopeId,
    space_phasors_scope: rumoca_core::ScopeId,
    components_scope: rumoca_core::ScopeId,
    space_phasor_scope: rumoca_core::ScopeId,
}

fn source_scope_modelica_class(ids: SourceScopeClassIds) -> ast::ClassDef {
    let mut space_phasor = make_class_def(
        "SpacePhasor",
        rumoca_core::ClassType::Model,
        ids.space_phasor_id,
        ids.space_phasor_scope,
    );
    space_phasor
        .imports
        .push(qualified_import(&["Modelica", "Constants", "pi"]));
    let mut components = make_class_def(
        "Components",
        rumoca_core::ClassType::Package,
        ids.components_id,
        ids.components_scope,
    );
    components
        .imports
        .push(qualified_import(&["Modelica", "Constants", "eps"]));
    components
        .classes
        .insert("SpacePhasor".into(), space_phasor);
    let mut space_phasors = make_class_def(
        "SpacePhasors",
        rumoca_core::ClassType::Package,
        ids.space_phasors_id,
        ids.space_phasors_scope,
    );
    space_phasors
        .classes
        .insert("Components".into(), components);
    let mut machines = make_class_def(
        "Machines",
        rumoca_core::ClassType::Package,
        ids.machines_id,
        ids.machines_scope,
    );
    machines
        .classes
        .insert("SpacePhasors".into(), space_phasors);
    let constants = make_class_def(
        "Constants",
        rumoca_core::ClassType::Package,
        ids.constants_id,
        ids.constants_scope,
    );
    let mut electrical = make_class_def(
        "Electrical",
        rumoca_core::ClassType::Package,
        ids.electrical_id,
        ids.electrical_scope,
    );
    electrical.classes.insert("Machines".into(), machines);
    let mut modelica = make_class_def(
        "Modelica",
        rumoca_core::ClassType::Package,
        ids.modelica_id,
        ids.modelica_scope,
    );
    modelica.classes.insert("Constants".into(), constants);
    modelica.classes.insert("Electrical".into(), electrical);
    modelica
}

fn qualified_import(path: &[&str]) -> ast::Import {
    ast::Import::Qualified {
        path: make_name(path),
        location: rumoca_core::Location::default(),
        global_scope: false,
    }
}

fn register_source_scope_import_names(
    tree: &mut ast::ClassTree,
    names: [(rumoca_core::DefId, &str); 7],
) {
    for (def_id, name) in names {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }
}

fn make_int(value: i64) -> Expression {
    Expression::Terminal {
        terminal_type: TerminalType::UnsignedInteger,
        token: Token {
            text: Arc::from(value.to_string()),
            ..Default::default()
        },
        span: rumoca_core::Span::DUMMY,
    }
}

fn make_for_index(name: &str, range: Expression) -> ForIndex {
    ForIndex {
        ident: Token {
            text: Arc::from(name.to_string()),
            ..Default::default()
        },
        range,
    }
}

#[test]
fn test_lexical_package_alias_does_not_shadow_local_package() {
    let a_id = rumoca_core::DefId::new(1);
    let ancestor_b_id = rumoca_core::DefId::new(2);
    let c_id = rumoca_core::DefId::new(3);
    let m_id = rumoca_core::DefId::new(4);
    let local_b_id = rumoca_core::DefId::new(5);

    let mut tree = ast::ClassTree::new();
    let a_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ast::ScopeKind::Package);
    let ancestor_b_scope = tree
        .scope_tree
        .create_scope(a_scope, ast::ScopeKind::Package);
    let c_scope = tree
        .scope_tree
        .create_scope(a_scope, ast::ScopeKind::Package);
    let m_scope = tree.scope_tree.create_scope(c_scope, ast::ScopeKind::Class);
    let local_b_scope = tree
        .scope_tree
        .create_scope(m_scope, ast::ScopeKind::Package);

    tree.scope_tree
        .add_member(tree.scope_tree.global(), scope_key("A"), a_id);
    tree.scope_tree
        .add_member(a_scope, scope_key("B"), ancestor_b_id);
    tree.scope_tree.add_member(a_scope, scope_key("C"), c_id);
    tree.scope_tree.add_member(c_scope, scope_key("M"), m_id);
    tree.scope_tree
        .add_member(m_scope, scope_key("B"), local_b_id);

    let ancestor_b = make_class_def(
        "B",
        rumoca_core::ClassType::Package,
        ancestor_b_id,
        ancestor_b_scope,
    );
    let local_b = make_class_def(
        "B",
        rumoca_core::ClassType::Package,
        local_b_id,
        local_b_scope,
    );
    let mut m = make_class_def("M", rumoca_core::ClassType::Model, m_id, m_scope);
    m.classes.insert("B".to_string(), local_b);
    let mut c = make_class_def("C", rumoca_core::ClassType::Package, c_id, c_scope);
    c.classes.insert("M".to_string(), m);
    let mut a = make_class_def("A", rumoca_core::ClassType::Package, a_id, a_scope);
    a.classes.insert("B".to_string(), ancestor_b);
    a.classes.insert("C".to_string(), c);
    tree.definitions.classes.insert("A".to_string(), a);

    for (def_id, name) in [
        (a_id, "A"),
        (ancestor_b_id, "A.B"),
        (c_id, "A.C"),
        (m_id, "A.C.M"),
        (local_b_id, "A.C.M.B"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }

    let mut imports = ImportMap::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    collect_lexical_package_aliases(&tree, &class_index, "A.C.M", &mut imports);

    assert_ne!(imports.get("B").map(String::as_str), Some("A.B"));
    assert!(!imports.contains_key("B"));
}

#[test]
fn test_lexical_package_alias_does_not_shadow_inherited_package() {
    let a_id = rumoca_core::DefId::new(10);
    let ancestor_b_id = rumoca_core::DefId::new(11);
    let c_id = rumoca_core::DefId::new(12);
    let base_id = rumoca_core::DefId::new(13);
    let m_id = rumoca_core::DefId::new(14);
    let inherited_b_id = rumoca_core::DefId::new(15);

    let mut tree = ast::ClassTree::new();
    let a_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ast::ScopeKind::Package);
    let ancestor_b_scope = tree
        .scope_tree
        .create_scope(a_scope, ast::ScopeKind::Package);
    let c_scope = tree
        .scope_tree
        .create_scope(a_scope, ast::ScopeKind::Package);
    let base_scope = tree.scope_tree.create_scope(c_scope, ast::ScopeKind::Class);
    let m_scope = tree.scope_tree.create_scope(c_scope, ast::ScopeKind::Class);
    let inherited_b_scope = tree
        .scope_tree
        .create_scope(base_scope, ast::ScopeKind::Package);

    tree.scope_tree
        .add_member(tree.scope_tree.global(), scope_key("A"), a_id);
    tree.scope_tree
        .add_member(a_scope, scope_key("B"), ancestor_b_id);
    tree.scope_tree.add_member(a_scope, scope_key("C"), c_id);
    tree.scope_tree
        .add_member(c_scope, scope_key("Base"), base_id);
    tree.scope_tree.add_member(c_scope, scope_key("M"), m_id);
    tree.scope_tree
        .add_member(base_scope, scope_key("B"), inherited_b_id);

    let ancestor_b = make_class_def(
        "B",
        rumoca_core::ClassType::Package,
        ancestor_b_id,
        ancestor_b_scope,
    );
    let inherited_b = make_class_def(
        "B",
        rumoca_core::ClassType::Package,
        inherited_b_id,
        inherited_b_scope,
    );
    let mut base = make_class_def("Base", rumoca_core::ClassType::Model, base_id, base_scope);
    base.classes.insert("B".to_string(), inherited_b);
    let mut m = make_class_def("M", rumoca_core::ClassType::Model, m_id, m_scope);
    m.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Base"),
        base_def_id: Some(base_id),
        ..Default::default()
    });
    let mut c = make_class_def("C", rumoca_core::ClassType::Package, c_id, c_scope);
    c.classes.insert("Base".to_string(), base);
    c.classes.insert("M".to_string(), m);
    let mut a = make_class_def("A", rumoca_core::ClassType::Package, a_id, a_scope);
    a.classes.insert("B".to_string(), ancestor_b);
    a.classes.insert("C".to_string(), c);
    tree.definitions.classes.insert("A".to_string(), a);

    for (def_id, name) in [
        (a_id, "A"),
        (ancestor_b_id, "A.B"),
        (c_id, "A.C"),
        (base_id, "A.C.Base"),
        (m_id, "A.C.M"),
        (inherited_b_id, "A.C.Base.B"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }

    let mut imports = ImportMap::default();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    collect_lexical_package_aliases(&tree, &class_index, "A.C.M", &mut imports);

    assert_ne!(imports.get("B").map(String::as_str), Some("A.B"));
    assert!(!imports.contains_key("B"));
}

#[test]
fn test_lexical_class_alias_replaces_outer_package_alias() {
    let modelica_id = rumoca_core::DefId::new(10);
    let top_utilities_id = rumoca_core::DefId::new(11);
    let fluid_id = rumoca_core::DefId::new(12);
    let fluid_utilities_id = rumoca_core::DefId::new(13);
    let pipes_id = rumoca_core::DefId::new(14);
    let func_id = rumoca_core::DefId::new(15);

    let mut tree = ast::ClassTree::new();
    let modelica_scope = tree
        .scope_tree
        .create_scope(tree.scope_tree.global(), ast::ScopeKind::Package);
    let top_utilities_scope = tree
        .scope_tree
        .create_scope(modelica_scope, ast::ScopeKind::Package);
    let fluid_scope = tree
        .scope_tree
        .create_scope(modelica_scope, ast::ScopeKind::Package);
    let fluid_utilities_scope = tree
        .scope_tree
        .create_scope(fluid_scope, ast::ScopeKind::Package);
    let pipes_scope = tree
        .scope_tree
        .create_scope(fluid_scope, ast::ScopeKind::Package);
    let func_scope = tree
        .scope_tree
        .create_scope(pipes_scope, ast::ScopeKind::Function);

    tree.scope_tree
        .add_member(tree.scope_tree.global(), scope_key("Modelica"), modelica_id);
    tree.scope_tree
        .add_member(modelica_scope, scope_key("Utilities"), top_utilities_id);
    tree.scope_tree
        .add_member(modelica_scope, scope_key("Fluid"), fluid_id);
    tree.scope_tree
        .add_member(fluid_scope, scope_key("Utilities"), fluid_utilities_id);
    tree.scope_tree
        .add_member(fluid_scope, scope_key("Pipes"), pipes_id);
    tree.scope_tree
        .add_member(pipes_scope, scope_key("f"), func_id);

    let top_utilities = make_class_def(
        "Utilities",
        rumoca_core::ClassType::Package,
        top_utilities_id,
        top_utilities_scope,
    );
    let fluid_utilities = make_class_def(
        "Utilities",
        rumoca_core::ClassType::Package,
        fluid_utilities_id,
        fluid_utilities_scope,
    );
    let func = make_class_def("f", rumoca_core::ClassType::Function, func_id, func_scope);
    let mut pipes = make_class_def(
        "Pipes",
        rumoca_core::ClassType::Package,
        pipes_id,
        pipes_scope,
    );
    pipes.classes.insert("f".to_string(), func);
    let mut fluid = make_class_def(
        "Fluid",
        rumoca_core::ClassType::Package,
        fluid_id,
        fluid_scope,
    );
    fluid
        .classes
        .insert("Utilities".to_string(), fluid_utilities);
    fluid.classes.insert("Pipes".to_string(), pipes);
    let mut modelica = make_class_def(
        "Modelica",
        rumoca_core::ClassType::Package,
        modelica_id,
        modelica_scope,
    );
    modelica
        .classes
        .insert("Utilities".to_string(), top_utilities);
    modelica.classes.insert("Fluid".to_string(), fluid);
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);

    for (def_id, name) in [
        (modelica_id, "Modelica"),
        (top_utilities_id, "Modelica.Utilities"),
        (fluid_id, "Modelica.Fluid"),
        (fluid_utilities_id, "Modelica.Fluid.Utilities"),
        (pipes_id, "Modelica.Fluid.Pipes"),
        (func_id, "Modelica.Fluid.Pipes.f"),
    ] {
        tree.def_map.insert(def_id, name.to_string());
        tree.name_map.insert(name.to_string(), def_id);
    }

    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut imports = ImportMap::default();
    imports.insert("Utilities".to_string(), "Modelica.Utilities".to_string());
    collect_lexical_class_aliases(&tree, &class_index, "Modelica.Fluid.Pipes.f", &mut imports);
    assert_eq!(
        imports.get("Utilities").map(String::as_str),
        Some("Modelica.Fluid.Utilities")
    );
}

#[test]
fn test_qualify_component_ref_empty_prefix() {
    let cr = make_comp_ref(&["x"]);
    let prefix = QualifiedName::new();
    let result = qualify_component_ref(&cr, &prefix, QualifyOptions::default());
    assert_eq!(result.parts.len(), 1);
    assert_eq!(&*result.parts[0].ident.text, "x");
}

#[test]
fn test_qualify_component_ref_with_prefix() {
    let cr = make_comp_ref(&["x"]);
    let mut prefix = QualifiedName::new();
    prefix.push("comp".to_string(), vec![]);

    let result = qualify_component_ref(&cr, &prefix, QualifyOptions::default());
    assert_eq!(result.parts.len(), 2);
    assert_eq!(&*result.parts[0].ident.text, "comp");
    assert_eq!(&*result.parts[1].ident.text, "x");
}

#[test]
fn qualifying_single_part_ref_preserves_target_def_id() {
    let def_id = rumoca_core::DefId::new(42);
    let mut cr = make_comp_ref(&["fluidConstants"]);
    cr.def_id = Some(def_id);
    let mut prefix = QualifiedName::new();
    prefix.push("source".to_string(), vec![]);
    prefix.push("medium".to_string(), vec![]);

    let result = qualify_component_ref(&cr, &prefix, QualifyOptions::default());

    assert_eq!(result.def_id, Some(def_id));
    assert_eq!(
        result
            .parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>(),
        vec!["source", "medium", "fluidConstants"]
    );
}

#[test]
fn test_qualify_local_ref_skipped() {
    let mut cr = make_comp_ref(&["x"]);
    cr.local = true;

    let mut prefix = QualifiedName::new();
    prefix.push("comp".to_string(), vec![]);

    let opts = QualifyOptions {
        skip_local: true,
        preserve_def_id: false,
    };
    let result = qualify_component_ref(&cr, &prefix, opts);

    // Local ref should not be qualified
    assert_eq!(result.parts.len(), 1);
    assert_eq!(&*result.parts[0].ident.text, "x");
}

#[test]
fn test_int_expr() {
    let expr = int_expr(42);
    if let Expression::Terminal {
        terminal_type,
        token,
        ..
    } = expr
    {
        assert_eq!(terminal_type, TerminalType::UnsignedInteger);
        assert_eq!(&*token.text, "42");
    } else {
        panic!("Expected Terminal expression");
    }
}

#[test]
fn test_qualify_array_comprehension_keeps_iterator_local() {
    let mut prefix = QualifiedName::new();
    prefix.push("comp".to_string(), vec![]);

    let expr = Expression::ArrayComprehension {
        expr: Arc::new(Expression::ComponentReference(make_comp_ref(&["i"]))),
        indices: vec![make_for_index(
            "i",
            Expression::Range {
                start: Arc::new(make_int(1)),
                step: None,
                end: Arc::new(Expression::ComponentReference(make_comp_ref(&["n"]))),
                span: rumoca_core::Span::DUMMY,
            },
        )],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());
    let Expression::ArrayComprehension {
        expr,
        indices,
        filter,
        ..
    } = qualified
    else {
        panic!("Expected ArrayComprehension");
    };
    assert!(filter.is_none());

    let Expression::ComponentReference(body_ref) = expr.as_ref() else {
        panic!("Expected comprehension body ComponentReference");
    };
    assert_eq!(body_ref.parts.len(), 1);
    assert_eq!(body_ref.parts[0].ident.text.as_ref(), "i");

    let Expression::Range { end, .. } = &indices[0].range else {
        panic!("Expected range in comprehension index");
    };
    let Expression::ComponentReference(end_ref) = end.as_ref() else {
        panic!("Expected qualified range end component reference");
    };
    assert_eq!(end_ref.parts.len(), 2);
    assert_eq!(end_ref.parts[0].ident.text.as_ref(), "comp");
    assert_eq!(end_ref.parts[1].ident.text.as_ref(), "n");
}

#[test]
fn test_qualify_array_comprehension_range_sees_prior_indices() {
    let mut prefix = QualifiedName::new();
    prefix.push("comp".to_string(), vec![]);

    let expr = Expression::ArrayComprehension {
        expr: Arc::new(Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Arc::new(Expression::ComponentReference(make_comp_ref(&["i"]))),
            rhs: Arc::new(Expression::ComponentReference(make_comp_ref(&["j"]))),
            span: rumoca_core::Span::DUMMY,
        }),
        indices: vec![
            make_for_index(
                "i",
                Expression::Range {
                    start: Arc::new(make_int(1)),
                    step: None,
                    end: Arc::new(Expression::ComponentReference(make_comp_ref(&["n"]))),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            make_for_index(
                "j",
                Expression::Range {
                    start: Arc::new(make_int(1)),
                    step: None,
                    end: Arc::new(Expression::ComponentReference(make_comp_ref(&["i"]))),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
        ],
        filter: None,
        span: rumoca_core::Span::DUMMY,
    };

    let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());
    let Expression::ArrayComprehension { indices, .. } = qualified else {
        panic!("Expected ArrayComprehension");
    };
    let Expression::Range { end, .. } = &indices[1].range else {
        panic!("Expected range in second comprehension index");
    };
    let Expression::ComponentReference(end_ref) = end.as_ref() else {
        panic!("Expected component reference in second range end");
    };
    assert_eq!(end_ref.parts.len(), 1);
    assert_eq!(end_ref.parts[0].ident.text.as_ref(), "i");
}

#[test]
fn test_import_resolves_short_name_to_fqn() {
    let mut prefix = QualifiedName::new();
    prefix.push("comp".to_string(), vec![]);

    let mut imports = ImportMap::default();
    imports.insert("pi".to_string(), "Modelica.Constants.pi".to_string());

    let expr = Expression::ComponentReference(make_comp_ref(&["pi"]));
    let qualified =
        qualify_expression_with_imports(&expr, &prefix, QualifyOptions::default(), &imports);

    let Expression::ComponentReference(cr) = qualified else {
        panic!("Expected ComponentReference");
    };
    assert_eq!(cr.parts.len(), 3);
    assert_eq!(cr.parts[0].ident.text.as_ref(), "Modelica");
    assert_eq!(cr.parts[1].ident.text.as_ref(), "Constants");
    assert_eq!(cr.parts[2].ident.text.as_ref(), "pi");
}

#[test]
fn test_import_resolves_function_call_receiver_to_fqn() {
    let mut imports = ImportMap::default();
    imports.insert(
        "Medium".to_string(),
        "Modelica.Media.Air.ReferenceAir.Air_pT".to_string(),
    );
    let expr = Expression::FunctionCall {
        comp: make_comp_ref(&["Medium", "temperature_phX"]),
        args: vec![],
        span: rumoca_core::Span::DUMMY,
    };

    let qualified = qualify_expression_with_imports(
        &expr,
        &QualifiedName::new(),
        QualifyOptions::default(),
        &imports,
    );

    let Expression::FunctionCall { comp, .. } = qualified else {
        panic!("Expected FunctionCall");
    };
    let parts = comp
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>();
    assert_eq!(
        parts,
        vec![
            "Modelica",
            "Media",
            "Air",
            "ReferenceAir",
            "Air_pT",
            "temperature_phX"
        ]
    );
}

#[test]
fn test_imported_function_call_shadows_builtin_name() {
    let mut imports = ImportMap::default();
    imports.insert("abs".to_string(), "Modelica.ComplexMath.abs".to_string());
    let expr = Expression::FunctionCall {
        comp: make_comp_ref(&["abs"]),
        args: vec![Expression::ComponentReference(make_comp_ref(&["c1"]))],
        span: rumoca_core::Span::DUMMY,
    };

    let qualified = qualify_expression_with_imports(
        &expr,
        &QualifiedName::new(),
        QualifyOptions::default(),
        &imports,
    );

    let Expression::FunctionCall { comp, .. } = qualified else {
        panic!("Expected FunctionCall");
    };
    let parts = comp
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>();
    assert_eq!(parts, vec!["Modelica", "ComplexMath", "abs"]);
}

#[test]
fn test_leading_dot_builtin_function_call_bypasses_import_shadow() {
    let mut imports = ImportMap::default();
    imports.insert("sum".to_string(), "Modelica.ComplexMath.sum".to_string());
    let mut comp = make_comp_ref(&["sum"]);
    comp.local = true;
    let expr = Expression::FunctionCall {
        comp,
        args: vec![Expression::ComponentReference(make_comp_ref(&["x"]))],
        span: rumoca_core::Span::DUMMY,
    };

    let qualified = qualify_expression_with_imports(
        &expr,
        &QualifiedName::new(),
        QualifyOptions::default(),
        &imports,
    );

    let Expression::FunctionCall { comp, .. } = qualified else {
        panic!("Expected FunctionCall");
    };
    assert_eq!(comp.parts.len(), 1);
    assert_eq!(comp.parts[0].ident.text.as_ref(), "sum");
    assert!(
        comp.local,
        "leading-dot builtin calls must retain their explicit global marker"
    );
}

#[test]
fn test_builtin_function_call_receiver_not_instance_qualified() {
    let mut prefix = QualifiedName::new();
    prefix.push("table".to_string(), vec![]);
    prefix.push("combiTimeTable".to_string(), vec![]);

    let expr = Expression::FunctionCall {
        comp: make_comp_ref(&["size"]),
        args: vec![Expression::ComponentReference(make_comp_ref(&["table"]))],
        span: rumoca_core::Span::DUMMY,
    };

    let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());

    let Expression::FunctionCall { comp, args, .. } = qualified else {
        panic!("Expected FunctionCall");
    };
    assert_eq!(comp.parts.len(), 1);
    assert_eq!(comp.parts[0].ident.text.as_ref(), "size");
    let Expression::ComponentReference(arg) = &args[0] else {
        panic!("Expected qualified component reference argument");
    };
    assert_eq!(arg.parts.len(), 3);
    assert_eq!(arg.parts[0].ident.text.as_ref(), "table");
    assert_eq!(arg.parts[1].ident.text.as_ref(), "combiTimeTable");
    assert_eq!(arg.parts[2].ident.text.as_ref(), "table");
}

#[test]
fn test_unimported_function_call_receiver_not_instance_qualified() {
    let mut prefix = QualifiedName::new();
    prefix.push("component".to_string(), vec![]);

    let expr = Expression::FunctionCall {
        comp: make_comp_ref(&["localFunction"]),
        args: vec![],
        span: rumoca_core::Span::DUMMY,
    };

    let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());

    let Expression::FunctionCall { comp, .. } = qualified else {
        panic!("Expected FunctionCall");
    };
    assert_eq!(comp.parts.len(), 1);
    assert_eq!(comp.parts[0].ident.text.as_ref(), "localFunction");
}

#[test]
fn test_time_builtin_not_qualified() {
    let mut prefix = QualifiedName::new();
    prefix.push("source".to_string(), vec![]);

    let expr = Expression::ComponentReference(make_comp_ref(&["time"]));
    let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());

    let Expression::ComponentReference(cr) = qualified else {
        panic!("Expected ComponentReference");
    };
    assert_eq!(cr.parts.len(), 1);
    assert_eq!(cr.parts[0].ident.text.as_ref(), "time");
}

#[test]
fn test_builtin_enum_literals_not_qualified() {
    let mut prefix = QualifiedName::new();
    prefix.push("source".to_string(), vec![]);

    let state_select = Expression::ComponentReference(make_comp_ref(&["StateSelect", "default"]));
    let qualified_state = qualify_expression(&state_select, &prefix, QualifyOptions::default());
    let Expression::ComponentReference(state_cr) = qualified_state else {
        panic!("Expected ComponentReference for StateSelect literal");
    };
    assert_eq!(state_cr.parts.len(), 2);
    assert_eq!(state_cr.parts[0].ident.text.as_ref(), "StateSelect");
    assert_eq!(state_cr.parts[1].ident.text.as_ref(), "default");

    let assertion = Expression::ComponentReference(make_comp_ref(&["AssertionLevel", "error"]));
    let qualified_assertion = qualify_expression(&assertion, &prefix, QualifyOptions::default());
    let Expression::ComponentReference(assert_cr) = qualified_assertion else {
        panic!("Expected ComponentReference for AssertionLevel literal");
    };
    assert_eq!(assert_cr.parts.len(), 2);
    assert_eq!(assert_cr.parts[0].ident.text.as_ref(), "AssertionLevel");
    assert_eq!(assert_cr.parts[1].ident.text.as_ref(), "error");
}

#[test]
fn test_non_imported_name_still_qualified() {
    let mut prefix = QualifiedName::new();
    prefix.push("comp".to_string(), vec![]);

    let mut imports = ImportMap::default();
    imports.insert("pi".to_string(), "Modelica.Constants.pi".to_string());

    // "f" is not in imports, so it should be qualified normally
    let expr = Expression::ComponentReference(make_comp_ref(&["f"]));
    let qualified =
        qualify_expression_with_imports(&expr, &prefix, QualifyOptions::default(), &imports);

    let Expression::ComponentReference(cr) = qualified else {
        panic!("Expected ComponentReference");
    };
    assert_eq!(cr.parts.len(), 2);
    assert_eq!(cr.parts[0].ident.text.as_ref(), "comp");
    assert_eq!(cr.parts[1].ident.text.as_ref(), "f");
}

#[test]
fn test_import_alias_resolves_prefixed_component_reference() {
    let mut prefix = QualifiedName::new();
    prefix.push("inst".to_string(), vec![]);

    let mut imports = ImportMap::default();
    imports.insert(
        "L".to_string(),
        "Modelica.Electrical.Digital.Interfaces.Logic".to_string(),
    );

    let expr = Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("L"),
                    ..Default::default()
                },
                subs: None,
            },
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("'1'"),
                    ..Default::default()
                },
                subs: None,
            },
        ],
        def_id: Some(rumoca_core::DefId::new(99)),
        span: rumoca_core::Span::DUMMY,
    });

    let qualified = qualify_expression_with_imports(
        &expr,
        &prefix,
        QualifyOptions {
            preserve_def_id: true,
            ..QualifyOptions::default()
        },
        &imports,
    );
    let Expression::ComponentReference(cr) = qualified else {
        panic!("Expected ComponentReference");
    };
    let parts: Vec<&str> = cr
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect();
    assert_eq!(
        parts,
        vec![
            "Modelica",
            "Electrical",
            "Digital",
            "Interfaces",
            "Logic",
            "'1'"
        ]
    );
    assert_eq!(cr.def_id, Some(rumoca_core::DefId::new(99)));
}

#[test]
fn test_fully_qualified_ref_qualifies_subscript_expressions() {
    let mut prefix = QualifiedName::new();
    prefix.push("MUX".to_string(), vec![]);
    prefix.push("And1".to_string(), vec![]);

    let ref_with_subscripts = ComponentReference {
        local: false,
        parts: vec![
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("Modelica"),
                    ..Default::default()
                },
                subs: None,
            },
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("Electrical"),
                    ..Default::default()
                },
                subs: None,
            },
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("Digital"),
                    ..Default::default()
                },
                subs: None,
            },
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("Tables"),
                    ..Default::default()
                },
                subs: None,
            },
            ComponentRefPart {
                ident: Token {
                    text: Arc::from("AndTable"),
                    ..Default::default()
                },
                subs: Some(vec![
                    Subscript::Expression(Expression::ComponentReference(ComponentReference {
                        local: false,
                        parts: vec![ComponentRefPart {
                            ident: Token {
                                text: Arc::from("auxiliary"),
                                ..Default::default()
                            },
                            subs: Some(vec![Subscript::Expression(make_int(1))]),
                        }],
                        def_id: None,
                        span: rumoca_core::Span::DUMMY,
                    })),
                    Subscript::Expression(Expression::ComponentReference(ComponentReference {
                        local: false,
                        parts: vec![ComponentRefPart {
                            ident: Token {
                                text: Arc::from("x"),
                                ..Default::default()
                            },
                            subs: Some(vec![Subscript::Expression(make_int(2))]),
                        }],
                        def_id: None,
                        span: rumoca_core::Span::DUMMY,
                    })),
                ]),
            },
        ],
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    };

    let expr = Expression::ComponentReference(ref_with_subscripts);
    let qualified = qualify_expression(&expr, &prefix, QualifyOptions::default());
    let Expression::ComponentReference(qualified_ref) = qualified else {
        panic!("Expected ComponentReference");
    };
    assert_eq!(qualified_ref.parts.len(), 5);
    assert_eq!(qualified_ref.parts[0].ident.text.as_ref(), "Modelica");
    let Some(subscripts) = qualified_ref.parts[4].subs.as_ref() else {
        panic!("expected AndTable subscripts");
    };
    assert_eq!(subscripts.len(), 2);

    let Subscript::Expression(Expression::ComponentReference(aux_ref)) = &subscripts[0] else {
        panic!("expected auxiliary ref in first subscript");
    };
    assert_eq!(aux_ref.parts.len(), 3);
    assert_eq!(aux_ref.parts[0].ident.text.as_ref(), "MUX");
    assert_eq!(aux_ref.parts[1].ident.text.as_ref(), "And1");
    assert_eq!(aux_ref.parts[2].ident.text.as_ref(), "auxiliary");

    let Subscript::Expression(Expression::ComponentReference(x_ref)) = &subscripts[1] else {
        panic!("expected x ref in second subscript");
    };
    assert_eq!(x_ref.parts.len(), 3);
    assert_eq!(x_ref.parts[0].ident.text.as_ref(), "MUX");
    assert_eq!(x_ref.parts[1].ident.text.as_ref(), "And1");
    assert_eq!(x_ref.parts[2].ident.text.as_ref(), "x");
}
