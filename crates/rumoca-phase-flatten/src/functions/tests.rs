use super::*;
use std::sync::Arc;

fn token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        ..rumoca_core::Token::default()
    }
}

fn class(
    name: &str,
    class_type: rumoca_core::ClassType,
    def_id: rumoca_core::DefId,
) -> ast::ClassDef {
    ast::ClassDef {
        name: token(name),
        class_type,
        def_id: Some(def_id),
        ..ast::ClassDef::default()
    }
}

/// Canonicalize with no declared class scopes, so callable references keep the
/// exact structured path the fixture gives them.
fn canonicalize_collected_function_calls_without_scopes(
    flat: &mut flat::Model,
) -> Result<(), FlattenError> {
    let tree = ast::ClassTree::new();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    canonicalize_collected_function_calls(flat, &class_index)
}

fn test_source_map() -> rumoca_core::SourceMap {
    let mut source_map = rumoca_core::SourceMap::new();
    source_map.add(
        "function_param_fixture.mo",
        "record C\n  Real orientation;\n  Real N;\nend C;\n",
    );
    source_map
}

fn resolve_test_source(source: &str, file_name: &str) -> rumoca_phase_resolve::ResolvedTree {
    let stored = rumoca_phase_parse::parse_to_ast(source, file_name).expect("valid source");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file_name, source);
    rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("test source resolves")
}

fn test_location(start: u32, end: u32) -> rumoca_core::Location {
    rumoca_core::Location {
        start_line: 1,
        start_column: start + 1,
        end_line: 1,
        end_column: end + 1,
        start,
        end,
        source: rumoca_core::SourceId::from_source_name("function_param_fixture.mo"),
    }
}

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_param_fixture.mo"),
        1,
        2,
    )
}

#[test]
fn vectorization_certificate_rejects_replaceable_exposure_parent() {
    let package_id = rumoca_core::DefId::new(80_001);
    let function_id = rumoca_core::DefId::new(80_002);
    let mut package = class("P", rumoca_core::ClassType::Package, package_id);
    package.is_replaceable = true;
    package.classes.insert(
        "f".to_string(),
        class("f", rumoca_core::ClassType::Function, function_id),
    );
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("P".to_string(), package);
    let index = ast::ClassDefIndex::from_tree(&tree);
    let reference = core_structured_comp_ref(&[("P", package_id), ("f", function_id)]);
    let request = FunctionRequest::from_component_reference(&reference);

    assert!(!request_proves_transitive_non_replaceability(
        &index, &request
    ));
}

#[test]
fn vectorization_certificate_ignores_replaceable_nested_sibling() {
    let package_id = rumoca_core::DefId::new(80_011);
    let function_id = rumoca_core::DefId::new(80_012);
    let sibling_id = rumoca_core::DefId::new(80_013);
    let mut package = class("P", rumoca_core::ClassType::Package, package_id);
    package.classes.insert(
        "f".to_string(),
        class("f", rumoca_core::ClassType::Function, function_id),
    );
    let mut sibling = class("Choice", rumoca_core::ClassType::Model, sibling_id);
    sibling.is_replaceable = true;
    package.classes.insert("Choice".to_string(), sibling);
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("P".to_string(), package);
    let index = ast::ClassDefIndex::from_tree(&tree);
    let reference = core_structured_comp_ref(&[("P", package_id), ("f", function_id)]);
    let request = FunctionRequest::from_component_reference(&reference);

    assert!(request_proves_transitive_non_replaceability(
        &index, &request
    ));
}

#[test]
fn vectorization_certificate_rejects_unresolved_short_alias() {
    let function_id = rumoca_core::DefId::new(80_021);
    let mut function = class("f", rumoca_core::ClassType::Function, function_id);
    function.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Missing"),
        base_def_id: None,
        ..ast::Extend::default()
    });
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("f".to_string(), function);
    let index = ast::ClassDefIndex::from_tree(&tree);
    let reference = core_structured_comp_ref(&[("f", function_id)]);
    let request = FunctionRequest::from_component_reference(&reference);

    assert!(!request_proves_transitive_non_replaceability(
        &index, &request
    ));
}

fn core_comp_ref(parts: &[&str], def_id: rumoca_core::DefId) -> rumoca_core::ComponentReference {
    let display = parts.join(".");
    rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: display,
            span: test_span(),
            subs: Vec::new(),
            def_id,
        }],
    )
    .expect("test reference is nonempty and resolved")
}

fn core_structured_comp_ref(
    parts: &[(&str, rumoca_core::DefId)],
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        parts
            .iter()
            .map(|(ident, def_id)| rumoca_core::ComponentRefPart {
                ident: (*ident).to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: *def_id,
            })
            .collect(),
    )
    .expect("test reference is nonempty and every path segment is resolved")
}

fn ast_comp_ref(parts: &[&str], def_id: rumoca_core::DefId) -> ast::ComponentReference {
    let display = parts.join(".");
    ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: token(&display),
            subs: None,
            def_id: Some(def_id),
        }],
        span: test_span(),
        qualified_display_name: Some(rumoca_core::VarName::new(display)),
    }
}

fn ast_comp_ref_with_subscripts(
    parts: &[&str],
    def_id: rumoca_core::DefId,
    subscripts: Vec<ast::Subscript>,
) -> ast::ComponentReference {
    let mut reference = ast_comp_ref(parts, def_id);
    reference.parts[0].subs = Some(subscripts);
    reference
}

#[test]
fn canonicalize_collected_function_calls_does_not_recover_hierarchy_from_suffix() {
    let mut flat = flat::Model::new();
    let mut function = rumoca_core::Function::new("Modelica.Math.Polynomials.fitting", test_span());
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(function);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Polynomials.fitting"),
            args: vec![],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("canonicalize function calls");

    let rumoca_core::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call residual");
    };
    assert_eq!(name.as_str(), "Polynomials.fitting");
    assert_eq!(name.resolved_function(), None);
}

#[test]
fn canonicalize_collected_function_calls_uses_def_id_for_record_constructors() {
    let constructor_def_id = rumoca_core::DefId::new(42);
    let mut flat = flat::Model::new();
    let mut constructor = rumoca_core::Function::new(
        "Modelica.Electrical.Machines.Utilities.ParameterRecords.SM_PermanentMagnetData",
        test_span(),
    );
    constructor.def_id = Some(constructor_def_id);
    constructor.is_constructor = true;
    constructor.add_input(crate::test_support::real_param(
        "PRef",
        Vec::new(),
        test_span(),
    ));
    flat.add_function(constructor);
    let component_ref = core_comp_ref(
        &["Utilities", "ParameterRecords", "SM_PermanentMagnetData"],
        constructor_def_id,
    );
    let variable_name = rumoca_core::VarName::new("x");
    flat.add_variable(
        variable_name.clone(),
        flat::Variable {
            name: variable_name.clone(),
            binding: Some(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::with_component_reference(
                    "Utilities.ParameterRecords.SM_PermanentMagnetData",
                    component_ref.clone(),
                ),
                args: vec![],
                is_constructor: true,
                span: test_span(),
            }),
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("canonicalize function calls");

    let Some(rumoca_core::Expression::FunctionCall { name, .. }) = flat
        .variables
        .get(&variable_name)
        .and_then(|var| var.binding.as_ref())
    else {
        panic!("expected constructor binding");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Electrical.Machines.Utilities.ParameterRecords.SM_PermanentMagnetData"
    );
    assert_eq!(name.target_def_id(), Some(constructor_def_id));
    assert_eq!(name.component_ref(), Some(&component_ref));

    prune_unreachable_functions(&mut flat);
    assert!(
        flat.functions.values().any(|function| {
            function.def_id == Some(constructor_def_id) && function.is_constructor
        }),
        "canonicalized constructor calls must retain their function by instance identity"
    );
}

#[test]
fn exact_instance_completes_valid_nested_constructor_field_projection() {
    let mut flat = flat::Model::new();
    let field_def_id = rumoca_core::DefId::new(45);
    let mut constructor = rumoca_core::Function::new("Pkg.SyntheticRecord", test_span());
    constructor.def_id = Some(rumoca_core::DefId::new(44));
    constructor.is_constructor = true;
    constructor.add_input(
        crate::test_support::real_param("field", Vec::new(), test_span()).with_def_id(field_def_id),
    );
    flat.add_function(constructor);
    let constructor_instance = flat.functions[&rumoca_core::VarName::new("Pkg.SyntheticRecord")]
        .instance_id
        .expect("Flat assigns the canonical constructor instance");

    let mut read = rumoca_core::Function::new("Pkg.read", test_span());
    read.add_input(crate::test_support::real_param(
        "value",
        Vec::new(),
        test_span(),
    ));
    read.body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(read);
    let read_instance = flat.functions[&rumoca_core::VarName::new("Pkg.read")]
        .instance_id
        .expect("Flat assigns the canonical regular-function instance");

    let constructor_call = rumoca_core::Expression::FunctionCall {
        // Deliberately use a different display spelling: only the exact
        // resolved instance is allowed to complete callable kind.
        name: rumoca_core::Reference::new("sourceConstructorExposure").with_resolved_function(
            rumoca_core::ResolvedFunctionReference {
                instance_id: constructor_instance,
                base_part_count: 0,
                transitively_non_replaceable: false,
            },
        ),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("sourceReadExposure").with_resolved_function(
                rumoca_core::ResolvedFunctionReference {
                    instance_id: read_instance,
                    base_part_count: 0,
                    transitively_non_replaceable: false,
                },
            ),
            args: vec![rumoca_core::Expression::FieldAccess {
                base: Box::new(constructor_call),
                field: "field".to_string(),
                field_def_id,
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("exact constructor metadata completes the occurrence role");

    let rumoca_core::Expression::FunctionCall {
        args,
        is_constructor: outer_is_constructor,
        ..
    } = &flat.equations[0].residual
    else {
        panic!("expected outer function call residual");
    };
    assert!(!outer_is_constructor);
    let [rumoca_core::Expression::FieldAccess { base, .. }] = args.as_slice() else {
        panic!("expected one record-field projection argument");
    };
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = base.as_ref()
    else {
        panic!("expected constructor beneath the field projection");
    };
    assert_eq!(args.len(), 1, "the constructor call keeps valid arity");
    assert_eq!(name.as_str(), "Pkg.SyntheticRecord");
    assert_eq!(
        name.resolved_function()
            .map(|resolved| resolved.instance_id),
        Some(constructor_instance)
    );
    assert!(*is_constructor);
}

#[test]
fn exact_instance_separates_constructor_and_regular_shared_def_exposures() {
    let mut flat = flat::Model::new();
    let shared_def_id = rumoca_core::DefId::new(46);
    let mut constructor = rumoca_core::Function::new("Pkg.Shared.constructor", test_span());
    constructor.def_id = Some(shared_def_id);
    constructor.is_constructor = true;
    flat.add_function(constructor);
    let constructor_instance = flat.functions[&rumoca_core::VarName::new("Pkg.Shared.constructor")]
        .instance_id
        .expect("Flat assigns the constructor exposure identity");

    let mut regular = rumoca_core::Function::new("Pkg.Shared.regular", test_span());
    regular.def_id = Some(shared_def_id);
    regular
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(regular);
    let regular_instance = flat.functions[&rumoca_core::VarName::new("Pkg.Shared.regular")]
        .instance_id
        .expect("Flat assigns the regular exposure identity");
    for instance_id in [constructor_instance, regular_instance] {
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                // Both occurrences deliberately render alike and originate
                // from one declaration. Only their exact exposure differs.
                name: rumoca_core::Reference::new("Pkg.Shared.exposure").with_resolved_function(
                    rumoca_core::ResolvedFunctionReference {
                        instance_id,
                        base_part_count: 0,
                        transitively_non_replaceable: false,
                    },
                ),
                args: vec![],
                is_constructor: false,
                span: test_span(),
            },
            test_span(),
            flat::EquationOrigin::ComponentEquation {
                component: "test".to_string(),
            },
        ));
    }

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("exact exposure identity decides callable kind");

    let kinds = flat
        .equations
        .iter()
        .map(|equation| {
            let rumoca_core::Expression::FunctionCall {
                name,
                is_constructor,
                ..
            } = &equation.residual
            else {
                panic!("expected function call residual");
            };
            (
                name.resolved_function()
                    .expect("canonical call keeps exact exposure")
                    .instance_id,
                *is_constructor,
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        kinds,
        vec![(constructor_instance, true), (regular_instance, false)]
    );
}

#[test]
fn unresolved_name_and_def_matches_do_not_acquire_constructor_kind() {
    let mut flat = flat::Model::new();
    let constructor_def_id = rumoca_core::DefId::new(47);
    let mut constructor = rumoca_core::Function::new("Pkg.UnresolvedConstructor", test_span());
    constructor.def_id = Some(constructor_def_id);
    constructor.is_constructor = true;
    flat.add_function(constructor);
    let unresolved_names = [
        rumoca_core::Reference::new("Pkg.UnresolvedConstructor"),
        rumoca_core::Reference::with_component_reference(
            "SourceOnlyConstructorExposure",
            core_comp_ref(&["SourceOnlyConstructorExposure"], constructor_def_id),
        ),
    ];
    for name in unresolved_names {
        flat.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name,
                args: vec![],
                is_constructor: false,
                span: test_span(),
            },
            test_span(),
            flat::EquationOrigin::ComponentEquation {
                component: "test".to_string(),
            },
        ));
    }

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("name-only lookup cannot establish structural constructor kind");

    for equation in &flat.equations {
        let rumoca_core::Expression::FunctionCall {
            name,
            is_constructor,
            ..
        } = &equation.residual
        else {
            panic!("expected function call residual");
        };
        assert!(!is_constructor);
        assert_eq!(
            name.resolved_function(),
            None,
            "a later pass must not launder a fallback match into exact provenance"
        );
    }
}

#[test]
fn canonicalize_collected_function_calls_rejects_constructor_marker_on_regular_instance() {
    let mut flat = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.regular", test_span());
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(function);
    let instance_id = flat.functions[&rumoca_core::VarName::new("Pkg.regular")]
        .instance_id
        .expect("Flat assigns the canonical regular-function instance");
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.regular").with_resolved_function(
                rumoca_core::ResolvedFunctionReference {
                    instance_id,
                    base_part_count: 0,
                    transitively_non_replaceable: false,
                },
            ),
            args: vec![],
            is_constructor: true,
            span: test_span(),
        },
        test_span(),
        flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let error = canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect_err("a constructor marker cannot override exact regular-function identity");
    assert!(matches!(
        error,
        FlattenError::InconsistentFunctionCallKind {
            function,
            instance,
            span,
        } if function == "Pkg.regular"
            && instance == instance_id.index()
            && span == test_span()
    ));
}

#[test]
fn canonicalize_collected_function_calls_rejects_disagreeing_name_and_resolved_identity() {
    let function_def_id = rumoca_core::DefId::new(43);
    let mut flat = flat::Model::new();
    let mut partial = rumoca_core::Function::new(
        "Modelica.Media.Interfaces.PartialMedium.setState_pTX",
        test_span(),
    );
    partial.def_id = Some(function_def_id);
    partial
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(partial);
    let mut concrete = rumoca_core::Function::new(
        "Modelica.Media.Air.ReferenceMoistAir.setState_pTX",
        test_span(),
    );
    concrete.def_id = Some(function_def_id);
    concrete
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(concrete);

    let component_ref = core_comp_ref(
        &[
            "Modelica",
            "Media",
            "Air",
            "ReferenceMoistAir",
            "setState_pTX",
        ],
        function_def_id,
    );
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "Modelica.Media.Interfaces.PartialMedium.setState_pTX",
                component_ref.clone(),
            ),
            args: vec![],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let error = canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect_err("conflicting function identities must fail at the Flat boundary");

    assert!(matches!(
        error,
        FlattenError::InconsistentFunctionReference { rendered, structured, .. }
            if rendered == "Modelica.Media.Interfaces.PartialMedium.setState_pTX"
                && structured == "Modelica.Media.Air.ReferenceMoistAir.setState_pTX"
    ));
}

/// MLS §5.3: a callable written `Concrete.Element` inside `package P` looks up
/// to `P.Concrete.Element`, so Resolve keeps the use-site path in the
/// structured reference and the lookup-qualified path in the rendered name.
/// Canonicalization must restate the structured path from exact
/// enclosing-scope identities rather than reject the pair.
#[test]
fn canonicalize_collected_function_calls_restates_enclosing_scope_identity() {
    let package_def = rumoca_core::DefId::new(1);
    let inner_def = rumoca_core::DefId::new(2);
    let record_def = rumoca_core::DefId::new(3);

    let mut inner = class("Concrete", rumoca_core::ClassType::Package, inner_def);
    inner.classes.insert(
        "Element".to_string(),
        class("Element", rumoca_core::ClassType::Record, record_def),
    );
    let mut outer = class("P", rumoca_core::ClassType::Package, package_def);
    outer.classes.insert("Concrete".to_string(), inner);
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("P".to_string(), outer);
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    let mut flat = flat::Model::new();
    let mut constructor = rumoca_core::Function::new("P.Concrete.Element", test_span());
    constructor.def_id = Some(record_def);
    constructor.is_constructor = true;
    constructor.add_input(crate::test_support::real_param(
        "position",
        Vec::new(),
        test_span(),
    ));
    flat.add_function(constructor);

    let variable_name = rumoca_core::VarName::new("left");
    flat.add_variable(
        variable_name.clone(),
        flat::Variable {
            name: variable_name.clone(),
            binding: Some(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::with_component_reference(
                    "P.Concrete.Element",
                    core_structured_comp_ref(&[("Concrete", inner_def), ("Element", record_def)]),
                ),
                args: vec![],
                is_constructor: true,
                span: test_span(),
            }),
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    canonicalize_collected_function_calls(&mut flat, &class_index)
        .expect("scope-qualified callable identity should reconcile");

    let Some(rumoca_core::Expression::FunctionCall { name, .. }) = flat
        .variables
        .get(&variable_name)
        .and_then(|var| var.binding.as_ref())
    else {
        panic!("expected constructor binding");
    };
    assert_eq!(name.as_str(), "P.Concrete.Element");
    let reference = name.component_ref().expect("structured callable reference");
    assert_eq!(
        reference
            .parts()
            .iter()
            .map(|part| (part.ident.as_str(), part.def_id))
            .collect::<Vec<_>>(),
        vec![
            ("P", package_def),
            ("Concrete", inner_def),
            ("Element", record_def),
        ]
    );
    assert!(
        name.resolved_function().is_some(),
        "reconciled callable must carry its Flat instance identity"
    );
}

#[test]
fn canonicalize_collected_function_calls_visits_when_chains() {
    let mut flat = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.Events.trip", test_span());
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(function);

    let mut branch = flat::WhenBranch::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.Events.trip"),
            args: vec![],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
    );
    branch.add_equation(flat::WhenEquation::Conditional {
        branches: vec![(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.Events.trip"),
                args: vec![],
                is_constructor: false,
                span: test_span(),
            },
            vec![flat::WhenEquation::FunctionCallOutputs {
                outputs: vec![rumoca_core::VarName::new("y")],
                function: rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Pkg.Events.trip"),
                    args: vec![],
                    is_constructor: false,
                    span: test_span(),
                },
                span: test_span(),
                origin: "when function call".to_string(),
            }],
        )],
        else_branch: Some(vec![flat::WhenEquation::Assign {
            target: rumoca_core::VarName::new("y"),
            value: rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("Pkg.Events.trip"),
                args: vec![],
                is_constructor: false,
                span: test_span(),
            },
            span: test_span(),
            origin: "when assignment".to_string(),
        }]),
        span: test_span(),
        origin: "nested when branch".to_string(),
    });
    let chain = flat::WhenChain::new(branch, test_span());
    flat.when_chains.push(chain);

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("canonicalize function calls");

    assert_function_call_name(&flat.when_chains[0].first().condition, "Pkg.Events.trip");
    let flat::WhenEquation::Conditional {
        branches,
        else_branch,
        ..
    } = &flat.when_chains[0].first().equations[0]
    else {
        panic!("expected conditional when equation");
    };
    assert_function_call_name(&branches[0].0, "Pkg.Events.trip");
    let flat::WhenEquation::FunctionCallOutputs { function, .. } = &branches[0].1[0] else {
        panic!("expected function-call output when equation");
    };
    assert_function_call_name(function, "Pkg.Events.trip");
    let flat::WhenEquation::Assign { value, .. } = &else_branch
        .as_ref()
        .expect("source else branch remains present")[0]
    else {
        panic!("expected assignment when equation");
    };
    assert_function_call_name(value, "Pkg.Events.trip");
}

fn assert_function_call_name(expr: &rumoca_core::Expression, expected: &str) {
    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call expression, got {expr:?}");
    };
    assert_eq!(name.as_str(), expected);
}

#[test]
fn canonicalize_collected_function_calls_leaves_ambiguous_suffix() {
    let mut flat = flat::Model::new();
    let mut math_function =
        rumoca_core::Function::new("Modelica.Math.Polynomials.fitting", test_span());
    math_function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(math_function);
    let mut user_function = rumoca_core::Function::new("User.Polynomials.fitting", test_span());
    user_function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(user_function);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Polynomials.fitting"),
            args: vec![],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("canonicalize function calls");

    let rumoca_core::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call residual");
    };
    assert_eq!(name.as_str(), "Polynomials.fitting");
}

#[test]
fn canonicalize_collected_function_calls_distinguishes_duplicate_inherited_def_ids() {
    let mut flat = flat::Model::new();
    let package_def_id = rumoca_core::DefId::new(898);
    let exposure_def_id = rumoca_core::DefId::new(899);
    let shared_def_id = rumoca_core::DefId::new(900);
    for name in ["Pkg.A.f", "Pkg.B.f"] {
        let mut function = rumoca_core::Function::new(name, test_span());
        function.def_id = Some(shared_def_id);
        function
            .body
            .push(rumoca_core::Statement::Return { span: test_span() });
        flat.add_function(function);
    }
    let call_ref = core_structured_comp_ref(&[
        ("Pkg", package_def_id),
        ("B", exposure_def_id),
        ("f", shared_def_id),
    ]);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(call_ref)
                .with_instance_id(rumoca_core::InstanceId::new(77)),
            args: vec![],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("canonicalize function calls");

    let expected_instance = flat.functions[&rumoca_core::VarName::new("Pkg.B.f")]
        .instance_id
        .expect("flattened function instance identity");
    let rumoca_core::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call residual");
    };
    assert_eq!(name.as_str(), "Pkg.B.f");
    assert_eq!(
        name.instance_id(),
        Some(rumoca_core::InstanceId::new(77)),
        "canonicalizing an inherited function exposure preserves occurrence identity"
    );
    assert_eq!(
        name.resolved_function(),
        Some(rumoca_core::ResolvedFunctionReference {
            instance_id: expected_instance,
            base_part_count: 3,
            transitively_non_replaceable: true,
        })
    );
}

#[test]
fn canonicalize_collected_function_calls_prefers_exact_name_over_stale_def_id() {
    let inherited_def_id = rumoca_core::DefId::new(901);
    let flattened_def_id = rumoca_core::DefId::new(902);
    let mut flat = flat::Model::new();
    let mut function = rumoca_core::Function::new("Pkg.Medium.density", test_span());
    function.def_id = Some(flattened_def_id);
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(function);
    let call_ref = core_comp_ref(&["Pkg", "Medium", "density"], inherited_def_id);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(call_ref),
            args: vec![],
            is_constructor: false,
            span: test_span(),
        },
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    canonicalize_collected_function_calls_without_scopes(&mut flat)
        .expect("canonicalize function calls");

    let expected_instance = flat.functions[&rumoca_core::VarName::new("Pkg.Medium.density")]
        .instance_id
        .expect("flattened function instance identity");
    let rumoca_core::Expression::FunctionCall { name, .. } = &flat.equations[0].residual else {
        panic!("expected function call residual");
    };
    assert_eq!(
        name.resolved_function()
            .map(|resolved| resolved.instance_id),
        Some(expected_instance)
    );
}

#[test]
fn validates_function_output_assignment_before_return() {
    let output_def_id = rumoca_core::DefId::new(4101);
    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(output_def_id),
    );
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["y"], output_def_id),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: test_span(),
        },
        span: test_span(),
    });

    validate_function_outputs_assigned(&function).expect("output assignment is valid");
}

#[test]
fn rejects_function_output_only_assigned_after_return() {
    let output_def_id = rumoca_core::DefId::new(4102);
    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_output(
        crate::test_support::real_param("y", Vec::new(), test_span()).with_def_id(output_def_id),
    );
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["y"], output_def_id),
        value: rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: test_span(),
        },
        span: test_span(),
    });

    let err = validate_function_outputs_assigned(&function).expect_err("output is unassigned");
    assert!(matches!(
        err,
        FlattenError::FunctionOutputUnassigned {
            ref function,
            ref output,
            ..
        } if function == "Pkg.f" && output == "y"
    ));
}

#[test]
fn test_collect_no_function_calls() {
    let flat = flat::Model::new();
    let calls = collect_function_calls(&flat);
    assert!(calls.is_empty());
}

#[test]
fn test_collect_function_call_in_equation() {
    let mut flat = flat::Model::new();

    // Create an equation with a function call: 0 = myFunc(x) - y
    let func_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("MyPackage.myFunc"),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("x"),
            subscripts: vec![],
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(func_call),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("y"),
            subscripts: vec![],
            span: test_span(),
        }),
        span: test_span(),
    };
    flat.add_equation(flat::Equation::new(
        residual,
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let calls = collect_function_calls(&flat);
    assert!(calls.contains("MyPackage.myFunc"));
    assert_eq!(calls.len(), 1);
}

#[test]
fn function_request_collection_deduplicates_by_def_id() {
    let target_def_id = rumoca_core::DefId::new(42);
    let y_def_id = rumoca_core::DefId::new(43);
    let z_def_id = rumoca_core::DefId::new(44);
    let mut function = rumoca_core::Function::new("Pkg.wrapper", test_span());
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["y"], y_def_id),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "Alias.f",
                core_comp_ref(&["Alias", "f"], target_def_id),
            ),
            args: Vec::new(),
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
    });
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["z"], z_def_id),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "Pkg.f",
                core_comp_ref(&["Alias", "f"], target_def_id),
            ),
            args: Vec::new(),
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
    });

    let calls = collect_function_dep_requests(&function);

    assert_eq!(calls.len(), 1);
    assert_eq!(calls[0].target_def_id, Some(target_def_id));
}

#[test]
fn target_def_id_request_keeps_concrete_exposed_package() {
    let partial_pkg_def = rumoca_core::DefId::new(1);
    let inherited_fn_def = rumoca_core::DefId::new(2);
    let concrete_pkg_def = rumoca_core::DefId::new(3);

    let mut inherited_fn = class(
        "specificEnthalpy_pTX",
        rumoca_core::ClassType::Function,
        inherited_fn_def,
    );
    inherited_fn.location = test_location(9, 27);
    let mut return_token = token("return");
    return_token.location = test_location(28, 34);
    inherited_fn.algorithms.push(vec![ast::Statement::Return {
        token: return_token,
    }]);

    let mut partial_pkg = class(
        "PartialMedium",
        rumoca_core::ClassType::Package,
        partial_pkg_def,
    );
    partial_pkg
        .classes
        .insert("specificEnthalpy_pTX".to_string(), inherited_fn);

    let mut concrete_pkg = class(
        "ReferenceMoistAir",
        rumoca_core::ClassType::Package,
        concrete_pkg_def,
    );
    concrete_pkg.extends.push(ast::Extend {
        base_name: ast::Name {
            def_id: Some(partial_pkg_def),
            ..ast::Name::from_string("PartialMedium")
        },
        base_def_id: Some(partial_pkg_def),
        ..ast::Extend::default()
    });

    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.source_map = test_source_map();
    tree.definitions
        .classes
        .insert("PartialMedium".to_string(), partial_pkg);
    tree.definitions
        .classes
        .insert("ReferenceMoistAir".to_string(), concrete_pkg);
    tree.def_map
        .insert(partial_pkg_def, "PartialMedium".to_string());
    tree.def_map.insert(
        inherited_fn_def,
        "PartialMedium.specificEnthalpy_pTX".to_string(),
    );
    tree.def_map
        .insert(concrete_pkg_def, "ReferenceMoistAir".to_string());

    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let request = FunctionRequest {
        name: "ReferenceMoistAir.specificEnthalpy_pTX".to_string(),
        target_def_id: Some(inherited_fn_def),
        target_instance_id: None,
        component_ref: Some(core_structured_comp_ref(&[
            ("ReferenceMoistAir", concrete_pkg_def),
            ("specificEnthalpy_pTX", inherited_fn_def),
        ])),
    };

    let type_overlay = crate::test_support::type_overlay(&tree);
    let (resolved_name, function) = lookup_function_request(
        &tree,
        &class_index,
        &request,
        FunctionTypeCatalog::new(&type_overlay),
    )
    .expect("lookup should not error")
    .expect("inherited function should resolve through concrete package");

    assert_eq!(resolved_name, "ReferenceMoistAir.specificEnthalpy_pTX");
    assert_eq!(
        function.name.as_str(),
        "ReferenceMoistAir.specificEnthalpy_pTX"
    );
    assert_eq!(function.def_id, Some(inherited_fn_def));
    assert!(function.transitively_non_replaceable);
}

#[test]
fn preserves_relative_package_call_spelling_with_exact_target() {
    let (tree, rename_def) = spice3_relative_function_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);

    assert_relative_package_function_call_is_exact(
        &tree,
        &class_index,
        "Modelica.Electrical.Spice3.Internal.MOS",
        rename_def,
    );
    assert_relative_package_function_call_is_exact(
        &tree,
        &class_index,
        "Modelica.Electrical.Spice3.Semiconductors.M_PMOS",
        rename_def,
    );
}

fn spice3_relative_function_tree() -> (ast::ClassTree, rumoca_core::DefId) {
    let modelica_def = rumoca_core::DefId::new(1);
    let electrical_def = rumoca_core::DefId::new(2);
    let spice3_def = rumoca_core::DefId::new(3);
    let internal_def = rumoca_core::DefId::new(4);
    let mos_def = rumoca_core::DefId::new(5);
    let mos1_def = rumoca_core::DefId::new(6);
    let rename_def = rumoca_core::DefId::new(7);
    let semiconductors_def = rumoca_core::DefId::new(8);
    let pmos_def = rumoca_core::DefId::new(9);

    let rename = class(
        "mos1RenameParameters",
        rumoca_core::ClassType::Function,
        rename_def,
    );
    let mut mos1 = class("Mos1", rumoca_core::ClassType::Package, mos1_def);
    mos1.classes
        .insert("mos1RenameParameters".to_string(), rename);
    let mos = class("MOS", rumoca_core::ClassType::Model, mos_def);
    let mut internal = class("Internal", rumoca_core::ClassType::Package, internal_def);
    internal.classes.insert("MOS".to_string(), mos);
    internal.classes.insert("Mos1".to_string(), mos1);
    let mut spice3 = class("Spice3", rumoca_core::ClassType::Package, spice3_def);
    spice3.classes.insert("Internal".to_string(), internal);
    let mut pmos = class("M_PMOS", rumoca_core::ClassType::Model, pmos_def);
    pmos.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Modelica.Electrical.Spice3.Internal.MOS"),
        base_def_id: Some(mos_def),
        location: rumoca_core::Location::default(),
        modifications: Vec::new(),
        break_names: Vec::new(),
        is_protected: false,
        annotation: Vec::new(),
    });
    let mut semiconductors = class(
        "Semiconductors",
        rumoca_core::ClassType::Package,
        semiconductors_def,
    );
    semiconductors.classes.insert("M_PMOS".to_string(), pmos);
    spice3
        .classes
        .insert("Semiconductors".to_string(), semiconductors);
    let mut electrical = class(
        "Electrical",
        rumoca_core::ClassType::Package,
        electrical_def,
    );
    electrical.classes.insert("Spice3".to_string(), spice3);
    let mut modelica = class("Modelica", rumoca_core::ClassType::Package, modelica_def);
    modelica
        .classes
        .insert("Electrical".to_string(), electrical);

    let mut tree = ast::ClassTree::new();
    tree.definitions
        .classes
        .insert("Modelica".to_string(), modelica);
    (tree, rename_def)
}

fn assert_relative_package_function_call_is_exact(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    source_scope: &str,
    rename_def: rumoca_core::DefId,
) {
    let mut expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::with_component_reference(
            "Mos1.mos1RenameParameters",
            core_comp_ref(&["Mos1", "mos1RenameParameters"], rename_def),
        ),
        args: Vec::new(),
        is_constructor: false,
        span: test_span(),
    };

    canonicalize_function_calls_in_expression_with_scope(
        &mut expr,
        tree,
        class_index,
        Some(source_scope),
    );

    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        panic!("expected function call");
    };
    assert_eq!(
        name.as_str(),
        "Mos1.mos1RenameParameters",
        "canonical resolution must not replace the readable source occurrence spelling"
    );
    assert_eq!(name.target_def_id(), Some(rename_def));
    assert!(name.has_structure());
}

#[test]
fn test_collect_nested_function_calls() {
    let mut flat = flat::Model::new();

    // Create: 0 = outer(inner(x)) - y
    let inner_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("inner"),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("x"),
            subscripts: vec![],
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let outer_call = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("outer"),
        args: vec![inner_call],
        is_constructor: false,
        span: test_span(),
    };
    let residual = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: Box::new(outer_call),
        rhs: Box::new(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("y"),
            subscripts: vec![],
            span: test_span(),
        }),
        span: test_span(),
    };
    flat.add_equation(flat::Equation::new(
        residual,
        test_span(),
        rumoca_ir_flat::EquationOrigin::ComponentEquation {
            component: "test".to_string(),
        },
    ));

    let calls = collect_function_calls(&flat);
    assert!(calls.contains("inner"));
    assert!(calls.contains("outer"));
    assert_eq!(calls.len(), 2);
}

#[test]
fn validates_flat_boundary_rejects_non_executable_functions() {
    let mut flat = flat::Model::new();
    flat.add_function(rumoca_core::Function::new(
        "Modelica.Media.Interfaces.PartialMedium.setState_phX",
        test_span(),
    ));

    let err = validate_flat_function_bindings(&flat)
        .expect_err("flat boundary must reject abstract function bindings");

    assert!(matches!(
        err,
        FlattenError::FunctionWithoutBody { ref name, .. }
            if name == "Modelica.Media.Interfaces.PartialMedium.setState_phX"
    ));
}

#[test]
fn validates_flat_boundary_allows_constructors_without_body() {
    let mut flat = flat::Model::new();
    let mut constructor = rumoca_core::Function::new("RecordType", test_span());
    constructor.is_constructor = true;
    flat.add_function(constructor);

    validate_flat_function_bindings(&flat).expect("record constructors are structural");
}

#[test]
fn record_function_signature_keeps_constructor_as_structural_dependency() {
    let record_def_id = rumoca_core::DefId::new(41);
    let mut function = rumoca_core::Function::new("Pkg.makePose", test_span());
    function.add_output(
        crate::test_support::aggregate_param("pose", "Pkg.Pose", Vec::new(), test_span())
            .with_type_class(rumoca_core::ClassType::Record)
            .with_type_def_id(record_def_id),
    );

    let dependencies = collect_function_dep_requests(&function);
    let constructor = dependencies
        .iter()
        .find(|dependency| dependency.name == "Pkg.Pose")
        .expect("record constructor should remain a structural dependency");
    assert_eq!(
        constructor.target_def_id,
        Some(record_def_id),
        "record constructor dependencies must preserve declaration identity"
    );
}

#[test]
fn contextualized_record_parameter_updates_declaration_identity() {
    let package_def = rumoca_core::DefId::new(1);
    let inherited_state_def = rumoca_core::DefId::new(2);
    let concrete_state_def = rumoca_core::DefId::new(3);
    let mut package = class("Pkg", rumoca_core::ClassType::Package, package_def);
    package.classes.insert(
        "State".to_string(),
        class("State", rumoca_core::ClassType::Record, concrete_state_def),
    );
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_input(
        crate::test_support::aggregate_param("state", "Pkg.State", Vec::new(), test_span())
            .with_type_class(rumoca_core::ClassType::Record)
            .with_type_def_id(inherited_state_def),
    );

    contextualize_record_param_type_names(&tree, &class_index, "Pkg.f", &mut function).unwrap();

    assert_eq!(function.inputs[0].type_name, "Pkg.State");
    assert_eq!(function.inputs[0].type_def_id, Some(concrete_state_def));
}

#[test]
fn contextualized_record_parameter_follows_replaceable_type_alias() {
    let package_def = rumoca_core::DefId::new(11);
    let record_def = rumoca_core::DefId::new(12);
    let alias_def = rumoca_core::DefId::new(13);
    let mut package = class("Pkg", rumoca_core::ClassType::Package, package_def);
    package.classes.insert(
        "Quaternion".to_string(),
        class("Quaternion", rumoca_core::ClassType::Record, record_def),
    );
    let mut alias = class("Orientation", rumoca_core::ClassType::Type, alias_def);
    alias.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Pkg.Quaternion"),
        base_def_id: Some(record_def),
        ..Default::default()
    });
    package.classes.insert("Orientation".to_string(), alias);

    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_input(
        crate::test_support::aggregate_param("orientation", "Orientation", Vec::new(), test_span())
            .with_type_class(rumoca_core::ClassType::Record)
            .with_type_def_id(record_def),
    );

    contextualize_record_param_type_names(&tree, &class_index, "Pkg.f", &mut function).unwrap();

    assert_eq!(function.inputs[0].type_name, "Pkg.Orientation");
    assert_eq!(function.inputs[0].type_def_id, Some(alias_def));
}

#[test]
fn contextualized_record_parameter_uses_resolved_identity_for_lexical_alias() {
    let package_def = rumoca_core::DefId::new(21);
    let record_def = rumoca_core::DefId::new(22);
    let mut package = class("Pkg", rumoca_core::ClassType::Package, package_def);
    package.classes.insert(
        "ComplexVoltage".to_string(),
        class("ComplexVoltage", rumoca_core::ClassType::Record, record_def),
    );
    let mut tree = ast::ClassTree::new();
    tree.definitions.classes.insert("Pkg".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut function = rumoca_core::Function::new("Pkg.f", test_span());
    function.add_input(
        crate::test_support::aggregate_param(
            "voltage",
            "SI.ComplexVoltage",
            Vec::new(),
            test_span(),
        )
        .with_type_class(rumoca_core::ClassType::Record)
        .with_type_def_id(record_def),
    );

    contextualize_record_param_type_names(&tree, &class_index, "Pkg.f", &mut function).unwrap();

    assert_eq!(function.inputs[0].type_name, "Pkg.ComplexVoltage");
    assert_eq!(function.inputs[0].type_def_id, Some(record_def));
}

#[test]
fn validates_flat_boundary_allows_output_binding_functions() {
    let mut flat = flat::Model::new();
    let mut function =
        rumoca_core::Function::new("Modelica.Math.BooleanVectors.anyTrue", test_span());
    function.add_output(
        crate::test_support::boolean_param("result", Vec::new(), test_span()).with_default(
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: test_span(),
            },
        ),
    );
    flat.add_function(function);

    validate_flat_function_bindings(&flat)
        .expect("functions with output declaration bindings are executable");
}

#[test]
fn test_convert_component_to_param_prefers_binding_over_start_default() {
    let mut component = ast::Component {
        type_name: ast::Name::from_string("Real"),
        location: test_location(9, 15),
        has_explicit_binding: true,
        start: ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: "0".into(),
                ..Default::default()
            },
            span: test_span(),
        },
        binding: Some(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: "3".into(),
                ..Default::default()
            },
            span: test_span(),
        }),
        ..ast::Component::empty_with_span(test_span())
    };

    let source_map = test_source_map();
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    component.type_name.def_id = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Integer"));
    component.type_def_id = component.type_name.def_id;
    let type_overlay = crate::test_support::type_overlay(&tree);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let param = convert_component_to_param(
        &class_index,
        "m",
        &component,
        &source_map,
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();
    assert!(matches!(
        param.default,
        Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            ..
        })
    ));
}

fn integer_subscript(value: i64) -> ast::Subscript {
    ast::Subscript::Expression(ast::Expression::Terminal {
        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: value.to_string().into(),
            ..Default::default()
        },
        span: test_span(),
    })
}

fn size_subscript(
    reference: &str,
    reference_def_id: rumoca_core::DefId,
    dimension: i64,
) -> ast::Subscript {
    let predefined_size_def_id = rumoca_core::DefId::new(4200);
    ast::Subscript::Expression(ast::Expression::FunctionCall {
        comp: ast_comp_ref(&["size"], predefined_size_def_id),
        args: vec![
            ast::Expression::ComponentReference(ast_comp_ref(&[reference], reference_def_id)),
            ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token {
                    text: dimension.to_string().into(),
                    ..Default::default()
                },
                span: test_span(),
            },
        ],
        is_partial_application: false,
        span: test_span(),
    })
}

#[test]
fn test_convert_component_to_param_preserves_mixed_dynamic_rank() {
    let c0_def_id = rumoca_core::DefId::new(4201);
    let mut component = ast::Component {
        type_name: ast::Name::from_string("Real"),
        location: test_location(9, 15),
        shape: vec![2],
        shape_expr: vec![size_subscript("c0", c0_def_id, 1), integer_subscript(2)],
        ..ast::Component::empty_with_span(test_span())
    };

    let source_map = test_source_map();
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    component.type_name.def_id = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"));
    component.type_def_id = component.type_name.def_id;
    let type_overlay = crate::test_support::type_overlay(&tree);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let param = convert_component_to_param(
        &class_index,
        "den2",
        &component,
        &source_map,
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dimensions(), [0, 2]);
    assert_eq!(param.shape_expr.len(), 2);
}

#[test]
fn test_convert_component_to_param_resolves_constant_shape_expr() {
    let package_def = rumoca_core::DefId::new(10);
    let n_state_def = rumoca_core::DefId::new(11);
    let mut package = class(
        "Xorshift64star",
        rumoca_core::ClassType::Package,
        package_def,
    );
    package.components.insert(
        "nState".to_string(),
        ast::Component {
            def_id: Some(n_state_def),
            type_name: ast::Name::from_string("Integer"),
            variability: rumoca_core::Variability::Constant(token("constant")),
            has_explicit_binding: true,
            binding: Some(ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: token("2"),
                span: test_span(),
            }),
            ..ast::Component::empty_with_span(test_span())
        },
    );

    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.definitions
        .classes
        .insert("Xorshift64star".to_string(), package);
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);
    let n_state_ref = ast_comp_ref(&["nState"], n_state_def);
    let mut component = ast::Component {
        type_name: ast::Name::from_string("Integer"),
        location: test_location(9, 15),
        shape_expr: vec![ast::Subscript::Expression(
            ast::Expression::ComponentReference(n_state_ref),
        )],
        ..ast::Component::empty_with_span(test_span())
    };
    component.type_name.def_id = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Integer"));
    component.type_def_id = component.type_name.def_id;
    let param = convert_component_to_param(
        &class_index,
        "state",
        &component,
        &test_source_map(),
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dimensions(), [2]);
    assert!(matches!(
        param.shape_expr.as_slice(),
        [rumoca_core::Subscript::Index { value: 2, .. }]
    ));
}

#[test]
fn test_convert_component_to_param_inherits_type_alias_dims() {
    let orientation_id = rumoca_core::DefId::new(1);
    let matrix_id = rumoca_core::DefId::new(2);

    let mut matrix = ast::ClassDef {
        def_id: Some(matrix_id),
        name: rumoca_core::Token {
            text: "TransformationMatrix".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Type,
        array_subscripts: vec![integer_subscript(3), integer_subscript(3)],
        ..Default::default()
    };
    matrix.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Real"),
        ..Default::default()
    });

    let mut orientation = ast::ClassDef {
        def_id: Some(orientation_id),
        name: rumoca_core::Token {
            text: "Orientation".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Type,
        ..Default::default()
    };
    orientation.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Pkg.Internal.TransformationMatrix"),
        base_def_id: Some(matrix_id),
        ..Default::default()
    });

    let mut package = ast::ClassDef {
        name: rumoca_core::Token {
            text: "Pkg".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    let mut internal = ast::ClassDef {
        name: rumoca_core::Token {
            text: "Internal".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        ..Default::default()
    };
    internal
        .classes
        .insert("TransformationMatrix".to_string(), matrix);
    package.classes.insert("Internal".to_string(), internal);
    package
        .classes
        .insert("Orientation".to_string(), orientation);

    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.definitions.classes.insert("Pkg".to_string(), package);
    tree.def_map
        .insert(orientation_id, "Pkg.Orientation".to_string());
    tree.def_map
        .insert(matrix_id, "Pkg.Internal.TransformationMatrix".to_string());

    let mut type_name = ast::Name::from_string("Pkg.Orientation");
    type_name.def_id = Some(orientation_id);
    let component = ast::Component {
        type_name,
        location: test_location(9, 15),
        ..ast::Component::empty_with_span(test_span())
    };
    let source_map = test_source_map();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);
    let param = convert_component_to_param(
        &class_index,
        "T",
        &component,
        &source_map,
        FunctionExpressionContext {
            predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
            type_catalog: FunctionTypeCatalog::new(&type_overlay),
        },
        &qualify::ImportMap::default(),
        &HashSet::new(),
    )
    .unwrap();

    assert_eq!(param.dimensions(), [3, 3]);
    assert_eq!(param.type_name, "Real");
}

#[test]
fn test_constructor_signature_preserves_local_default_references() {
    let orientation_def = rumoca_core::DefId::new(1);
    let n_def = rumoca_core::DefId::new(2);
    let mut class_def = ast::ClassDef {
        name: rumoca_core::Token {
            text: "C".into(),
            ..Default::default()
        },
        location: test_location(0, 8),
        class_type: rumoca_core::ClassType::Record,
        components: ast::AstIndexMap::from_iter([
            (
                "orientation".to_string(),
                ast::Component {
                    name: "orientation".to_string(),
                    def_id: Some(orientation_def),
                    type_name: ast::Name::from_string("Real"),
                    location: test_location(11, 27),
                    ..ast::Component::empty_with_span(test_span())
                },
            ),
            (
                "N".to_string(),
                ast::Component {
                    name: "N".to_string(),
                    def_id: Some(n_def),
                    type_name: ast::Name::from_string("Real"),
                    location: test_location(30, 37),
                    has_explicit_binding: true,
                    binding: Some(ast::Expression::ComponentReference(ast_comp_ref(
                        &["orientation"],
                        orientation_def,
                    ))),
                    ..ast::Component::empty_with_span(test_span())
                },
            ),
        ]),
        ..Default::default()
    };
    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.def_map
        .insert(orientation_def, "Pkg.C.orientation".to_string());
    tree.def_map.insert(n_def, "Pkg.C.N".to_string());
    let real_def = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"))
        .expect("fixture tree owns predefined Real");
    for component in class_def.components.values_mut() {
        component.type_name.def_id = Some(real_def);
        component.type_def_id = Some(real_def);
    }
    let source_map = test_source_map();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);

    let constructor = convert_constructor_signature(
        &class_index,
        &class_def,
        "Pkg.C",
        &source_map,
        ast_lower::PredefinedIntrinsicIds::from_tree(&tree),
        FunctionTypeCatalog::new(&type_overlay),
    )
    .unwrap();

    let n_param = constructor
        .inputs
        .iter()
        .find(|param| param.name == "N")
        .expect("N constructor input");
    assert!(matches!(
        n_param.default,
        Some(rumoca_core::Expression::VarRef { ref name, .. }) if name.as_str() == "orientation"
    ));
}

#[test]
fn record_type_fields_preserve_short_operator_record_base_fields() {
    let complex_def = rumoca_core::DefId::new(101);
    let re_def = rumoca_core::DefId::new(102);
    let im_def = rumoca_core::DefId::new(103);
    let flux_def = rumoca_core::DefId::new(104);
    let mut complex = class("Complex", rumoca_core::ClassType::Record, complex_def);
    complex.operator_record = true;
    complex.location = test_location(0, 8);
    for (name, def_id, location) in [
        ("re", re_def, test_location(11, 27)),
        ("im", im_def, test_location(30, 37)),
    ] {
        complex.components.insert(
            name.to_string(),
            ast::Component {
                name: name.to_string(),
                def_id: Some(def_id),
                type_name: ast::Name::from_string("Real"),
                location,
                ..ast::Component::empty_with_span(test_span())
            },
        );
    }
    let mut flux = class(
        "ComplexMagneticFlux",
        rumoca_core::ClassType::Record,
        flux_def,
    );
    flux.operator_record = true;
    flux.location = test_location(0, 8);
    flux.extends.push(ast::Extend {
        base_name: ast::Name::from_string("Complex"),
        base_def_id: Some(complex_def),
        ..Default::default()
    });

    let mut tree = ast::ClassTree::new();
    crate::test_support::install_predefined_type_identities(&mut tree);
    tree.source_map = test_source_map();
    tree.definitions
        .classes
        .insert("Complex".to_string(), complex);
    tree.definitions
        .classes
        .insert("ComplexMagneticFlux".to_string(), flux);
    let real_def = tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"))
        .expect("fixture tree owns predefined Real");
    for class in tree.definitions.classes.values_mut() {
        for component in class.components.values_mut() {
            component.type_name.def_id = Some(real_def);
            component.type_def_id = Some(real_def);
        }
    }
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let type_overlay = crate::test_support::type_overlay(&tree);
    let flux = class_index
        .get(flux_def)
        .expect("derived operator record class");

    let fields = record_type_fields(
        &class_index,
        flux,
        "ComplexMagneticFlux",
        &tree,
        FunctionTypeCatalog::new(&type_overlay),
    )
    .expect("resolved record field metadata");

    assert_eq!(
        fields
            .iter()
            .map(|field| (field.name.as_str(), field.def_id))
            .collect::<Vec<_>>(),
        vec![("re", re_def), ("im", im_def)]
    );
}

#[test]
fn test_function_local_normalization_rewrites_self_qualified_default() {
    let orientation_def = rumoca_core::DefId::new(14_003);
    let mut function = rumoca_core::Function::new("Pkg.C", test_span());
    function.add_input(
        crate::test_support::real_param("orientation", Vec::new(), test_span())
            .with_def_id(orientation_def),
    );
    function.add_input(
        crate::test_support::real_param("N", Vec::new(), test_span())
            .with_def_id(rumoca_core::DefId::new(14_004))
            .with_default(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::with_component_reference(
                    "Pkg.C.orientation",
                    core_structured_comp_ref(&[
                        ("Pkg", rumoca_core::DefId::new(14_001)),
                        ("C", rumoca_core::DefId::new(14_002)),
                        ("orientation", orientation_def),
                    ]),
                ),
                subscripts: vec![],
                span: test_span(),
            }),
    );

    normalize_function_local_references(&mut function);

    let Some(rumoca_core::Expression::VarRef { name, .. }) = &function.inputs[1].default else {
        panic!("expected normalized reference default");
    };
    assert_eq!(name.as_str(), "orientation");
    assert_eq!(name.target_def_id(), Some(orientation_def));
    assert!(
        name.parts()
            .iter()
            .all(|part| part.def_id.index() != 0 && !part.span.is_dummy())
    );
}

#[test]
fn test_function_context_inherits_base_lexical_imports() {
    let (tree, derived_function) = function_context_inheritance_tree();
    let class_index = ast::ClassDefIndex::from_tree(&tree);
    let mut member_cache = qualify::MemberDefIdCache::default();
    let context =
        collect_function_context(&tree, &class_index, &derived_function, &mut member_cache);

    assert_eq!(
        context.imports.get("pi").map(String::as_str),
        Some("Modelica.Constants.pi")
    );
    assert!(context.components.contains_key("crossArea"));
}

fn function_context_inheritance_tree() -> (ast::ClassTree, ast::ClassDef) {
    let base_def = rumoca_core::DefId::new(1);
    let derived_def = rumoca_core::DefId::new(2);
    let root_package_def = rumoca_core::DefId::new(3);
    let base_package_def = rumoca_core::DefId::new(4);
    let derived_package_def = rumoca_core::DefId::new(5);

    let base_function = base_pressure_loss_function(base_def);
    let derived_function = ast::ClassDef {
        def_id: Some(derived_def),
        name: rumoca_core::Token {
            text: "pressureLoss".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Function,
        extends: vec![ast::Extend {
            base_name: ast::Name::from_string("P.Base.pressureLoss"),
            base_def_id: Some(base_def),
            ..Default::default()
        }],
        ..Default::default()
    };
    let base_package = ast::ClassDef {
        def_id: Some(base_package_def),
        name: rumoca_core::Token {
            text: "Base".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        imports: vec![ast::Import::Qualified {
            path: ast::Name::from_string("Modelica.Constants.pi"),
            location: rumoca_core::Location::default(),
            global_scope: false,
        }],
        classes: ast::AstIndexMap::from_iter([("pressureLoss".to_string(), base_function)]),
        ..Default::default()
    };
    let derived_package = ast::ClassDef {
        def_id: Some(derived_package_def),
        name: rumoca_core::Token {
            text: "Derived".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        classes: ast::AstIndexMap::from_iter([(
            "pressureLoss".to_string(),
            derived_function.clone(),
        )]),
        ..Default::default()
    };
    let root_package = ast::ClassDef {
        def_id: Some(root_package_def),
        name: rumoca_core::Token {
            text: "P".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Package,
        classes: ast::AstIndexMap::from_iter([
            ("Base".to_string(), base_package),
            ("Derived".to_string(), derived_package),
        ]),
        ..Default::default()
    };

    let mut tree = ast::ClassTree::default();
    tree.definitions
        .classes
        .insert("P".to_string(), root_package);
    register_function_context_inheritance_names(
        &mut tree,
        [
            (root_package_def, "P"),
            (base_package_def, "P.Base"),
            (derived_package_def, "P.Derived"),
            (base_def, "P.Base.pressureLoss"),
            (derived_def, "P.Derived.pressureLoss"),
        ],
    );
    (tree, derived_function)
}

fn base_pressure_loss_function(base_def: rumoca_core::DefId) -> ast::ClassDef {
    ast::ClassDef {
        def_id: Some(base_def),
        name: rumoca_core::Token {
            text: "pressureLoss".into(),
            ..Default::default()
        },
        class_type: rumoca_core::ClassType::Function,
        components: ast::AstIndexMap::from_iter([("crossArea".to_string(), pi_component())]),
        ..Default::default()
    }
}

fn pi_component() -> ast::Component {
    ast::Component {
        name: "crossArea".to_string(),
        type_name: ast::Name::from_string("Real"),
        binding: Some(ast::Expression::ComponentReference(ast_comp_ref(
            &["pi"],
            rumoca_core::DefId::new(4300),
        ))),
        ..ast::Component::empty_with_span(test_span())
    }
}

fn register_function_context_inheritance_names(
    tree: &mut ast::ClassTree,
    names: [(rumoca_core::DefId, &str); 5],
) {
    for (def_id, name) in names {
        tree.name_map.insert(name.to_string(), def_id);
        tree.def_map.insert(def_id, name.to_string());
    }
}

#[test]
fn external_function_metadata_preserves_library_and_include_annotations() {
    let source = r##"
pure function Linked
  input Real u;
  output Real y;
external "C" y = linked_call(u)
  annotation(
    Library = {"Linked", "Support"},
    Include = "#include \"linked.h\"");
end Linked;
"##;
    let resolved = resolve_test_source(source, "external_annotations.mo");
    let external = resolved
        .inner()
        .definitions
        .classes
        .get("Linked")
        .and_then(|class| class.external.as_ref())
        .expect("external declaration");
    let metadata = convert_external_function(
        external,
        ast_lower::PredefinedIntrinsicIds::from_tree(resolved.inner()),
    )
    .expect("external annotations should lower without loss");

    let [library, include] = metadata.annotations.as_slice() else {
        panic!("expected Library and Include metadata");
    };
    assert_eq!(library.name, ["Library"]);
    let rumoca_core::Expression::Array { elements, .. } = &library.value else {
        panic!("Library must retain its array expression");
    };
    assert!(matches!(
        elements.as_slice(),
        [
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(first),
                ..
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(second),
                ..
            }
        ] if first == "Linked" && second == "Support"
    ));

    assert_eq!(include.name, ["Include"]);
    assert!(matches!(
        &include.value,
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value),
            ..
        } if value == "#include \"linked.h\""
    ));
}

#[test]
fn external_function_metadata_rejects_annotation_syntax_it_cannot_preserve() {
    let source = r#"
pure function Linked
  input Real u;
  output Real y;
external "C" y = linked_call(u)
  annotation(__Vendor(options = 1));
end Linked;
"#;
    let resolved = resolve_test_source(source, "external_annotations.mo");
    let external = resolved
        .inner()
        .definitions
        .classes
        .get("Linked")
        .and_then(|class| class.external.as_ref())
        .expect("external declaration");
    let error = convert_external_function(
        external,
        ast_lower::PredefinedIntrinsicIds::from_tree(resolved.inner()),
    )
    .expect_err("unrepresentable annotation syntax must fail instead of being dropped");

    assert!(
        error
            .to_string()
            .contains("unsupported external-function annotation")
    );
}

#[test]
fn external_function_metadata_preserves_ordered_argument_expressions() {
    let source = r#"
pure function Linked
  input Real u;
  input Real v;
  output Real y;
external "C" y = linked_call(u, size({u, v}, 1), 2.0 * v);
end Linked;
"#;
    let resolved = resolve_test_source(source, "external_arguments.mo");
    let external = resolved
        .inner()
        .definitions
        .classes
        .get("Linked")
        .and_then(|class| class.external.as_ref())
        .expect("external declaration");
    let lowered = convert_external_function(
        external,
        ast_lower::PredefinedIntrinsicIds::from_tree(resolved.inner()),
    )
    .expect("all external ABI arguments must lower without loss");

    assert_eq!(lowered.args.len(), 3);
    assert!(matches!(
        lowered.args[0],
        rumoca_core::Expression::VarRef { .. }
    ));
    assert!(matches!(
        lowered.args[1],
        rumoca_core::Expression::BuiltinCall { .. }
    ));
    assert!(matches!(
        lowered.args[2],
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            ..
        }
    ));
    assert!(
        lowered
            .args
            .iter()
            .all(|argument| argument.span().is_some_and(|span| !span.is_dummy())),
        "every ABI argument must retain its exact source provenance"
    );
}

#[test]
fn test_extract_derivative_annotation_simple() {
    use rumoca_core::Token;
    use std::sync::Arc;

    // Test: annotation(derivative = myFunc_der)
    let derivative_function_def_id = rumoca_core::DefId::new(4400);
    let annotations = vec![ast::Expression::NamedArgument {
        name: Token {
            text: Arc::from("derivative"),
            ..Default::default()
        },
        value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
            &["myFunc_der"],
            derivative_function_def_id,
        ))),
        span: test_span(),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der");
    assert_eq!(derivs[0].order, 1);
    assert!(derivs[0].zero_derivative.is_empty());
    assert!(derivs[0].no_derivative.is_empty());
}

#[test]
fn test_extract_derivative_annotation_with_modification() {
    use rumoca_core::Token;
    use std::sync::Arc;

    // Test: annotation(derivative(order=2) = myFunc_der2)
    // This is represented as a Modification with target having subscripts
    let derivative_annotation_def_id = rumoca_core::DefId::new(4401);
    let derivative_function_def_id = rumoca_core::DefId::new(4402);
    let annotations = vec![ast::Expression::Modification {
        target: ast_comp_ref_with_subscripts(
            &["derivative"],
            derivative_annotation_def_id,
            vec![ast::Subscript::Expression(ast::Expression::NamedArgument {
                name: Token {
                    text: Arc::from("order"),
                    ..Default::default()
                },
                value: Arc::new(ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                    token: Token {
                        text: Arc::from("2"),
                        ..Default::default()
                    },
                    span: test_span(),
                }),
                span: test_span(),
            })],
        ),
        value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
            &["myFunc_der2"],
            derivative_function_def_id,
        ))),
        span: test_span(),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der2");
    assert_eq!(derivs[0].order, 2);
}

#[test]
fn test_extract_derivative_annotation_with_zero_derivative() {
    use rumoca_core::Token;
    use std::sync::Arc;

    // Test: annotation(derivative(zeroDerivative=k) = myFunc_der)
    let derivative_annotation_def_id = rumoca_core::DefId::new(4403);
    let derivative_function_def_id = rumoca_core::DefId::new(4404);
    let zero_derivative_param_def_id = rumoca_core::DefId::new(4405);
    let annotations = vec![ast::Expression::Modification {
        target: ast_comp_ref_with_subscripts(
            &["derivative"],
            derivative_annotation_def_id,
            vec![ast::Subscript::Expression(ast::Expression::NamedArgument {
                name: Token {
                    text: Arc::from("zeroDerivative"),
                    ..Default::default()
                },
                value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
                    &["k"],
                    zero_derivative_param_def_id,
                ))),
                span: test_span(),
            })],
        ),
        value: Arc::new(ast::Expression::ComponentReference(ast_comp_ref(
            &["myFunc_der"],
            derivative_function_def_id,
        ))),
        span: test_span(),
    }];

    let derivs = extract_derivative_annotations(&annotations);
    assert_eq!(derivs.len(), 1);
    assert_eq!(derivs[0].derivative_function, "myFunc_der");
    assert_eq!(derivs[0].order, 1);
    assert_eq!(derivs[0].zero_derivative, vec!["k"]);
}
