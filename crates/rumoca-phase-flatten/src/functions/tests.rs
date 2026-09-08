mod call_kind;
mod function_context_and_metadata;
mod lexical_exposures;
mod type_identity;

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
    let mut function = rumoca_core::Function::new(
        "Modelica.Math.Polynomials.fitting",
        rumoca_core::DefId::new(62_001),
        test_span(),
    );
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(function);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Polynomials.fitting"),
            args: vec![],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
        constructor_def_id,
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
                call_kind: rumoca_core::FunctionCallKind::Invocation,
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
fn reachable_constructor_retains_record_layout_without_component_instance() {
    let record_def_id = rumoca_core::DefId::new(42);
    let field_def_id = rumoca_core::DefId::new(43);
    let field_type_def_id = rumoca_core::DefId::new(44);
    let mut constructor =
        rumoca_core::Function::new("Pkg.ReturnOnlyRecord", record_def_id, test_span());
    constructor.def_id = Some(record_def_id);
    constructor.is_constructor = true;
    let mut field =
        crate::test_support::real_param("values", vec![3], test_span()).with_def_id(field_def_id);
    field.type_def_id = Some(field_type_def_id);
    let field_type = field.effective_type.clone();
    constructor.add_input(field);
    let mut flat = flat::Model::new();

    retain_constructor_record_type(&mut flat, &constructor)
        .expect("constructor signature owns its compact record layout");

    assert_eq!(
        flat.record_types.get(&record_def_id),
        Some(&flat::RecordType {
            name: "Pkg.ReturnOnlyRecord".to_string(),
            fields: vec![flat::RecordField {
                name: "values".to_string(),
                def_id: field_def_id,
                type_def_id: field_type_def_id,
                effective_type: field_type,
                dims: vec![3],
            }],
        })
    );
}

#[test]
fn exact_instance_completes_valid_nested_constructor_field_projection() {
    let mut flat = flat::Model::new();
    let field_def_id = rumoca_core::DefId::new(45);
    let mut constructor = rumoca_core::Function::new(
        "Pkg.SyntheticRecord",
        rumoca_core::DefId::new(44),
        test_span(),
    );
    constructor.def_id = Some(rumoca_core::DefId::new(44));
    constructor.is_constructor = true;
    constructor.add_input(
        crate::test_support::real_param("field", Vec::new(), test_span()).with_def_id(field_def_id),
    );
    flat.add_function(constructor);
    let constructor_instance = flat.functions[&rumoca_core::VarName::new("Pkg.SyntheticRecord")]
        .instance_id
        .expect("Flat assigns the canonical constructor instance");

    let mut read =
        rumoca_core::Function::new("Pkg.read", rumoca_core::DefId::new(62_002), test_span());
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
        call_kind: rumoca_core::FunctionCallKind::Invocation,
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut constructor = rumoca_core::Function::new(
        "Pkg.Shared.constructor",
        rumoca_core::DefId::new(62_003),
        test_span(),
    );
    constructor.def_id = Some(shared_def_id);
    constructor.is_constructor = true;
    flat.add_function(constructor);
    let constructor_instance = flat.functions[&rumoca_core::VarName::new("Pkg.Shared.constructor")]
        .instance_id
        .expect("Flat assigns the constructor exposure identity");

    let mut regular = rumoca_core::Function::new(
        "Pkg.Shared.regular",
        rumoca_core::DefId::new(62_004),
        test_span(),
    );
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
                call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut constructor =
        rumoca_core::Function::new("Pkg.UnresolvedConstructor", constructor_def_id, test_span());
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
                call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut function =
        rumoca_core::Function::new("Pkg.regular", rumoca_core::DefId::new(62_005), test_span());
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
        rumoca_core::DefId::new(62_006),
        test_span(),
    );
    partial.def_id = Some(function_def_id);
    partial
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(partial);
    let mut concrete = rumoca_core::Function::new(
        "Modelica.Media.Air.ReferenceMoistAir.setState_pTX",
        function_def_id,
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut constructor = rumoca_core::Function::new("P.Concrete.Element", record_def, test_span());
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
                call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut function = rumoca_core::Function::new(
        "Pkg.Events.trip",
        rumoca_core::DefId::new(62_007),
        test_span(),
    );
    function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(function);

    let mut branch = flat::WhenBranch::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Pkg.Events.trip"),
            args: vec![],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: test_span(),
            },
            vec![flat::WhenEquation::FunctionCallOutputs {
                outputs: vec![rumoca_core::VarName::new("y")],
                function: rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::new("Pkg.Events.trip"),
                    args: vec![],
                    is_constructor: false,
                    call_kind: rumoca_core::FunctionCallKind::Invocation,
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
                call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut math_function = rumoca_core::Function::new(
        "Modelica.Math.Polynomials.fitting",
        rumoca_core::DefId::new(62_008),
        test_span(),
    );
    math_function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(math_function);
    let mut user_function = rumoca_core::Function::new(
        "User.Polynomials.fitting",
        rumoca_core::DefId::new(62_009),
        test_span(),
    );
    user_function
        .body
        .push(rumoca_core::Statement::Return { span: test_span() });
    flat.add_function(user_function);
    flat.add_equation(flat::Equation::new(
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("Polynomials.fitting"),
            args: vec![],
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
        let exposure = if name == "Pkg.A.f" {
            rumoca_core::DefId::new(62_010)
        } else {
            rumoca_core::DefId::new(62_011)
        };
        let mut function = rumoca_core::Function::new(name, exposure, test_span());
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
            // This identity-only fixture intentionally supplies no class tree.
            // Canonicalization may distinguish the inherited exposures, but it
            // must not fabricate the separate MLS §6.4 certificate.
            transitively_non_replaceable: false,
        })
    );
}

#[test]
fn canonicalize_collected_function_calls_prefers_exact_name_over_stale_def_id() {
    let inherited_def_id = rumoca_core::DefId::new(901);
    let flattened_def_id = rumoca_core::DefId::new(902);
    let mut flat = flat::Model::new();
    let mut function =
        rumoca_core::Function::new("Pkg.Medium.density", flattened_def_id, test_span());
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(62_012), test_span());
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
    let mut function =
        rumoca_core::Function::new("Pkg.f", rumoca_core::DefId::new(62_013), test_span());
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
        call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let mut function =
        rumoca_core::Function::new("Pkg.wrapper", rumoca_core::DefId::new(62_014), test_span());
    function.body.push(rumoca_core::Statement::Assignment {
        comp: core_comp_ref(&["y"], y_def_id),
        value: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::with_component_reference(
                "Alias.f",
                core_comp_ref(&["Alias", "f"], target_def_id),
            ),
            args: Vec::new(),
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
            call_kind: rumoca_core::FunctionCallKind::Invocation,
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
    let semantic_catalogs = crate::test_support::semantic_catalog_projection();
    let (resolved_name, function) = lookup_function_request(
        &tree,
        &class_index,
        &request,
        FunctionTypeCatalog::new(&type_overlay, &semantic_catalogs),
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

mod exact_call_cases;
use exact_call_cases::integer_subscript;
