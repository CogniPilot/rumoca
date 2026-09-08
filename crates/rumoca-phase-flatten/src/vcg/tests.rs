use super::*;
use rumoca_core::{SourceId, Span, Token};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;
use std::sync::Arc;

fn test_span(start: usize, end: usize) -> Span {
    Span::from_offsets(SourceId::from_source_name("vcg_test.mo"), start, end)
}

fn token(text: &str) -> Token {
    Token {
        text: Arc::from(text),
        ..Default::default()
    }
}

fn component_ref(parts: &[&str], span: Span) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: parts
            .iter()
            .enumerate()
            .map(|(index, part)| ast::ComponentRefPart {
                ident: token(part),
                subs: None,
                def_id: Some(rumoca_core::DefId::new(18_001 + index as u32)),
            })
            .collect(),
        span,
        qualified_display_name: None,
    }
}

fn connection_operator_ref(
    parts: &[&str],
    span: Span,
    role: rumoca_core::ConnectionGraphOperatorRole,
    operators: &ast::ConnectionOperatorCatalog,
) -> ast::ComponentReference {
    let mut reference = component_ref(parts, span);
    reference
        .parts
        .last_mut()
        .expect("fixture operator reference is nonempty")
        .def_id = Some(operators.declaration(role));
    reference
}

fn integer_literal(value: i64, span: Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: token(&value.to_string()),
        span,
    }
}

fn bad_range(span: Span) -> ast::Expression {
    ast::Expression::Range {
        start: Arc::new(ast::Expression::ComponentReference(component_ref(
            &["n"],
            test_span(20, 21),
        ))),
        step: None,
        end: Arc::new(integer_literal(3, test_span(24, 25))),
        span,
    }
}

fn for_equation_with_call(call: ast::ComponentReference, range_span: Span) -> ast::Equation {
    ast::Equation::For {
        indices: vec![ast::ForIndex {
            ident: token("i"),
            range: bad_range(range_span),
        }],
        equations: vec![ast::Equation::FunctionCall {
            comp: call,
            args: vec![ast::Expression::ComponentReference(component_ref(
                &["frame", "R"],
                test_span(40, 47),
            ))],
            span: test_span(30, 48),
        }],
    }
}

#[test]
fn vcg_for_prescan_bubbles_range_errors_with_range_span() {
    let range_span = test_span(10, 15);
    let operators = crate::test_support::connection_operators();
    let equation = for_equation_with_call(
        connection_operator_ref(
            &["Connections", "root"],
            test_span(30, 46),
            rumoca_core::ConnectionGraphOperatorRole::Root,
            &operators,
        ),
        range_span,
    );
    let mut data = VcgPreScanData {
        definite_roots: FxHashSet::default(),
        branches: Vec::new(),
        branch_spans: Vec::new(),
        potential_roots: Vec::new(),
    };

    let err = collect_vcg_from_equation(
        &equation,
        &ast::QualifiedName::default(),
        &Context::default(),
        &operators,
        &mut data,
    )
    .expect_err("VCG pre-scan should reject unresolved source ranges");

    match err {
        FlattenError::UnsupportedEquation { span, .. } => {
            assert_eq!(span, range_span);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn vcg_for_prescan_does_not_evaluate_non_vcg_loop_ranges() {
    let operators = crate::test_support::connection_operators();
    let equation = for_equation_with_call(
        component_ref(&["Other", "root"], test_span(30, 40)),
        test_span(10, 15),
    );
    let mut data = VcgPreScanData {
        definite_roots: FxHashSet::default(),
        branches: Vec::new(),
        branch_spans: Vec::new(),
        potential_roots: Vec::new(),
    };

    collect_vcg_from_equation(
        &equation,
        &ast::QualifiedName::default(),
        &Context::default(),
        &operators,
        &mut data,
    )
    .expect("non-VCG loops should not be range-expanded by VCG pre-scan");

    assert!(data.definite_roots.is_empty());
    assert!(data.branches.is_empty());
    assert!(data.potential_roots.is_empty());
}

fn add_orientation_record(flat: &mut flat::Model, base: &str) {
    for (suffix, dims) in [("T", vec![3, 3]), ("w", vec![3])] {
        let name = rumoca_core::VarName::new(format!("{base}.{suffix}"));
        flat.add_variable(
            name.clone(),
            rumoca_ir_flat::Variable {
                name,
                dims,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span(1, 2))
            },
        );
    }
}

#[test]
fn test_compute_break_edge_scalar_count_orientation_cycle() {
    // Required edges form a tree; one optional edge closes the cycle.
    let branches = vec![
        ("a.R".to_string(), "b.R".to_string()),
        ("b.R".to_string(), "c.R".to_string()),
    ];
    let optional_edges = vec![("c.R".to_string(), "a.R".to_string())];
    let definite_roots: FxHashSet<String> = ["a.R".to_string()].into_iter().collect();
    let potential_roots: Vec<(String, i64)> = Vec::new();

    let mut flat = flat::Model::new();
    add_orientation_record(&mut flat, "a.R");
    add_orientation_record(&mut flat, "b.R");
    add_orientation_record(&mut flat, "c.R");

    let break_edge_scalars = compute_break_edge_scalar_count(
        &branches,
        &optional_edges,
        &definite_roots,
        &potential_roots,
        &flat,
    );
    assert_eq!(
        break_edge_scalars, 12,
        "one Orientation break edge should contribute 12 scalars (T[3,3] + w[3])"
    );
}

#[test]
fn test_compute_break_edge_scalar_count_multiple_definite_roots_form_forest() {
    let branches = vec![("a.R".to_string(), "b.R".to_string())];
    let optional_edges = vec![
        ("b.R".to_string(), "c.R".to_string()),
        ("c.R".to_string(), "a.R".to_string()),
    ];
    let definite_roots: FxHashSet<String> =
        ["a.R".to_string(), "c.R".to_string()].into_iter().collect();
    let potential_roots: Vec<(String, i64)> = Vec::new();

    let mut flat = flat::Model::new();
    add_orientation_record(&mut flat, "a.R");
    add_orientation_record(&mut flat, "b.R");
    add_orientation_record(&mut flat, "c.R");

    let break_edge_scalars = compute_break_edge_scalar_count(
        &branches,
        &optional_edges,
        &definite_roots,
        &potential_roots,
        &flat,
    );
    assert_eq!(
        break_edge_scalars, 24,
        "two definite roots in one component require a two-root forest, so two Orientation edges are broken"
    );
}

#[test]
fn test_build_vcg_keeps_distinct_definite_root_trees_separate() {
    let branches = vec![("a.R".to_string(), "b.R".to_string())];
    let optional_edges = vec![("b.R".to_string(), "c.R".to_string())];
    let definite_roots: FxHashSet<String> =
        ["a.R".to_string(), "c.R".to_string()].into_iter().collect();
    let data = VcgPreScanData {
        definite_roots,
        branch_spans: vec![test_span(1, 2)],
        branches,
        potential_roots: Vec::new(),
    };

    let required_forest =
        RequiredEdgeForest::construct(&data, &optional_edges).expect("valid required forest");
    let vcg = build_vcg(&data, &optional_edges, &required_forest);

    assert_eq!(vcg.is_root.get("a.R"), Some(&true));
    assert_eq!(vcg.is_root.get("c.R"), Some(&true));
    assert_eq!(vcg.rooted.get("a.R"), Some(&true));
    assert_eq!(vcg.rooted.get("b.R"), Some(&false));
}

#[test]
fn test_build_vcg_computes_rooted_relative_to_each_branch() {
    let branches = vec![
        ("a.R".to_string(), "b.R".to_string()),
        ("b.R".to_string(), "c.R".to_string()),
    ];
    let optional_edges: Vec<(String, String)> = Vec::new();

    let root_at_c: FxHashSet<String> = ["c.R".to_string()].into_iter().collect();
    let data = VcgPreScanData {
        definite_roots: root_at_c,
        branch_spans: vec![test_span(1, 2), test_span(3, 4)],
        branches: branches.clone(),
        potential_roots: Vec::new(),
    };
    let required_forest =
        RequiredEdgeForest::construct(&data, &optional_edges).expect("valid required forest");
    let vcg = build_vcg(&data, &optional_edges, &required_forest);
    assert_eq!(vcg.rooted.get("a.R"), Some(&false));
    assert_eq!(vcg.rooted.get("b.R"), Some(&false));

    let root_at_a: FxHashSet<String> = ["a.R".to_string()].into_iter().collect();
    let data = VcgPreScanData {
        definite_roots: root_at_a,
        branch_spans: vec![test_span(1, 2), test_span(3, 4)],
        branches,
        potential_roots: Vec::new(),
    };
    let required_forest =
        RequiredEdgeForest::construct(&data, &optional_edges).expect("valid required forest");
    let vcg = build_vcg(&data, &optional_edges, &required_forest);
    assert_eq!(vcg.rooted.get("a.R"), Some(&true));
    assert_eq!(vcg.rooted.get("b.R"), Some(&true));
}

#[test]
fn test_component_oc_record_scalar_count_uses_max_node_size() {
    let mut flat = flat::Model::default();
    for (base, dims) in [("a.R", vec![1]), ("b.R", vec![3]), ("c.R", vec![2])] {
        let name = rumoca_core::VarName::new(format!("{base}.x"));
        flat.add_variable(
            name.clone(),
            rumoca_ir_flat::Variable {
                name,
                dims,
                is_primitive: true,
                ..flat::Variable::empty_with_span(test_span(1, 2))
            },
        );
    }

    let component = vec!["a.R", "b.R", "c.R"];
    assert_eq!(component_oc_record_scalar_count(&component, &flat), 3);
}

#[test]
fn test_expand_optional_edges_for_suffix_matches_indexed_nodes() {
    let vcg_nodes: FxHashSet<String> = [
        "source[1].pin_p.reference",
        "source[2].pin_p.reference",
        "resistor[1].pin_p.reference",
        "resistor[2].pin_p.reference",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    let node_index = build_vcg_node_index(&vcg_nodes);

    let edges = expand_optional_edges_for_suffix(
        "source.pin_p.reference",
        "resistor.pin_p.reference",
        &node_index,
    );

    assert_eq!(edges.len(), 2);
    assert!(edges.contains(&(
        "source[1].pin_p.reference".to_string(),
        "resistor[1].pin_p.reference".to_string()
    )));
    assert!(edges.contains(&(
        "source[2].pin_p.reference".to_string(),
        "resistor[2].pin_p.reference".to_string()
    )));
}

#[test]
fn test_expand_optional_edges_for_suffix_drops_half_resolved_edges() {
    let vcg_nodes: FxHashSet<String> = ["source.pin.reference"]
        .into_iter()
        .map(str::to_string)
        .collect();
    let node_index = build_vcg_node_index(&vcg_nodes);

    let edges = expand_optional_edges_for_suffix(
        "source.pin.reference",
        "missing.pin.reference",
        &node_index,
    );

    assert!(
        edges.is_empty(),
        "a missing endpoint must not create a phantom VCG node"
    );
}

#[test]
fn test_validate_component_roots_requires_root_in_each_branched_component() {
    let first_span = test_span(10, 20);
    let second_span = test_span(30, 40);
    let data = VcgPreScanData {
        definite_roots: ["rooted_a.R".to_string()].into_iter().collect(),
        branches: vec![
            ("rooted_a.R".to_string(), "rooted_b.R".to_string()),
            ("unrooted_a.R".to_string(), "unrooted_b.R".to_string()),
        ],
        branch_spans: vec![first_span, second_span],
        potential_roots: Vec::new(),
    };

    let err = validate_component_roots(&data, &[])
        .expect_err("a root in another connected component must not satisfy CONN-013");

    match err {
        FlattenError::UnsupportedEquation { span, .. } => {
            assert_eq!(span, second_span);
        }
        other => panic!("unexpected error: {other:?}"),
    }
}

#[test]
fn required_edge_cycle_is_rejected_at_the_closing_edge() {
    let closing_span = test_span(50, 80);
    let data = VcgPreScanData {
        definite_roots: ["a.R".to_string()].into_iter().collect(),
        branches: vec![
            ("a.R".to_string(), "b.R".to_string()),
            ("b.R".to_string(), "c.R".to_string()),
            ("c.R".to_string(), "a.R".to_string()),
        ],
        branch_spans: vec![test_span(10, 20), test_span(30, 40), closing_span],
        potential_roots: Vec::new(),
    };

    let error =
        RequiredEdgeForest::construct(&data, &[]).expect_err("required edges must form a forest");
    assert!(matches!(
        error,
        FlattenError::InvalidConnectionGraph { span, .. } if span == closing_span
    ));
}

#[test]
fn required_edge_cannot_join_two_definite_root_trees() {
    let joining_span = test_span(30, 60);
    let data = VcgPreScanData {
        definite_roots: ["a.R".to_string(), "b.R".to_string()].into_iter().collect(),
        branches: vec![("a.R".to_string(), "b.R".to_string())],
        branch_spans: vec![joining_span],
        potential_roots: Vec::new(),
    };

    let error = RequiredEdgeForest::construct(&data, &[])
        .expect_err("one required-edge tree has at most one root");
    assert!(matches!(
        error,
        FlattenError::InvalidConnectionGraph { span, .. } if span == joining_span
    ));
}

#[test]
fn test_resolve_vcg_nodes_for_endpoint_preserves_explicit_indices() {
    let vcg_nodes: FxHashSet<String> = [
        "adapter[1].pin[1].reference",
        "adapter[1].pin[2].reference",
        "adapter[2].pin[1].reference",
        "adapter[2].pin[2].reference",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();
    let node_index = build_vcg_node_index(&vcg_nodes);

    let mut resolved = resolve_vcg_nodes_for_endpoint("adapter[1].pin.reference", &node_index);
    resolved.sort();

    assert_eq!(
        resolved,
        vec![
            "adapter[1].pin[1].reference".to_string(),
            "adapter[1].pin[2].reference".to_string(),
        ]
    );
}

#[test]
fn test_extract_overconstrained_suffixes_ignores_dot_inside_subscript_expression() {
    let vcg_nodes: FxHashSet<String> = [
        "adapter[data.medium]",
        "adapter[data.medium].reference",
        "body.frame_a.R",
    ]
    .into_iter()
    .map(str::to_string)
    .collect();

    let suffixes = extract_overconstrained_suffixes(&vcg_nodes);

    assert!(suffixes.contains(".reference"));
    assert!(suffixes.contains(".R"));
    assert!(
        !suffixes.contains(".medium]"),
        "dot inside bracketed subscript content must not be treated as a suffix separator"
    );
}

#[test]
fn test_derive_optional_edges_maps_wrapper_alias_arrays() {
    fn q(parts: &[(&str, &[i64])]) -> ast::QualifiedName {
        ast::QualifiedName {
            parts: parts
                .iter()
                .map(|(name, idx)| ((*name).to_string(), idx.to_vec()))
                .collect(),
        }
    }

    let vcg_data = VcgPreScanData {
        definite_roots: FxHashSet::default(),
        branches: vec![
            (
                "adapter.plugToPin[1].plug.reference".to_string(),
                "adapter.plugToPin[1].pin.reference".to_string(),
            ),
            (
                "adapter.plugToPin[2].plug.reference".to_string(),
                "adapter.plugToPin[2].pin.reference".to_string(),
            ),
            (
                "resistor[1].pin_p.reference".to_string(),
                "resistor[1].pin_n.reference".to_string(),
            ),
            (
                "resistor[2].pin_p.reference".to_string(),
                "resistor[2].pin_n.reference".to_string(),
            ),
            (
                "adapter.pin[1].reference".to_string(),
                "resistor[1].pin_p.reference".to_string(),
            ),
            (
                "adapter.pin[2].reference".to_string(),
                "resistor[2].pin_p.reference".to_string(),
            ),
        ],
        branch_spans: Vec::new(),
        potential_roots: Vec::new(),
    };

    let mut overlay = ast::InstanceOverlay::default();
    overlay.classes.insert(
        rumoca_core::InstanceId::new(1),
        ast::ClassInstanceData {
            class_def_id: None,
            qualified_name: ast::QualifiedName::from_ident("root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    q(&[("adapter", &[]), ("plugToPin", &[1]), ("pin", &[])]),
                    q(&[("adapter", &[]), ("pin", &[1])]),
                    None,
                    test_span(1, 2),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
                ast::InstanceConnection::scalar(
                    q(&[("adapter", &[]), ("plugToPin", &[2]), ("pin", &[])]),
                    q(&[("adapter", &[]), ("pin", &[2])]),
                    None,
                    test_span(2, 3),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
                ast::InstanceConnection::scalar(
                    q(&[("adapter", &[]), ("pin", &[])]),
                    q(&[("resistor", &[]), ("pin_p", &[])]),
                    None,
                    test_span(3, 4),
                    String::new(),
                )
                .expect("test scalar connection is valid"),
            ],
            ..Default::default()
        },
    );

    let overconstrained = crate::test_support::finalized_test_overlay(&mut overlay);
    let edges = derive_optional_edges(&overconstrained, &vcg_data).expect("optional edges");

    assert!(edges.contains(&(
        "adapter.pin[1].reference".to_string(),
        "resistor[1].pin_p.reference".to_string()
    )));
    assert!(edges.contains(&(
        "adapter.pin[2].reference".to_string(),
        "resistor[2].pin_p.reference".to_string()
    )));
    assert!(
        !edges.contains(&(
            "adapter.pin.reference".to_string(),
            "resistor.pin_p.reference".to_string()
        )),
        "unindexed wrapper edges should expand to indexed edges"
    );
}

#[test]
fn connection_freeze_optional_edges_prune_per_endpoint_disabled_connection() {
    let vcg_data = VcgPreScanData {
        definite_roots: FxHashSet::default(),
        branches: vec![("disabled.pin.R".to_string(), "active.pin.R".to_string())],
        branch_spans: vec![test_span(1, 2)],
        potential_roots: Vec::new(),
    };
    let mut overlay = ast::InstanceOverlay::default();
    overlay.classes.insert(
        rumoca_core::InstanceId::new(1),
        ast::ClassInstanceData {
            qualified_name: ast::QualifiedName::from_ident("root"),
            connections: vec![
                ast::InstanceConnection::scalar(
                    ast::QualifiedName::from_dotted("disabled.pin"),
                    ast::QualifiedName::from_dotted("active.pin"),
                    None,
                    test_span(1, 2),
                    String::new(),
                )
                .expect("disabled optional-edge fixture is a valid connection"),
            ],
            ..Default::default()
        },
    );
    overlay
        .disabled_components
        .insert(rumoca_core::ComponentPath::from_parts(["disabled"]));

    let overconstrained = crate::test_support::finalized_test_overlay(&mut overlay);
    let edges = derive_optional_edges(&overconstrained, &vcg_data)
        .expect("disabled connections are absent from the active VCG view");

    assert!(edges.is_empty());
}

fn checked_overconstrained_fixture(
    constraint_size: usize,
) -> (
    ast::ClassTree,
    rumoca_phase_typecheck::TypedOverlayProjection,
    flat::Model,
) {
    let source = format!(
        r#"
record R
  Real T;
  Real w;
  function equalityConstraint
    input R lhs;
    input R rhs;
    output Real residue[{constraint_size}];
  algorithm
    for i in 1:{constraint_size} loop
      residue[i] := lhs.T - rhs.T;
    end for;
  end equalityConstraint;
end R;
connector Pin
  R reference;
end Pin;
model M
  Pin a;
  Pin b;
  Pin c;
end M;
"#
    );
    let file = "vcg_finalized_catalog_fixture.mo";
    let stored = rumoca_phase_parse::parse_to_ast(&source, file).expect("fixture parses");
    let mut tree = ast::ClassTree::from_parsed(stored);
    tree.source_map.add(file, &source);
    let resolved =
        rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree)).expect("fixture resolves");
    let overlay =
        match rumoca_phase_instantiate::instantiate_model_with_outcome(resolved.inner(), "M") {
            rumoca_phase_instantiate::InstantiationOutcome::Success(overlay) => overlay,
            rumoca_phase_instantiate::InstantiationOutcome::NeedsInner {
                missing_inners, ..
            } => {
                panic!("fixture unexpectedly needs inner declarations: {missing_inners:?}")
            }
            rumoca_phase_instantiate::InstantiationOutcome::Error(error) => {
                panic!("fixture instantiation failed: {error}")
            }
        };
    let typed = rumoca_phase_typecheck::typecheck_instanced_tree(&resolved, overlay, "M")
        .expect("fixture typechecks");
    let overlay = typed.shared_projection();
    let flat =
        crate::flatten_typed(typed, crate::FlattenOptions::default()).expect("fixture flattens");
    let tree = resolved.inner().clone();
    (tree, overlay, flat)
}

fn cycle_forest() -> OverconstrainedEquationForest {
    let roots = FxHashSet::from_iter(["a.reference".to_string()]);
    let branches = vec![("a.reference".to_string(), "b.reference".to_string())];
    let optional = vec![
        ("a.reference".to_string(), "c.reference".to_string()),
        ("c.reference".to_string(), "b.reference".to_string()),
    ];
    OverconstrainedEquationForest::new(test_required_forest(&roots, &branches, &optional))
}

#[test]
fn finalized_catalog_contributes_exact_record_paths_to_vcg_nodes() {
    let (_tree, overlay, _flat) = checked_overconstrained_fixture(0);
    let catalog = overlay
        .overlay()
        .finalized_overconstrained()
        .expect("typecheck issues the finalized occurrence catalog");
    let nodes = collect_vcg_node_set(
        &VcgPreScanData {
            definite_roots: FxHashSet::default(),
            branches: Vec::new(),
            branch_spans: Vec::new(),
            potential_roots: Vec::new(),
        },
        &catalog,
    );
    assert!(nodes.contains("a.reference"));
    assert!(nodes.contains("b.reference"));
    assert!(nodes.contains("c.reference"));
}

#[test]
fn finalized_zero_width_catalog_still_selects_the_exact_broken_record_edge() {
    let (_tree, overlay, flat) = checked_overconstrained_fixture(0);
    let catalog = overlay
        .overlay()
        .finalized_overconstrained()
        .expect("typecheck issues the finalized occurrence catalog");
    let mut forest = cycle_forest();

    assert!(matches!(
        forest
            .generated_equality_disposition(
                &catalog,
                &flat,
                &rumoca_core::VarName::new("a.reference.T"),
                &rumoca_core::VarName::new("c.reference.T"),
                test_span(1, 2),
            )
            .unwrap(),
        GeneratedEqualityDisposition::Retain
    ));
    assert!(matches!(
        forest
            .generated_equality_disposition(
                &catalog,
                &flat,
                &rumoca_core::VarName::new("c.reference.T"),
                &rumoca_core::VarName::new("b.reference.T"),
                test_span(1, 2),
            )
            .unwrap(),
        GeneratedEqualityDisposition::Replace { lhs_record, rhs_record }
            if lhs_record == "c.reference" && rhs_record == "b.reference"
    ));
}

#[test]
fn foreign_flat_occurrence_cannot_default_to_ordinary_connection_evidence() {
    for foreign in [
        rumoca_core::InstanceId::UNSET,
        rumoca_core::InstanceId::new(u32::MAX),
    ] {
        let (_tree, overlay, mut flat) = checked_overconstrained_fixture(1);
        let variable = flat
            .variables
            .get_mut(&rumoca_core::VarName::new("a.reference.T"))
            .expect("source fixture materializes the exact record field");
        variable.instance_id = foreign;
        let catalog = overlay
            .overlay()
            .finalized_overconstrained()
            .expect("typecheck issues the finalized occurrence catalog");
        let mut forest = cycle_forest();

        let Err(error) = forest.generated_equality_disposition(
            &catalog,
            &flat,
            &rumoca_core::VarName::new("a.reference.T"),
            &rumoca_core::VarName::new("c.reference.T"),
            test_span(1, 2),
        ) else {
            panic!("a foreign Flat occurrence cannot be classified as ordinary");
        };

        assert!(matches!(
            error,
            FlattenError::InvalidConnectionEvidence { .. }
        ));
        assert!(
            error
                .to_string()
                .contains("foreign or unset Instance identity")
        );
    }
}

#[test]
fn finalized_nonempty_catalog_replaces_one_record_edge_only_once() {
    let (_tree, overlay, flat) = checked_overconstrained_fixture(1);
    let catalog = overlay
        .overlay()
        .finalized_overconstrained()
        .expect("typecheck issues the finalized occurrence catalog");
    let mut forest = cycle_forest();

    assert!(matches!(
        forest
            .generated_equality_disposition(
                &catalog,
                &flat,
                &rumoca_core::VarName::new("a.reference.T"),
                &rumoca_core::VarName::new("c.reference.T"),
                test_span(1, 2),
            )
            .unwrap(),
        GeneratedEqualityDisposition::Retain
    ));
    assert!(matches!(
        forest
            .generated_equality_disposition(
                &catalog,
                &flat,
                &rumoca_core::VarName::new("c.reference.T"),
                &rumoca_core::VarName::new("b.reference.T"),
                test_span(1, 2),
            )
            .unwrap(),
        GeneratedEqualityDisposition::Replace { .. }
    ));
    assert!(matches!(
        forest
            .generated_equality_disposition(
                &catalog,
                &flat,
                &rumoca_core::VarName::new("c.reference.w"),
                &rumoca_core::VarName::new("b.reference.w"),
                test_span(1, 2),
            )
            .unwrap(),
        GeneratedEqualityDisposition::Omit
    ));
}
