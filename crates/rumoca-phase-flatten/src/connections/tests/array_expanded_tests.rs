//! Atomic array-to-expanded connection routing.

use super::super::*;

fn route_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("array_expanded_connections.mo"),
        5,
        17,
    )
}

fn route_variable(name: &str, dims: Vec<i64>) -> flat::Variable {
    flat::Variable {
        name: rumoca_core::VarName::new(name),
        dims,
        is_primitive: true,
        ..connection_test_variable(route_span())
    }
}

fn add_route_variable(flat: &mut flat::Model, name: &str, dims: Vec<i64>) {
    let effective = rumoca_core::EffectiveType::new(TypeId(1), TypeId(1), dims.clone()).unwrap();
    let type_id = flat
        .effective_types
        .iter()
        .find_map(|(type_id, candidate)| (candidate == &effective).then_some(*type_id))
        .unwrap_or_else(|| {
            let type_id = TypeId(100 + flat.effective_types.len() as u32);
            flat.effective_types.insert(type_id, effective);
            type_id
        });
    let mut variable = route_variable(name, dims);
    variable.type_id = type_id;
    flat.add_variable(rumoca_core::VarName::new(name), variable);
}

fn insert_mismatched_scalar_type(flat: &mut flat::Model) -> TypeId {
    let type_id = TypeId(2);
    flat.effective_types.insert(
        type_id,
        rumoca_core::EffectiveType::new(type_id, type_id, Vec::<i64>::new()).unwrap(),
    );
    type_id
}

fn nested_route_model() -> flat::Model {
    let mut flat = connection_test_model();
    add_route_variable(&mut flat, "values", vec![2, 2]);
    for name in [
        "bank[1].sensor[1].value",
        "bank[1].sensor[2].value",
        "bank[2].sensor[1].value",
        "bank[2].sensor[2].value",
    ] {
        add_route_variable(&mut flat, name, Vec::new());
    }
    flat
}

fn assert_union_pair(uf: &mut UnionFind, left: &str, right: &str) {
    let sets = uf.get_sets();
    assert!(sets.values().any(|set| {
        set.iter().any(|name| name.as_str() == left)
            && set.iter().any(|name| name.as_str() == right)
    }));
}

fn route_nested_array(reverse: bool) -> UnionFind {
    let flat = nested_route_model();
    let compact = rumoca_core::VarName::new("values");
    let expanded = rumoca_core::VarName::new("bank.sensor.value");
    let var_index = ConnectionVarIndex::new(&flat);
    let (path_a, path_b, var_a, var_b, a_is_primitive, b_is_primitive) = if reverse {
        (
            expanded.as_str(),
            compact.as_str(),
            &expanded,
            &compact,
            false,
            true,
        )
    } else {
        (
            compact.as_str(),
            expanded.as_str(),
            &compact,
            &expanded,
            true,
            false,
        )
    };
    let ctx = ArrayConnCtx {
        path_a,
        path_b,
        var_a,
        var_b,
        a_is_primitive,
        b_is_primitive,
        span: route_span(),
    };
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    connect_array_output_variables(
        &ctx,
        &flat,
        &var_index,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
    )
    .expect("all nested expanded coordinates prove a complete compact route");
    assert!(flow_pairs.is_empty());
    assert!(stream_uf.get_sets().is_empty());
    potential_uf
}

#[test]
fn nested_indices_route_every_coordinate_in_both_orientations() {
    for reverse in [false, true] {
        let mut potential_uf = route_nested_array(reverse);
        for (compact, expanded) in [
            ("values[1,1]", "bank[1].sensor[1].value"),
            ("values[1,2]", "bank[1].sensor[2].value"),
            ("values[2,1]", "bank[2].sensor[1].value"),
            ("values[2,2]", "bank[2].sensor[2].value"),
        ] {
            assert_union_pair(&mut potential_uf, compact, expanded);
        }
        assert_eq!(potential_uf.get_sets().len(), 4);
    }
}

#[derive(Clone, Copy, Debug)]
enum LaterMismatch {
    Flow,
    Stream,
    Type,
    Quantity,
    Variability,
    Dimensions,
}

fn mismatch_model(mismatch: LaterMismatch) -> flat::Model {
    let mut flat = connection_test_model();
    add_route_variable(&mut flat, "values", vec![2]);
    add_route_variable(&mut flat, "bank[1].value", Vec::new());
    add_route_variable(&mut flat, "bank[2].value", Vec::new());
    let compact = flat
        .variables
        .get_mut(&rumoca_core::VarName::new("values"))
        .expect("compact fixture exists");
    if matches!(mismatch, LaterMismatch::Stream) {
        compact.stream = true;
    }
    if matches!(mismatch, LaterMismatch::Quantity) {
        compact.quantity = Some("Voltage".to_string());
    }
    let first = flat
        .variables
        .get_mut(&rumoca_core::VarName::new("bank[1].value"))
        .expect("first expanded fixture exists");
    if matches!(mismatch, LaterMismatch::Stream) {
        first.stream = true;
    }
    if matches!(mismatch, LaterMismatch::Quantity) {
        first.quantity = Some("Voltage".to_string());
    }
    let mismatched_type =
        matches!(mismatch, LaterMismatch::Type).then(|| insert_mismatched_scalar_type(&mut flat));
    let later = flat
        .variables
        .get_mut(&rumoca_core::VarName::new("bank[2].value"))
        .expect("later expanded fixture exists");
    match mismatch {
        LaterMismatch::Flow => later.flow = true,
        LaterMismatch::Stream => {}
        LaterMismatch::Type => {
            later.type_id = mismatched_type.expect("type mismatch fixture was interned")
        }
        LaterMismatch::Quantity => later.quantity = Some("Current".to_string()),
        LaterMismatch::Variability => {
            later.variability = rumoca_core::Variability::Parameter(rumoca_core::Token::default());
        }
        LaterMismatch::Dimensions => later.dims = vec![1],
    }
    flat
}

#[test]
fn a_later_semantic_mismatch_never_commits_an_earlier_pair() {
    for mismatch in [
        LaterMismatch::Flow,
        LaterMismatch::Stream,
        LaterMismatch::Type,
        LaterMismatch::Quantity,
        LaterMismatch::Variability,
        LaterMismatch::Dimensions,
    ] {
        let flat = mismatch_model(mismatch);
        let expanded = [
            rumoca_core::VarName::new("bank[1].value"),
            rumoca_core::VarName::new("bank[2].value"),
        ];
        let mut flow_pairs = Vec::new();
        let mut potential_uf = UnionFind::new();
        let mut stream_uf = UnionFind::new();
        let array_var = rumoca_core::VarName::new("values");
        let route_ctx = ArrayExpandedRouteCtx {
            array_var: &array_var,
            expanded_path: "bank.value",
            expanded_vars: &expanded,
            flat: &flat,
            span: route_span(),
        };
        let error = connect_array_to_expanded(
            &route_ctx,
            &mut flow_pairs,
            &mut potential_uf,
            &mut stream_uf,
        )
        .expect_err("every pair must validate before the first pair mutates a set");

        assert!(!error.to_string().is_empty(), "{mismatch:?}");
        assert!(flow_pairs.is_empty(), "{mismatch:?}");
        assert!(potential_uf.get_sets().is_empty(), "{mismatch:?}");
        assert!(stream_uf.get_sets().is_empty(), "{mismatch:?}");
    }
}

#[test]
fn rank_cardinality_and_bounds_fail_before_mutation() {
    let cases = [
        (vec![2], vec!["bank[1].value"], "bank.value", true),
        (
            vec![2],
            vec!["bank[1].sensor[1].value", "bank[2].sensor[1].value"],
            "bank.sensor.value",
            true,
        ),
        (
            vec![2],
            vec!["bank[1].value", "bank[3].value"],
            "bank.value",
            true,
        ),
        (vec![1], vec!["bank[1].value"], "bank.value", false),
    ];
    for (dims, expanded_names, pattern, declare_expanded) in cases {
        let mut flat = connection_test_model();
        add_route_variable(&mut flat, "values", dims);
        if declare_expanded {
            for name in &expanded_names {
                add_route_variable(&mut flat, name, Vec::new());
            }
        }
        let expanded = expanded_names
            .iter()
            .map(|name| rumoca_core::VarName::new(*name))
            .collect::<Vec<_>>();
        let mut flow_pairs = Vec::new();
        let mut potential_uf = UnionFind::new();
        let mut stream_uf = UnionFind::new();
        let array_var = rumoca_core::VarName::new("values");
        let route_ctx = ArrayExpandedRouteCtx {
            array_var: &array_var,
            expanded_path: pattern,
            expanded_vars: &expanded,
            flat: &flat,
            span: route_span(),
        };
        let result = connect_array_to_expanded(
            &route_ctx,
            &mut flow_pairs,
            &mut potential_uf,
            &mut stream_uf,
        );

        assert!(
            matches!(result, Err(FlattenError::InvalidConnectionEvidence { .. })),
            "{result:?}"
        );
        assert!(flow_pairs.is_empty());
        assert!(potential_uf.get_sets().is_empty());
        assert!(stream_uf.get_sets().is_empty());
    }
}

#[test]
fn nonempty_compact_structural_array_is_refused_before_pair_mutation() {
    let mut flat = mismatch_model(LaterMismatch::Type);
    for variable in flat.variables.values_mut() {
        variable.type_id = TypeId(1);
        variable.variability = rumoca_core::Variability::Parameter(rumoca_core::Token::default());
    }
    let expanded = [
        rumoca_core::VarName::new("bank[1].value"),
        rumoca_core::VarName::new("bank[2].value"),
    ];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let array_var = rumoca_core::VarName::new("values");
    let route_ctx = ArrayExpandedRouteCtx {
        array_var: &array_var,
        expanded_path: "bank.value",
        expanded_vars: &expanded,
        flat: &flat,
        span: route_span(),
    };

    let error = connect_array_to_expanded(
        &route_ctx,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
    )
    .expect_err("Flat has no compact owner for a nonempty structural assertion family");

    assert!(matches!(
        error,
        FlattenError::InvalidConnectionEvidence { .. }
    ));
    assert!(
        error
            .to_string()
            .contains("nonempty compact structural array")
    );
    assert!(flow_pairs.is_empty());
    assert!(potential_uf.get_sets().is_empty());
    assert!(stream_uf.get_sets().is_empty());
}

#[test]
fn zero_sized_compact_structural_array_is_vacuously_equal() {
    let mut flat = connection_test_model();
    add_route_variable(&mut flat, "values", vec![0]);
    flat.variables
        .get_mut(&rumoca_core::VarName::new("values"))
        .expect("compact fixture exists")
        .variability = rumoca_core::Variability::Parameter(rumoca_core::Token::default());
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let array_var = rumoca_core::VarName::new("values");
    let expanded = Vec::new();
    let route_ctx = ArrayExpandedRouteCtx {
        array_var: &array_var,
        expanded_path: "bank.value",
        expanded_vars: &expanded,
        flat: &flat,
        span: route_span(),
    };

    let matched = connect_array_to_expanded(
        &route_ctx,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
    )
    .expect("a zero-sized structural array requires no assertion members");

    assert_eq!(matched, 0);
    assert!(flow_pairs.is_empty());
    assert!(potential_uf.get_sets().is_empty());
    assert!(stream_uf.get_sets().is_empty());
}

#[test]
fn expanded_to_expanded_later_pair_failure_is_atomic() {
    let mut flat = connection_test_model();
    for name in [
        "left[1].value",
        "left[2].value",
        "right[1].value",
        "right[2].value",
    ] {
        add_route_variable(&mut flat, name, Vec::new());
    }
    let mismatched_type = insert_mismatched_scalar_type(&mut flat);
    flat.variables
        .get_mut(&rumoca_core::VarName::new("right[2].value"))
        .expect("later expanded fixture exists")
        .type_id = mismatched_type;
    let var_index = ConnectionVarIndex::new(&flat);
    let var_a = rumoca_core::VarName::new("left.value");
    let var_b = rumoca_core::VarName::new("right.value");
    let ctx = ArrayConnCtx {
        path_a: var_a.as_str(),
        path_b: var_b.as_str(),
        var_a: &var_a,
        var_b: &var_b,
        a_is_primitive: false,
        b_is_primitive: false,
        span: route_span(),
    };
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();

    let error = connect_array_output_variables(
        &ctx,
        &flat,
        &var_index,
        &mut flow_pairs,
        &mut potential_uf,
        &mut stream_uf,
    )
    .expect_err("all expanded pairs must validate before the first union");

    assert!(matches!(error, FlattenError::IncompatibleConnectors { .. }));
    assert!(flow_pairs.is_empty());
    assert!(potential_uf.get_sets().is_empty());
    assert!(stream_uf.get_sets().is_empty());
}

struct ExpandedMemberConnectionAttempt {
    result: Result<(), FlattenError>,
    potential_uf: UnionFind,
    flow_pairs: Vec<(rumoca_core::VarName, rumoca_core::VarName)>,
    stream_uf: UnionFind,
}

fn expand_one_member_against_compact(
    source_name: &str,
    source_dims: Vec<i64>,
    compact_dims: Vec<i64>,
) -> ExpandedMemberConnectionAttempt {
    let mut flat = connection_test_model();
    add_route_variable(&mut flat, source_name, source_dims);
    add_route_variable(&mut flat, "b.x", compact_dims);
    let var_index = ConnectionVarIndex::new(&flat);
    let prefix_children = build_prefix_children(&flat);
    let subs_a = find_sub_variables_indexed("a", &prefix_children, &var_index);
    let subs_b = find_sub_variables_indexed("b", &prefix_children, &var_index);
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
        span: route_span(),
    };

    let result = expand_connector_connection(&subs_a, "a", "b", &subs_b, &mut ctx);
    ExpandedMemberConnectionAttempt {
        result,
        potential_uf,
        flow_pairs,
        stream_uf,
    }
}

#[test]
fn invalid_expanded_projection_cannot_launder_a_whole_array_connection() {
    for (source, source_dims, compact_dims, reason) in [
        ("a[3].x", vec![2], vec![2], "outside dimensions [2]"),
        (
            "a[1].x",
            vec![2, 2],
            vec![2, 2],
            "supplies 1 projection coordinates",
        ),
    ] {
        let ExpandedMemberConnectionAttempt {
            result,
            mut potential_uf,
            flow_pairs,
            mut stream_uf,
        } = expand_one_member_against_compact(source, source_dims, compact_dims);
        let error = result.expect_err(
            "unproved projection evidence must fail before any connection-set mutation",
        );

        assert!(matches!(
            error,
            FlattenError::InvalidConnectionEvidence { .. }
        ));
        assert!(error.to_string().contains(reason), "{source}: {error}");
        assert!(flow_pairs.is_empty());
        assert!(potential_uf.get_sets().is_empty());
        assert!(stream_uf.get_sets().is_empty());
    }
}

#[test]
fn expanded_projection_accepts_the_exact_in_bounds_boundary() {
    let ExpandedMemberConnectionAttempt {
        result,
        mut potential_uf,
        flow_pairs,
        mut stream_uf,
    } = expand_one_member_against_compact("a[2].x", Vec::new(), vec![2]);
    result.expect("the last valid compact coordinate has exact scalar evidence");

    assert!(flow_pairs.is_empty());
    assert!(stream_uf.get_sets().is_empty());
    assert_union_pair(&mut potential_uf, "a[2].x", "b.x[2]");
}

#[test]
fn nested_expanded_occurrence_coordinates_are_not_member_projections() {
    let mut flat = connection_test_model();
    add_route_variable(&mut flat, "a[1].sensor[2].x", vec![2]);
    add_route_variable(&mut flat, "b[1].sensor[2].x", vec![2]);
    let var_index = ConnectionVarIndex::new(&flat);
    let prefix_children = build_prefix_children(&flat);
    let subs_a = find_sub_variables_indexed("a.sensor", &prefix_children, &var_index);
    let subs_b = find_sub_variables_indexed("b.sensor", &prefix_children, &var_index);
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
        span: route_span(),
    };

    expand_connector_connection(&subs_a, "a.sensor", "b.sensor", &subs_b, &mut ctx)
        .expect("matching expanded occurrence coordinates retain the whole member value");

    assert!(flow_pairs.is_empty());
    assert!(stream_uf.get_sets().is_empty());
    assert_union_pair(&mut potential_uf, "a[1].sensor[2].x", "b[1].sensor[2].x");
    assert!(
        potential_uf
            .get_sets()
            .values()
            .flatten()
            .all(|name| !name.as_str().ends_with("x[1,2]")),
        "occurrence coordinates must not be appended as member-array selectors"
    );
}
