//! Connection path/array-index matching regressions.
//!
//! Split out of `connections::tests` to keep every test file inside the
//! SPEC_0021 file-size budget.

use super::*;

// =============================================================================
// Array Index Matching Tests
// =============================================================================

#[test]
fn test_split_path_with_indices() {
    // Simple path
    assert_eq!(path_segments_of("a.b.c"), vec!["a", "b", "c"]);

    // Path with array indices
    assert_eq!(
        path_segments_of("resistor[1].p.v"),
        vec!["resistor[1]", "p", "v"]
    );

    // Multiple array indices
    assert_eq!(
        path_segments_of("plug_p.pin[2].v"),
        vec!["plug_p", "pin[2]", "v"]
    );

    // Complex indices
    assert_eq!(
        path_segments_of("a[1,2].b[3].c"),
        vec!["a[1,2]", "b[3]", "c"]
    );
}

#[test]
fn test_strip_array_index() {
    assert_eq!(strip_array_index("resistor[1]"), "resistor");
    assert_eq!(strip_array_index("pin[2]"), "pin");
    assert_eq!(strip_array_index("p"), "p");
    assert_eq!(strip_array_index("a[1,2,3]"), "a");
}

#[test]
fn test_strip_embedded_array_indices() {
    assert_eq!(
        strip_embedded_array_indices("battery.resistor[2].p.i"),
        Some("battery.resistor.p.i".to_string())
    );
    assert_eq!(
        strip_embedded_array_indices("battery.capacitor.p.i[2]"),
        Some("battery.capacitor.p.i".to_string())
    );
    assert_eq!(strip_embedded_array_indices("battery.p.i"), None);
}

#[test]
fn test_extract_array_index() {
    assert_eq!(extract_array_index("resistor[1]"), Some("[1]".to_string()));
    assert_eq!(extract_array_index("pin[2]"), Some("[2]".to_string()));
    assert_eq!(extract_array_index("p"), None);
    assert_eq!(extract_array_index("a[1,2,3]"), Some("[1,2,3]".to_string()));
}

#[test]
fn test_parse_array_element_ref_simple_and_indexed_prefix() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("x"),
        flat::Variable {
            dims: vec![3],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("cell[2].v"),
        flat::Variable {
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    assert_eq!(
        parse_array_element_ref("x[1]", &flat),
        Some((rumoca_core::VarName::new("x"), 1))
    );
    assert_eq!(
        parse_array_element_ref("cell[2].v[3]", &flat),
        Some((rumoca_core::VarName::new("cell[2].v"), 3))
    );
}

#[test]
fn test_parse_array_element_ref_rejects_non_scalar_or_non_terminal_subscripts() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("x"),
        flat::Variable {
            dims: vec![2, 2],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    assert_eq!(parse_array_element_ref("x[1,2]", &flat), None);
    assert_eq!(parse_array_element_ref("x[1].y", &flat), None);
}

#[test]
fn test_scalarize_collapsed_connector_element() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("s[1].inductance.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let scalarized = scalarize_collapsed_connector_element(
        &rumoca_core::VarName::new("s[1].inductance.n.i"),
        "s[1].inductance.n[2]",
        &flat,
    );
    assert_eq!(
        scalarized,
        rumoca_core::VarName::new("s[1].inductance.n.i[2]")
    );
}

#[test]
fn test_scalarize_collapsed_connector_element_without_dims_still_scalarizes() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("cell.cell.resistor.p.i"),
        flat::Variable {
            flow: true,
            // Collapsed connector-array fields can reach flatten with dims=[].
            dims: vec![],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let scalarized = scalarize_collapsed_connector_element(
        &rumoca_core::VarName::new("cell.cell.resistor.p.i"),
        "cell.cell.resistor[2].p",
        &flat,
    );
    assert_eq!(
        scalarized,
        rumoca_core::VarName::new("cell.cell.resistor.p.i[2]")
    );
}

#[test]
fn test_is_flow_variable_subscripted_with_unknown_dims() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("arr.n.i"),
        flat::Variable {
            flow: true,
            // Unknown dims in flat::Variable must still allow element flow handling.
            dims: vec![],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    assert!(is_flow_variable(
        &flat,
        &rumoca_core::VarName::new("arr.n.i[2]")
    ));
}

#[test]
fn test_matches_with_array_indices() {
    let matches = |name: &str, segments: &[&str]| {
        let parsed = path_segments_of(name)
            .into_iter()
            .map(std::borrow::ToOwned::to_owned)
            .collect::<Vec<_>>();
        matches_with_array_indices_cached(&parsed, segments)
    };

    // resistor.p should match resistor[1].p.v
    assert!(matches("resistor[1].p.v", &["resistor", "p"]));
    assert!(matches("resistor[2].p.i", &["resistor", "p"]));

    // plug_p.pin should match plug_p.pin[1].v
    assert!(matches("plug_p.pin[1].v", &["plug_p", "pin"]));

    // Exact match (no array indices)
    assert!(matches("r1.n.v", &["r1", "n"]));

    // No match - wrong base name
    assert!(!matches("resistor[1].p.v", &["capacitor", "p"]));

    // No match - not enough parts (no suffix)
    assert!(!matches("resistor[1].p", &["resistor", "p"]));
}

#[test]
fn test_find_exact_match_with_array_expansion_handles_dot_inside_subscript() {
    let var_names = [
        rumoca_core::VarName::new("bus[data.medium].pin"),
        rumoca_core::VarName::new("bus[data.other].pin"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(var_names.iter());

    let matches = find_exact_match_with_array_expansion("bus[data.medium].pin", &var_index);
    assert_eq!(
        matches,
        vec![rumoca_core::VarName::new("bus[data.medium].pin")]
    );
}

#[test]
fn test_find_sub_variables_with_array_expansion_handles_dot_inside_subscript() {
    let var_names = [
        rumoca_core::VarName::new("bus[data.medium].pin.i"),
        rumoca_core::VarName::new("bus[data.medium].pin.v"),
        rumoca_core::VarName::new("bus[data.other].pin.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(var_names.iter());

    let matches =
        find_sub_variables_with_array_expansion_indexed("bus[data.medium].pin", &var_index);
    assert_eq!(matches.len(), 2);
    assert!(matches.contains(&rumoca_core::VarName::new("bus[data.medium].pin.i")));
    assert!(matches.contains(&rumoca_core::VarName::new("bus[data.medium].pin.v")));
}

#[test]
fn test_extract_suffix_exact() {
    // Exact prefix match
    let result = extract_suffix("r1.n.v", "r1.n");
    assert_eq!(result, Some(("v".to_string(), "".to_string())));

    let result = extract_suffix("a.b.c.d", "a.b.c");
    assert_eq!(result, Some(("d".to_string(), "".to_string())));
}

#[test]
fn test_extract_suffix_with_array_indices() {
    // Array prefix match
    let result = extract_suffix("resistor[1].p.v", "resistor.p");
    assert_eq!(result, Some(("v".to_string(), "[1]".to_string())));

    let result = extract_suffix("plug_p.pin[2].i", "plug_p.pin");
    assert_eq!(result, Some(("i".to_string(), "[2]".to_string())));

    // Multiple segments with indices
    let result = extract_suffix("resistor[1].p.v", "resistor.p");
    assert_eq!(result, Some(("v".to_string(), "[1]".to_string())));
}

#[test]
fn test_extract_suffix_preserves_segment_index_when_name_is_collapsed_array_field() {
    let result = extract_suffix("s[1].inductance.n.i", "s[1].inductance.n[2]");
    assert_eq!(result, Some(("i".to_string(), "[1][2]".to_string())));
}

#[test]
fn test_extract_suffix_handles_dot_inside_subscript_expression() {
    let result = extract_suffix("bus[data.medium].pin.i", "bus[data.medium].pin[2]");
    assert_eq!(
        result,
        Some(("i".to_string(), "[data.medium][2]".to_string()))
    );
}

#[test]
fn test_find_matching_var_b_exact() {
    let subs_b = vec![
        rumoca_core::VarName::new("plug_p.pin.v"),
        rumoca_core::VarName::new("plug_p.pin.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    // Exact match
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin.v")));

    let result = find_matching_var_b_indexed("i", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin.i")));

    // No match
    let result = find_matching_var_b_indexed("x", "", &sub_match_index);
    assert_eq!(result, None);
}

#[test]
fn test_find_matching_var_b_exact_with_dotted_suffix() {
    let subs_b = vec![
        rumoca_core::VarName::new("plug_p.pin.inner.v"),
        rumoca_core::VarName::new("plug_p.pin.inner.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    let result = find_matching_var_b_indexed("inner.v", "", &sub_match_index);
    assert_eq!(
        result,
        Some(rumoca_core::VarName::new("plug_p.pin.inner.v"))
    );
}

#[test]
fn test_find_matching_var_b_with_array_indices() {
    let subs_b = vec![
        rumoca_core::VarName::new("plug_p.pin[1].v"),
        rumoca_core::VarName::new("plug_p.pin[1].i"),
        rumoca_core::VarName::new("plug_p.pin[2].v"),
        rumoca_core::VarName::new("plug_p.pin[2].i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("plug_p.pin", &subs_b, &var_index);

    // Should find matching indexed variable
    let result = find_matching_var_b_indexed("v", "[1]", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin[1].v")));

    let result = find_matching_var_b_indexed("i", "[2]", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("plug_p.pin[2].i")));

    // Wrong index
    let result = find_matching_var_b_indexed("v", "[3]", &sub_match_index);
    assert_eq!(result, None);
}

#[test]
fn test_find_matching_var_b_with_collapsed_indexed_connector_path() {
    let subs_b = vec![
        rumoca_core::VarName::new("s[1].n.i"),
        rumoca_core::VarName::new("s[1].n.v"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("s[1].n[2]", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("i", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("s[1].n.i")));
}

#[test]
fn test_find_matching_var_b_preserves_explicit_cross_index_path() {
    // Connects like resistor[1].n <-> resistor[2].p should use B's explicit
    // index even when A/B indices differ.
    let subs_b = vec![
        rumoca_core::VarName::new("cell.cell.resistor.p.v"),
        rumoca_core::VarName::new("cell.cell.resistor.p.i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index =
        ConnectionSubMatchIndex::new("cell.cell.resistor[2].p", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(
        result,
        Some(rumoca_core::VarName::new("cell.cell.resistor.p.v"))
    );
}

#[test]
fn test_find_matching_var_b_does_not_cross_match_connector_member_name() {
    let subs_b = vec![
        rumoca_core::VarName::new("resistor.p.v"),
        rumoca_core::VarName::new("resistor.n.v"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("resistor[1].n", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("v", "", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("resistor.n.v")));
}

#[test]
fn test_find_matching_var_b_allows_indexed_b_when_a_has_no_indices() {
    // Reproduces scalar-to-indexed connector matches like:
    // connect(internalHeatPort, resistor[1].heatPort)
    // where A suffix extraction yields empty indices.
    let subs_b = vec![
        rumoca_core::VarName::new("battery.resistor.heatPort.T"),
        rumoca_core::VarName::new("battery.resistor.heatPort.Q_flow"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index =
        ConnectionSubMatchIndex::new("battery.resistor[1].heatPort", &subs_b, &var_index);

    let t_match = find_matching_var_b_indexed("T", "", &sub_match_index);
    let q_match = find_matching_var_b_indexed("Q_flow", "", &sub_match_index);

    assert_eq!(
        t_match,
        Some(rumoca_core::VarName::new("battery.resistor.heatPort.T"))
    );
    assert_eq!(
        q_match,
        Some(rumoca_core::VarName::new(
            "battery.resistor.heatPort.Q_flow"
        ))
    );
}

#[test]
fn test_strip_explicit_path_indices() {
    assert_eq!(
        strip_explicit_path_indices("[1][2]", "s[1].inductance.n"),
        "[2]"
    );
    assert_eq!(
        strip_explicit_path_indices("[1][2]", "s[1].inductance.n[2]"),
        ""
    );
    assert_eq!(strip_explicit_path_indices("[2]", "plug_p.pin"), "[2]");
}

#[test]
fn test_find_matching_var_b_keeps_trailing_connector_index_with_explicit_prefix() {
    let subs_b = vec![
        rumoca_core::VarName::new("s[1].p[1].i"),
        rumoca_core::VarName::new("s[1].p[2].i"),
        rumoca_core::VarName::new("s[1].p[3].i"),
    ];
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let sub_match_index = ConnectionSubMatchIndex::new("s[1].p", &subs_b, &var_index);
    let result = find_matching_var_b_indexed("i", "[2]", &sub_match_index);
    assert_eq!(result, Some(rumoca_core::VarName::new("s[1].p[2].i")));
}

#[test]
fn test_connect_sub_variable_indexes_collapsed_b_array_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("plug_p.pin[2].i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("plugs_n.pin.i"),
        flat::Variable {
            flow: true,
            dims: vec![3],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let sub_a = rumoca_core::VarName::new("plug_p.pin[2].i");
    let subs_b = vec![rumoca_core::VarName::new("plugs_n.pin.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("plugs_n.pin", &subs_b, &var_index);

    connect_sub_variable(
        &sub_a,
        "plug_p.pin",
        "plugs_n.pin",
        &sub_match_index,
        &mut ctx,
    );

    assert_eq!(
        flow_pairs,
        vec![(
            rumoca_core::VarName::new("plug_p.pin[2].i"),
            rumoca_core::VarName::new("plugs_n.pin.i[2]")
        )]
    );
}

#[test]
fn test_connect_sub_variable_does_not_index_scalar_b_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("resistor[1].p.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("r0.n.i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let sub_a = rumoca_core::VarName::new("resistor[1].p.i");
    let subs_b = vec![rumoca_core::VarName::new("r0.n.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("r0.n", &subs_b, &var_index);

    connect_sub_variable(&sub_a, "resistor.p", "r0.n", &sub_match_index, &mut ctx);

    assert_eq!(
        flow_pairs,
        vec![(
            rumoca_core::VarName::new("resistor[1].p.i"),
            rumoca_core::VarName::new("r0.n.i")
        )]
    );
}

#[test]
fn test_connect_sub_variable_does_not_index_single_element_b_array_member() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("plug_p.pin[1].i"),
        flat::Variable {
            flow: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("starpoints.pin.i"),
        flat::Variable {
            flow: true,
            dims: vec![1],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let sub_a = rumoca_core::VarName::new("plug_p.pin[1].i");
    let subs_b = vec![rumoca_core::VarName::new("starpoints.pin.i")];
    let mut flow_pairs = Vec::new();
    let mut potential_uf = UnionFind::new();
    let mut stream_uf = UnionFind::new();
    let var_index = ConnectionVarIndex::from_var_names(subs_b.iter());
    let mut ctx = ConnectionBuildCtx {
        flat: &flat,
        var_index: &var_index,
        flow_pairs: &mut flow_pairs,
        potential_uf: &mut potential_uf,
        stream_uf: &mut stream_uf,
    };
    let sub_match_index = ConnectionSubMatchIndex::new("starpoints.pin", &subs_b, &var_index);

    connect_sub_variable(
        &sub_a,
        "plug_p.pin",
        "starpoints.pin",
        &sub_match_index,
        &mut ctx,
    );

    assert_eq!(
        flow_pairs,
        vec![(
            rumoca_core::VarName::new("plug_p.pin[1].i"),
            rumoca_core::VarName::new("starpoints.pin.i")
        )]
    );
}

#[test]
fn test_find_sub_variables_with_array_expansion() {
    let mut flat = flat::Model::new();

    // Add variables for resistor[1-3].p.v and resistor[1-3].p.i
    for i in 1..=3 {
        flat.add_variable(
            rumoca_core::VarName::new(format!("resistor[{}].p.v", i)),
            flat::Variable::empty_with_span(test_span()),
        );
        flat.add_variable(
            rumoca_core::VarName::new(format!("resistor[{}].p.i", i)),
            flat::Variable {
                flow: true,
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    // Searching for "resistor.p" should find all resistor[*].p.* variables
    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("resistor.p", &pc, &var_index);
    assert_eq!(subs.len(), 6);

    // Verify all expected variables are found
    for i in 1..=3 {
        assert!(subs.contains(&rumoca_core::VarName::new(format!("resistor[{}].p.v", i))));
        assert!(subs.contains(&rumoca_core::VarName::new(format!("resistor[{}].p.i", i))));
    }
}

#[test]
fn test_find_sub_variables_indexed_prefix_matches_collapsed_connector_array_fields() {
    let mut flat = flat::Model::new();
    flat.add_variable(
        rumoca_core::VarName::new("s[1].inductance.n.i"),
        flat::Variable {
            flow: true,
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("s[1].inductance.n.v"),
        flat::Variable {
            dims: vec![4],
            ..flat::Variable::empty_with_span(test_span())
        },
    );

    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("s[1].inductance.n[2]", &pc, &var_index);
    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&rumoca_core::VarName::new("s[1].inductance.n.i")));
    assert!(subs.contains(&rumoca_core::VarName::new("s[1].inductance.n.v")));
}

#[test]
fn test_find_sub_variables_exact_match_preferred() {
    let mut flat = flat::Model::new();

    // Add both exact and indexed variables
    flat.add_variable(
        rumoca_core::VarName::new("r1.n.v"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r1.n.i"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r1[1].n.v"),
        flat::Variable::empty_with_span(test_span()),
    );
    flat.add_variable(
        rumoca_core::VarName::new("r1[1].n.i"),
        flat::Variable::empty_with_span(test_span()),
    );

    // Searching for "r1.n" should find exact matches
    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("r1.n", &pc, &var_index);
    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&rumoca_core::VarName::new("r1.n.v")));
    assert!(subs.contains(&rumoca_core::VarName::new("r1.n.i")));
}

#[test]
fn test_find_sub_variables_indexed_prefix_does_not_cross_match_connector_members() {
    let mut flat = flat::Model::new();
    // Collapsed connector-array fields commonly appear as indexless members with
    // array dims kept on the primitive variable itself.
    for name in [
        "resistor.p.v",
        "resistor.p.i",
        "resistor.n.v",
        "resistor.n.i",
    ] {
        flat.add_variable(
            rumoca_core::VarName::new(name),
            flat::Variable {
                dims: vec![1],
                flow: name.ends_with(".i"),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }

    let pc = build_prefix_children(&flat);
    let var_index = ConnectionVarIndex::new(&flat);
    let subs = find_sub_variables_indexed("resistor[1].n", &pc, &var_index);

    assert_eq!(subs.len(), 2);
    assert!(subs.contains(&rumoca_core::VarName::new("resistor.n.v")));
    assert!(subs.contains(&rumoca_core::VarName::new("resistor.n.i")));
    assert!(!subs.contains(&rumoca_core::VarName::new("resistor.p.v")));
    assert!(!subs.contains(&rumoca_core::VarName::new("resistor.p.i")));
}
