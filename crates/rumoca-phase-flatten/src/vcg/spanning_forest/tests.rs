//! The selected spanning forest must be the tree both `Connections.rooted`
//! and connection-equation emission follow (MLS §9.4, SPEC_0022 §3.12).

use rustc_hash::{FxHashMap, FxHashSet};

use rumoca_core::{SourceId, Span, VarName};
use rumoca_ir_flat as flat;

use super::super::{RequiredEdgeForest, VcgPreScanData, VcgResult, build_vcg};
use super::{EqualityLink, OverconstrainedEquationForest};

pub(crate) fn graph(
    definite_roots: &FxHashSet<String>,
    branches: &[(String, String)],
) -> VcgPreScanData {
    let source = SourceId::from_source_name("vcg_spanning_forest_test.mo");
    VcgPreScanData {
        definite_roots: definite_roots.clone(),
        branches: branches.to_vec(),
        branch_spans: (0..branches.len())
            .map(|index| Span::from_offsets(source, index * 2, index * 2 + 1))
            .collect(),
        potential_roots: Vec::new(),
    }
}

fn edges(pairs: &[(&str, &str)]) -> Vec<(String, String)> {
    pairs
        .iter()
        .map(|(lhs, rhs)| (lhs.to_string(), rhs.to_string()))
        .collect()
}

fn roots(nodes: &[&str]) -> FxHashSet<String> {
    nodes.iter().map(|node| node.to_string()).collect()
}

fn test_span() -> Span {
    Span::from_offsets(
        SourceId::from_source_name("vcg_spanning_forest_test.mo"),
        1,
        2,
    )
}

/// One overconstrained field `x` per record, with the given constraint width.
fn record_model(records: &FxHashSet<&str>, constraint_size: usize) -> flat::Model {
    let mut model = flat::Model::new();
    for record in records {
        let name = VarName::new(format!("{record}.x"));
        model.add_variable(
            name.clone(),
            flat::Variable {
                name,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(record.to_string()),
                oc_eq_constraint_size: Some(constraint_size),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    model
}

/// Record pairs related by an ordinary equality, and record pairs replaced by
/// `equalityConstraint`.
type EmittedRelations = (Vec<(String, String)>, Vec<(String, String)>);

/// The relations emission produces for the whole graph: every connection set
/// (a component of the optional edges) is emitted as one set, its members in
/// first-appearance order.
fn emitted_relations(
    forest: &mut OverconstrainedEquationForest,
    optional_edges: &[(String, String)],
) -> EmittedRelations {
    let records = optional_edges
        .iter()
        .flat_map(|(lhs, rhs)| [lhs.as_str(), rhs.as_str()])
        .collect::<FxHashSet<_>>();
    let model = record_model(&records, 3);
    let mut equal = Vec::new();
    let mut constraints = Vec::new();
    for set in connection_sets(optional_edges) {
        let variables = set
            .iter()
            .map(|record| VarName::new(format!("{record}.x")))
            .collect::<Vec<_>>();
        for link in forest.equality_links(&model, &variables).unwrap() {
            match link {
                EqualityLink::Equal { lhs, rhs } => {
                    equal.push((set[lhs].clone(), set[rhs].clone()))
                }
                EqualityLink::Constraint {
                    lhs_record,
                    rhs_record,
                    constraint_size,
                } => {
                    assert_eq!(constraint_size, 3);
                    constraints.push((lhs_record, rhs_record));
                }
            }
        }
    }
    (equal, constraints)
}

/// Components of the optional edges, members in first-appearance order.
fn connection_sets(optional_edges: &[(String, String)]) -> Vec<Vec<String>> {
    let mut sets: Vec<Vec<String>> = Vec::new();
    for (lhs, rhs) in optional_edges {
        let find = |sets: &Vec<Vec<String>>, node: &str| {
            sets.iter()
                .position(|set| set.iter().any(|member| member == node))
        };
        match (find(&sets, lhs), find(&sets, rhs)) {
            (Some(a), Some(b)) if a == b => {}
            (Some(a), Some(b)) => {
                let merged = sets.remove(a.max(b));
                sets[a.min(b)].extend(merged);
            }
            (Some(a), None) => sets[a].push(rhs.clone()),
            (None, Some(b)) => sets[b].push(lhs.clone()),
            (None, None) => sets.push(vec![lhs.clone(), rhs.clone()]),
        }
    }
    sets
}

fn adjacency(pairs: &[(String, String)]) -> FxHashMap<&str, Vec<&str>> {
    let mut adjacency: FxHashMap<&str, Vec<&str>> = FxHashMap::default();
    for (lhs, rhs) in pairs {
        adjacency.entry(lhs).or_default().push(rhs);
        adjacency.entry(rhs).or_default().push(lhs);
    }
    adjacency
}

fn reachable<'a>(
    adjacency: &FxHashMap<&'a str, Vec<&'a str>>,
    from: &'a str,
    skip: (&str, &str),
) -> FxHashSet<&'a str> {
    let mut seen = FxHashSet::from_iter([from]);
    let mut stack = vec![from];
    while let Some(node) = stack.pop() {
        for &next in adjacency.get(node).into_iter().flatten() {
            if (node, next) == skip || (next, node) == skip {
                continue;
            }
            if seen.insert(next) {
                stack.push(next);
            }
        }
    }
    seen
}

/// Build the VCG for one ordering of the optional edges and check that the
/// emitted equations realize the tree `rooted` reports. Returns the cut.
fn consistent_cut(
    definite_roots: &FxHashSet<String>,
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> Vec<(String, String)> {
    let data = graph(definite_roots, branches);
    let required = RequiredEdgeForest::construct(&data, optional_edges).unwrap();
    let VcgResult {
        is_root,
        rooted,
        spanning_forest,
    } = build_vcg(&data, optional_edges, &required);
    let mut forest = OverconstrainedEquationForest::new(spanning_forest);
    let (equal, constraints) = emitted_relations(&mut forest, optional_edges);

    // Required edges plus emitted equalities form a spanning forest: every
    // connection-set member is related to the others, and nothing closes a
    // cycle that a broken edge should have cut.
    let tree = branches
        .iter()
        .cloned()
        .chain(equal.iter().cloned())
        .collect::<Vec<_>>();
    let nodes = branches
        .iter()
        .chain(optional_edges)
        .flat_map(|(lhs, rhs)| [lhs.as_str(), rhs.as_str()])
        .collect::<FxHashSet<_>>();
    let tree_adjacency = adjacency(&tree);
    let mut components = 0;
    let mut covered = FxHashSet::default();
    for &node in &nodes {
        if covered.insert(node) {
            components += 1;
            covered.extend(reachable(&tree_adjacency, node, ("", "")));
        }
    }
    assert_eq!(
        tree.len(),
        nodes.len() - components,
        "emitted relations close a cycle"
    );
    // Every broken edge closes a cycle of the emitted tree, so its
    // equalityConstraint is the only relation left between its endpoints.
    for (lhs, rhs) in &constraints {
        assert!(reachable(&tree_adjacency, lhs, ("", "")).contains(rhs.as_str()));
    }
    assert_eq!(
        equal.len() + constraints.len(),
        optional_edges.len(),
        "each optional edge is either kept as an equality or cut exactly once"
    );

    // `rooted(a)` for `branch(a, b)` holds exactly when `a` lies on the root
    // side of that branch in the emitted tree.
    let selected_roots = is_root
        .iter()
        .filter(|(_, is_root)| **is_root)
        .map(|(node, _)| node.as_str())
        .collect::<Vec<_>>();
    for (lhs, rhs) in branches {
        let lhs_side = reachable(&tree_adjacency, lhs, (lhs, rhs));
        let root_on_lhs_side = selected_roots.iter().any(|root| lhs_side.contains(root));
        assert_eq!(
            rooted[lhs.as_str()],
            root_on_lhs_side,
            "Connections.rooted({lhs}) disagrees with the emitted cut {constraints:?}"
        );
    }
    let mut cut = constraints
        .into_iter()
        .map(|(lhs, rhs)| if lhs <= rhs { (lhs, rhs) } else { (rhs, lhs) })
        .collect::<Vec<_>>();
    cut.sort();
    cut
}

#[test]
fn three_node_loop_rooted_matches_emitted_cut() {
    let definite_roots = roots(&["a.R"]);
    let branches = edges(&[("a.R", "b.R")]);
    let optional = edges(&[("b.R", "c.R"), ("c.R", "a.R")]);
    let reversed = optional.iter().rev().cloned().collect::<Vec<_>>();

    let cut = consistent_cut(&definite_roots, &branches, &optional);
    assert_eq!(cut, edges(&[("b.R", "c.R")]));
    assert_eq!(consistent_cut(&definite_roots, &branches, &reversed), cut);
}

#[test]
fn multi_record_connection_set_is_partitioned_by_the_selected_forest() {
    // One connection set {p, q, s}; p and s hang off the root through required
    // edges, so the set's optional edges must break exactly once. Emitting the
    // set as a plain chain in its member order would relate the wrong pair.
    let definite_roots = roots(&["w.R"]);
    let branches = edges(&[("w.R", "p.R"), ("w.R", "s.R")]);
    let optional = edges(&[("p.R", "q.R"), ("q.R", "s.R")]);
    let cut = consistent_cut(&definite_roots, &branches, &optional);
    assert_eq!(cut.len(), 1);
}

/// Definite roots, required edges, and optional edges of one graph.
type Graph = (
    FxHashSet<String>,
    Vec<(String, String)>,
    Vec<(String, String)>,
);

/// The virtual connection graph of
/// `Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1`: one definite root
/// (`world`), eleven `Connections.branch` edges and twenty-one `connect`
/// edges, eight of them inside the three `BodyCylinder` components.
fn fourbar1() -> Graph {
    let branches = edges(&[
        ("j1.frame_a.R", "j1.frame_b.R"),
        ("j2.frame_a.R", "j2.frame_b.R"),
        (
            "b1.frameTranslation.frame_a.R",
            "b1.frameTranslation.frame_b.R",
        ),
        (
            "b2.frameTranslation.frame_a.R",
            "b2.frameTranslation.frame_b.R",
        ),
        (
            "b3.frameTranslation.frame_a.R",
            "b3.frameTranslation.frame_b.R",
        ),
        ("rev.frame_a.R", "rev.frame_b.R"),
        ("rev1.frame_a.R", "rev1.frame_b.R"),
        ("j3.frame_a.R", "j3.frame_b.R"),
        ("j4.frame_a.R", "j4.frame_b.R"),
        ("j5.frame_a.R", "j5.frame_b.R"),
        ("b0.frame_a.R", "b0.frame_b.R"),
    ]);
    let optional = edges(&[
        ("j2.frame_b.R", "b2.frame_a.R"),
        ("j1.frame_b.R", "b1.frame_a.R"),
        ("rev.frame_a.R", "b2.frame_b.R"),
        ("rev.frame_b.R", "rev1.frame_a.R"),
        ("rev1.frame_b.R", "b3.frame_a.R"),
        ("world.frame_b.R", "j1.frame_a.R"),
        ("b1.frame_b.R", "j3.frame_a.R"),
        ("j3.frame_b.R", "j4.frame_a.R"),
        ("j4.frame_b.R", "j5.frame_a.R"),
        ("j5.frame_b.R", "b3.frame_b.R"),
        ("b0.frame_a.R", "world.frame_b.R"),
        ("b0.frame_b.R", "j2.frame_a.R"),
        ("b1.frame_a.R", "b1.frameTranslation.frame_a.R"),
        ("b1.frame_b.R", "b1.frameTranslation.frame_b.R"),
        ("b1.frame_a.R", "b1.body.frame_a.R"),
        ("b2.frame_a.R", "b2.frameTranslation.frame_a.R"),
        ("b2.frame_b.R", "b2.frameTranslation.frame_b.R"),
        ("b2.frame_a.R", "b2.body.frame_a.R"),
        ("b3.frame_a.R", "b3.frameTranslation.frame_a.R"),
        ("b3.frame_b.R", "b3.frameTranslation.frame_b.R"),
        ("b3.frame_a.R", "b3.body.frame_a.R"),
    ]);
    (roots(&["world.frame_b.R"]), branches, optional)
}

#[test]
fn fourbar1_cut_is_consistent_and_independent_of_connect_order() {
    let (definite_roots, branches, optional) = fourbar1();
    let cut = consistent_cut(&definite_roots, &branches, &optional);
    // Both loop paths from `world` reach `b3` at distance 13; the loop closes
    // inside `b3`, at the lexicographically larger of its two tied edges.
    assert_eq!(
        cut,
        edges(&[("b3.frameTranslation.frame_b.R", "b3.frame_b.R")])
    );

    let mut reversed = optional.clone();
    reversed.reverse();
    assert_eq!(consistent_cut(&definite_roots, &branches, &reversed), cut);
    for rotation in 1..optional.len() {
        let mut rotated = optional.clone();
        rotated.rotate_left(rotation);
        assert_eq!(consistent_cut(&definite_roots, &branches, &rotated), cut);
        let flipped = rotated
            .into_iter()
            .map(|(lhs, rhs)| (rhs, lhs))
            .collect::<Vec<_>>();
        assert_eq!(consistent_cut(&definite_roots, &branches, &flipped), cut);
    }
}

#[test]
fn acyclic_graph_keeps_every_optional_edge_in_source_chain_order() {
    let definite_roots = roots(&["a.R"]);
    let branches = edges(&[("a.R", "b.R")]);
    let optional = edges(&[("b.R", "c.R"), ("c.R", "d.R")]);
    let data = graph(&definite_roots, &branches);
    let required = RequiredEdgeForest::construct(&data, &optional).unwrap();
    let vcg = build_vcg(&data, &optional, &required);
    assert!(vcg.spanning_forest.broken_edges().is_empty());

    let mut forest = OverconstrainedEquationForest::new(vcg.spanning_forest);
    let model = record_model(&FxHashSet::from_iter(["b.R", "c.R", "d.R"]), 3);
    let variables = ["d.R.x", "b.R.x", "c.R.x"].map(VarName::new);
    assert_eq!(
        forest.equality_links(&model, &variables).unwrap(),
        vec![
            EqualityLink::Equal { lhs: 0, rhs: 1 },
            EqualityLink::Equal { lhs: 1, rhs: 2 },
        ]
    );
}

#[test]
fn zero_width_broken_edge_is_omitted_and_nonempty_one_replaced_once() {
    let branches = edges(&[("a.R", "b.R")]);
    let optional = edges(&[("a.R", "c.R"), ("c.R", "b.R")]);
    let records = FxHashSet::from_iter(["a.R", "b.R", "c.R"]);
    let set =
        |field: &str| ["a.R", "c.R", "b.R"].map(|record| VarName::new(format!("{record}.{field}")));

    let mut forest = super::test_equation_forest(&FxHashSet::default(), &branches, &optional);
    let zero_width = record_model(&records, 0);
    assert_eq!(
        forest.equality_links(&zero_width, &set("x")).unwrap(),
        vec![EqualityLink::Equal { lhs: 0, rhs: 1 }]
    );

    let mut forest = super::test_equation_forest(&FxHashSet::default(), &branches, &optional);
    let mut model = record_model(&records, 3);
    for record in records {
        let name = VarName::new(format!("{record}.y"));
        model.add_variable(
            name.clone(),
            flat::Variable {
                name,
                is_primitive: true,
                is_overconstrained: true,
                oc_record_path: Some(record.to_string()),
                oc_eq_constraint_size: Some(3),
                ..flat::Variable::empty_with_span(test_span())
            },
        );
    }
    let replaced = EqualityLink::Constraint {
        lhs_record: "c.R".to_string(),
        rhs_record: "b.R".to_string(),
        constraint_size: 3,
    };
    assert_eq!(
        forest.equality_links(&model, &set("x")).unwrap(),
        vec![EqualityLink::Equal { lhs: 0, rhs: 1 }, replaced]
    );
    assert_eq!(
        forest.equality_links(&model, &set("y")).unwrap(),
        vec![EqualityLink::Equal { lhs: 0, rhs: 1 }],
        "the second field of a broken record edge must not repeat its constraint"
    );
}

#[test]
fn members_outside_the_graph_keep_their_equality_beside_a_broken_edge() {
    let branches = edges(&[("a.R", "b.R")]);
    let optional = edges(&[("a.R", "c.R"), ("c.R", "b.R")]);
    let mut forest = super::test_equation_forest(&FxHashSet::default(), &branches, &optional);
    let mut model = record_model(&FxHashSet::from_iter(["a.R", "b.R", "c.R"]), 3);
    let plain = VarName::new("z.x");
    model.add_variable(
        plain.clone(),
        flat::Variable {
            name: plain,
            is_primitive: true,
            ..flat::Variable::empty_with_span(test_span())
        },
    );
    let variables = ["a.R.x", "c.R.x", "b.R.x", "z.x"].map(VarName::new);
    assert_eq!(
        forest.equality_links(&model, &variables).unwrap(),
        vec![
            EqualityLink::Equal { lhs: 0, rhs: 1 },
            EqualityLink::Constraint {
                lhs_record: "c.R".to_string(),
                rhs_record: "b.R".to_string(),
                constraint_size: 3,
            },
            EqualityLink::Equal { lhs: 0, rhs: 3 },
        ],
        "the broken edge is cut and the member outside the graph stays equal"
    );
}

#[test]
fn root_selection_prefers_definite_then_lowest_priority_then_name() {
    let component = ["m.R", "p.R", "q.R", "r.R"];
    let potential = vec![
        ("r.R".to_string(), 2),
        ("q.R".to_string(), 1),
        ("p.R".to_string(), 1),
    ];
    let select = |definite: &[&str], potential: &[(String, i64)]| {
        super::super::select_root(&component, &roots(definite), potential)
    };
    assert_eq!(select(&["r.R"], &potential), "r.R");
    assert_eq!(select(&[], &potential), "p.R");
    assert_eq!(select(&[], &potential[..1]), "r.R");
    assert_eq!(select(&[], &[]), "m.R");
}

#[test]
fn potential_root_priority_places_the_root_and_the_cut() {
    // A four-node loop with no definite root: the lowest-priority potential
    // root is the root, so the loop breaks at the edge farthest from it.
    let branches = edges(&[("a.R", "b.R"), ("c.R", "d.R")]);
    let optional = edges(&[("b.R", "c.R"), ("d.R", "a.R")]);
    let mut data = graph(&FxHashSet::default(), &branches);
    data.potential_roots = vec![("c.R".to_string(), 0), ("a.R".to_string(), 1)];
    let required = RequiredEdgeForest::construct(&data, &optional).unwrap();
    let vcg = build_vcg(&data, &optional, &required);
    assert_eq!(vcg.is_root.get("c.R"), Some(&true));
    assert_eq!(vcg.is_root.get("a.R"), Some(&false));
    // Distances from c: b 1, d 1, a 2, so the optional edges are keyed
    // (b,c) = (0, 1) and (d,a) = (1, 2); (d.R, a.R) closes the loop.
    assert_eq!(vcg.spanning_forest.broken_edges(), edges(&[("d.R", "a.R")]));
    assert_eq!(vcg.rooted.get("c.R"), Some(&true));
    assert_eq!(vcg.rooted.get("a.R"), Some(&false));
}
