//! MLS §9.2 zero-flow coverage for partially connected compact arrays.
//!
//! Every fixture here runs the production connection transaction end to end
//! (`process_connections`) on compact primitive flow arrays and asserts the
//! exact multiset of generated flow rows. The load-bearing invariant is stated
//! once, in [`flow_element_constraint_counts`]: for connections declared at the
//! root scope, every scalar element of every flow declaration must be
//! constrained by exactly one row, either the flow sum of the one set it joins
//! or its own `= 0` row. A missing zero row shows up as a count of zero and a
//! double-counted element as a count of two, so neither defect can hide behind
//! a passing flatten.

use super::scalar_count_tests::{
    add_paired_component, array_variable, paired_overlay_root, test_span,
};
use super::*;
use std::collections::BTreeMap;

struct Fixture {
    flat: flat::Model,
    overlay: ast::InstanceOverlay,
    root: rumoca_core::InstanceId,
    connections: Vec<ast::InstanceConnection>,
}

/// Compact primitive declarations paired between Flat and the Instance
/// overlay. `flow` marks MLS §9.2 flow members; the rest are potentials.
fn fixture(components: &[(&str, &[i64], bool)]) -> Fixture {
    let mut flat = connection_test_model();
    for (name, dims, flow) in components {
        flat.add_variable(
            rumoca_core::VarName::new(*name),
            array_variable(dims.to_vec(), *flow),
        );
    }
    let (mut overlay, root) = paired_overlay_root(&mut flat);
    for (name, dims, _) in components {
        add_paired_component(&mut flat, &mut overlay, root, name, dims.to_vec());
    }
    Fixture {
        flat,
        overlay,
        root,
        connections: Vec::new(),
    }
}

fn endpoint(path: &str, subscripts: &[i64]) -> ast::QualifiedName {
    let mut name = ast::QualifiedName::from_dotted(path);
    name.parts.last_mut().expect("endpoint has a leaf").1 = subscripts.to_vec();
    name
}

impl Fixture {
    fn connect(&mut self, a: ast::QualifiedName, b: ast::QualifiedName, scope: &str) {
        self.connections.push(
            ast::InstanceConnection::scalar(a, b, None, test_span(), scope.to_string())
                .expect("fixture connection has valid endpoints and provenance"),
        );
    }

    fn run(&mut self) -> Result<(), FlattenError> {
        self.overlay
            .add_class(ast::ClassInstanceData {
                instance_id: self.root,
                qualified_name: ast::QualifiedName::from_ident("Root"),
                source_scope: Some(ast::QualifiedName::from_ident("Root")),
                connections: std::mem::take(&mut self.connections),
                ..Default::default()
            })
            .expect("fixture occurrence insertion must succeed");
        let _ = crate::test_support::finalized_test_overlay(&mut self.overlay);
        let overconstrained = self
            .overlay
            .finalized_overconstrained()
            .expect("connection fixture must construct finalized occurrence proofs");
        let mut forest = crate::vcg::OverconstrainedEquationForest::empty();
        finalize_connection_test_flat(&mut self.flat);
        process_connections_for_test(&mut self.flat, &overconstrained, &mut forest)
    }
}

/// Every generated connection row as `origin (scalar_count)`, sorted so the
/// comparison is an exact multiset equality independent of planning order.
fn rows(flat: &flat::Model) -> Vec<String> {
    let mut rows: Vec<String> = flat
        .equations
        .iter()
        .map(|equation| {
            let origin = match &equation.origin {
                flat::EquationOrigin::FlowSum { description } => format!("sum {description}"),
                flat::EquationOrigin::UnconnectedFlow { variable } => format!("zero {variable}"),
                flat::EquationOrigin::Connection { lhs, rhs } => format!("equal {lhs} = {rhs}"),
                other => panic!("unexpected connection row origin {other:?}"),
            };
            format!("{origin} ({})", equation.scalar_count)
        })
        .collect();
    rows.sort();
    rows
}

fn sorted(rows: &[&str]) -> Vec<String> {
    let mut rows: Vec<String> = rows.iter().map(|row| (*row).to_string()).collect();
    rows.sort();
    rows
}

fn collect_var_refs(
    expression: &rumoca_core::Expression,
    refs: &mut Vec<(rumoca_core::VarName, Vec<i64>)>,
) {
    match expression {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let subscripts = subscripts
                .iter()
                .map(|subscript| match subscript {
                    rumoca_core::Subscript::Index { value, .. } => *value,
                    other => panic!("connection rows carry literal subscripts only, got {other:?}"),
                })
                .collect();
            refs.push((name.var_name().clone(), subscripts));
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_var_refs(lhs, refs);
            collect_var_refs(rhs, refs);
        }
        rumoca_core::Expression::Unary { rhs, .. } => collect_var_refs(rhs, refs),
        rumoca_core::Expression::Literal { .. } => {}
        other => panic!("unexpected operand in a flow row: {other:?}"),
    }
}

/// Number of flow rows (flow sums and zero rows) constraining each scalar
/// element of each flow declaration.
///
/// The map is seeded with every element of every flow declaration at count
/// zero before any row is read, so an element no row mentions is reported as
/// zero rather than silently absent.
fn flow_element_constraint_counts(flat: &flat::Model) -> BTreeMap<(String, Vec<i64>), usize> {
    let mut counts = BTreeMap::new();
    for (name, variable) in &flat.variables {
        if !variable.flow {
            continue;
        }
        let every = flat::ConnectedDomain::unconnected()
            .unconnected_coordinates(&variable.dims)
            .expect("fixture dimensions are valid");
        for coordinate in every {
            counts.insert((name.as_str().to_string(), coordinate), 0usize);
        }
    }
    for equation in &flat.equations {
        if !matches!(
            equation.origin,
            flat::EquationOrigin::FlowSum { .. } | flat::EquationOrigin::UnconnectedFlow { .. }
        ) {
            continue;
        }
        let mut refs = Vec::new();
        collect_var_refs(&equation.residual, &mut refs);
        for (name, selection) in refs {
            let variable = &flat.variables[&name];
            assert!(variable.flow, "a flow row may only mention flow members");
            let mut selected = flat::ConnectedDomain::unconnected();
            selected
                .mark(&variable.dims, &selection)
                .expect("a flow row selects inside its declaration");
            for coordinate in selected
                .connected_coordinates(&variable.dims)
                .expect("fixture dimensions are valid")
            {
                *counts
                    .get_mut(&(name.as_str().to_string(), coordinate))
                    .expect("every flow element was seeded") += 1;
            }
        }
    }
    counts
}

fn assert_every_flow_element_constrained_exactly_once(flat: &flat::Model) {
    let violations: Vec<_> = flow_element_constraint_counts(flat)
        .into_iter()
        .filter(|(_, count)| *count != 1)
        .collect();
    assert!(
        violations.is_empty(),
        "flow elements constrained other than exactly once: {violations:?}"
    );
}

#[test]
fn one_connected_element_of_three_leaves_zero_rows_for_the_other_two() {
    let mut fixture = fixture(&[("a.c", &[3], true), ("s.p", &[], true)]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("s.p", &[]), "");

    fixture
        .run()
        .expect("one element of a compact flow array lowers");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum a.c[1] + s.p = 0 (1)",
            "zero a.c[2] (1)",
            "zero a.c[3] (1)"
        ])
    );
    let zero_rows: Vec<Vec<i64>> = flat
        .equations
        .iter()
        .filter(|equation| {
            matches!(
                equation.origin,
                flat::EquationOrigin::UnconnectedFlow { .. }
            )
        })
        .map(|equation| {
            let mut refs = Vec::new();
            collect_var_refs(&equation.residual, &mut refs);
            let [(name, subscripts)] = refs.as_slice() else {
                panic!("a zero row names exactly one member");
            };
            assert_eq!(name.as_str(), "a.c");
            subscripts.clone()
        })
        .collect();
    assert_eq!(zero_rows, vec![vec![2], vec![3]]);
    assert_eq!(
        flat.variables[&rumoca_core::VarName::new("a.c")]
            .connected
            .selections()
            .collect::<Vec<_>>(),
        vec![&[1][..]]
    );
    assert_every_flow_element_constrained_exactly_once(&flat);
}

#[test]
fn connecting_every_element_individually_produces_no_zero_row() {
    let mut fixture = fixture(&[
        ("a.c", &[3], true),
        ("s1.p", &[], true),
        ("s2.p", &[], true),
        ("s3.p", &[], true),
    ]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("s1.p", &[]), "");
    fixture.connect(endpoint("a.c", &[2]), endpoint("s2.p", &[]), "");
    fixture.connect(endpoint("a.c", &[3]), endpoint("s3.p", &[]), "");

    fixture
        .run()
        .expect("every element of a compact flow array lowers");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum a.c[1] + s1.p = 0 (1)",
            "sum a.c[2] + s2.p = 0 (1)",
            "sum a.c[3] + s3.p = 0 (1)",
        ])
    );
    assert_eq!(
        flat.variables[&rumoca_core::VarName::new("a.c")]
            .connected
            .coverage(&[3]),
        Ok(flat::ConnectedCoverage::Whole)
    );
    assert_every_flow_element_constrained_exactly_once(&flat);
}

#[test]
fn connecting_the_whole_array_keeps_one_compact_sum_and_no_zero_row() {
    let mut fixture = fixture(&[("a.c", &[3], true), ("t.c", &[3], true)]);
    fixture.connect(endpoint("a.c", &[]), endpoint("t.c", &[]), "");

    fixture.run().expect("a whole compact flow array lowers");
    let flat = fixture.flat;

    assert_eq!(rows(&flat), sorted(&["sum a.c + t.c = 0 (3)"]));
    assert_eq!(flat.structured_equations.len(), 1);
    assert_every_flow_element_constrained_exactly_once(&flat);
}

#[test]
fn one_element_of_a_matrix_leaves_zero_rows_for_the_other_three_in_row_major_order() {
    let mut fixture = fixture(&[("a.c", &[2, 2], true), ("s.p", &[], true)]);
    fixture.connect(endpoint("a.c", &[1, 2]), endpoint("s.p", &[]), "");

    fixture
        .run()
        .expect("one element of a compact flow matrix lowers");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum a.c[1,2] + s.p = 0 (1)",
            "zero a.c[1,1] (1)",
            "zero a.c[2,1] (1)",
            "zero a.c[2,2] (1)",
        ])
    );
    let zero_order: Vec<String> = flat
        .equations
        .iter()
        .filter_map(|equation| match &equation.origin {
            flat::EquationOrigin::UnconnectedFlow { variable } => Some(variable.clone()),
            _ => None,
        })
        .collect();
    assert_eq!(zero_order, ["a.c[1,1]", "a.c[2,1]", "a.c[2,2]"]);
    assert_every_flow_element_constrained_exactly_once(&flat);
}

#[test]
fn one_row_of_a_matrix_sums_pointwise_and_zeroes_the_other_row() {
    let mut fixture = fixture(&[("a.c", &[2, 2], true), ("t.r", &[2], true)]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("t.r", &[]), "");

    fixture
        .run()
        .expect("one row of a compact flow matrix lowers");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum a.c[1] + t.r = 0 (2)",
            "zero a.c[2,1] (1)",
            "zero a.c[2,2] (1)",
        ])
    );
    assert_every_flow_element_constrained_exactly_once(&flat);
}

#[test]
fn two_disjoint_element_connections_form_two_sets_and_zero_the_untouched_element() {
    let mut fixture = fixture(&[
        ("a.c", &[3], true),
        ("s1.p", &[], true),
        ("s2.p", &[], true),
    ]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("s1.p", &[]), "");
    fixture.connect(endpoint("a.c", &[3]), endpoint("s2.p", &[]), "");

    fixture
        .run()
        .expect("two disjoint element connections lower");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum a.c[1] + s1.p = 0 (1)",
            "sum a.c[3] + s2.p = 0 (1)",
            "zero a.c[2] (1)",
        ])
    );
    let domain = &flat.variables[&rumoca_core::VarName::new("a.c")].connected;
    assert_eq!(
        domain.selections().collect::<Vec<_>>(),
        vec![&[1][..], &[3][..]]
    );
    assert_eq!(domain.unconnected_coordinates(&[3]), Ok(vec![vec![2]]));
    assert_every_flow_element_constrained_exactly_once(&flat);
}

#[test]
fn a_potential_element_connection_generates_one_equality_and_no_zero_row() {
    let mut fixture = fixture(&[("a.u", &[3], false), ("s.y", &[], false)]);
    fixture.connect(endpoint("a.u", &[2]), endpoint("s.y", &[]), "");

    fixture
        .run()
        .expect("one element of a compact potential array lowers");
    let flat = fixture.flat;

    assert_eq!(rows(&flat), sorted(&["equal a.u[2] = s.y (1)"]));
    assert_eq!(
        flat.variables[&rumoca_core::VarName::new("a.u")]
            .connected
            .selections()
            .collect::<Vec<_>>(),
        vec![&[2][..]]
    );
}

/// The regression the checked domain exists for. Several declarations with
/// different partial patterns are lowered together; every flow element must
/// then be constrained exactly once. The second half proves the oracle has
/// teeth: dropping any single zero row, or duplicating any flow sum, is
/// detected as the exact element it leaves under- or over-constrained.
#[test]
fn every_flow_element_is_constrained_exactly_once_and_the_oracle_detects_omissions() {
    let mut fixture = fixture(&[
        ("a.c", &[3], true),
        ("b.c", &[2, 2], true),
        ("d.c", &[2], true),
        ("e.c", &[2], true),
        ("s1.p", &[], true),
        ("s2.p", &[], true),
        ("s3.p", &[], true),
        ("s4.p", &[], true),
        ("t.r", &[2], true),
    ]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("s1.p", &[]), "");
    fixture.connect(endpoint("b.c", &[1]), endpoint("t.r", &[]), "");
    fixture.connect(endpoint("b.c", &[2, 2]), endpoint("s2.p", &[]), "");
    fixture.connect(endpoint("d.c", &[1]), endpoint("s3.p", &[]), "");
    fixture.connect(endpoint("d.c", &[2]), endpoint("s4.p", &[]), "");

    fixture.run().expect("mixed partial connectivity lowers");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum a.c[1] + s1.p = 0 (1)",
            "sum b.c[1] + t.r = 0 (2)",
            "sum b.c[2,2] + s2.p = 0 (1)",
            "sum d.c[1] + s3.p = 0 (1)",
            "sum d.c[2] + s4.p = 0 (1)",
            "zero a.c[2] (1)",
            "zero a.c[3] (1)",
            "zero b.c[2,1] (1)",
            "zero e.c (2)",
        ])
    );
    assert_every_flow_element_constrained_exactly_once(&flat);

    // Oracle sensitivity: each zero row removed in turn exposes exactly the
    // elements it constrained, and nothing else changes.
    let zero_rows: Vec<usize> = flat
        .equations
        .iter()
        .enumerate()
        .filter(|(_, equation)| {
            matches!(
                equation.origin,
                flat::EquationOrigin::UnconnectedFlow { .. }
            )
        })
        .map(|(index, _)| index)
        .collect();
    assert_eq!(zero_rows.len(), 4);
    for index in zero_rows {
        let mut damaged = flat.clone();
        let removed = damaged.equations.remove(index);
        let flat::EquationOrigin::UnconnectedFlow { variable } = &removed.origin else {
            unreachable!("only zero rows are removed");
        };
        let zeros: Vec<_> = flow_element_constraint_counts(&damaged)
            .into_iter()
            .filter(|(_, count)| *count == 0)
            .map(|((name, coordinate), _)| {
                if coordinate.is_empty() {
                    name
                } else {
                    let rendered = coordinate
                        .iter()
                        .map(i64::to_string)
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{name}[{rendered}]")
                }
            })
            .collect();
        let expected: Vec<String> = if variable == "e.c" {
            vec!["e.c[1]".to_string(), "e.c[2]".to_string()]
        } else {
            vec![variable.clone()]
        };
        assert_eq!(zeros, expected, "removing `{variable}` must expose it");
    }

    // A duplicated flow sum double-counts exactly its members.
    let mut doubled = flat.clone();
    let sum = doubled
        .equations
        .iter()
        .find(|equation| {
            matches!(&equation.origin, flat::EquationOrigin::FlowSum { description } if description == "a.c[1] + s1.p = 0")
        })
        .cloned()
        .expect("the a.c[1] flow sum exists");
    doubled.equations.push(sum);
    let doubled_counts: Vec<_> = flow_element_constraint_counts(&doubled)
        .into_iter()
        .filter(|(_, count)| *count == 2)
        .map(|(element, _)| element)
        .collect();
    assert_eq!(
        doubled_counts,
        vec![
            ("a.c".to_string(), vec![1]),
            ("s1.p".to_string(), Vec::new())
        ]
    );
}

/// An interface element connected only inside its component is an outside
/// member of that set (negative sign) and, being in no set at the enclosing
/// scope, still owes the enclosing scope its zero row; the untouched elements
/// owe theirs from the inside pass.
#[test]
fn an_inner_element_connection_without_outer_coverage_zeroes_every_element_once_at_the_parent() {
    let mut fixture = fixture(&[("a.c", &[3], true), ("a.inner.x", &[], true)]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("a.inner.x", &[]), "a");

    fixture.run().expect("an inner element connection lowers");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum -a.c[1] + a.inner.x = 0 (1)",
            "zero a.c[1] (1)",
            "zero a.c[2] (1)",
            "zero a.c[3] (1)",
        ])
    );
}

#[test]
fn a_whole_array_connection_at_the_parent_covers_an_inner_element_connection() {
    let mut fixture = fixture(&[
        ("a.c", &[3], true),
        ("a.inner.x", &[], true),
        ("t.c", &[3], true),
    ]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("a.inner.x", &[]), "a");
    fixture.connect(endpoint("a.c", &[]), endpoint("t.c", &[]), "");

    fixture
        .run()
        .expect("nested whole and element connections lower");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&["sum -a.c[1] + a.inner.x = 0 (1)", "sum a.c + t.c = 0 (3)"])
    );
}

#[test]
fn a_parent_element_connection_covers_only_its_own_element() {
    let mut fixture = fixture(&[
        ("a.c", &[3], true),
        ("a.inner.x", &[], true),
        ("t.p", &[], true),
    ]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("a.inner.x", &[]), "a");
    fixture.connect(endpoint("a.c", &[2]), endpoint("t.p", &[]), "");

    fixture
        .run()
        .expect("nested disjoint element connections lower");
    let flat = fixture.flat;

    // `a.c[2]` is summed at the root; `a.c[1]` is summed inside `a` and, being
    // in no root set, zeroed at the root; `a.c[3]` is in no set anywhere.
    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum -a.c[1] + a.inner.x = 0 (1)",
            "sum a.c[2] + t.p = 0 (1)",
            "zero a.c[1] (1)",
            "zero a.c[3] (1)",
        ])
    );
}

#[test]
fn a_parent_connection_of_the_same_element_replaces_its_zero_row() {
    let mut fixture = fixture(&[
        ("a.c", &[3], true),
        ("a.inner.x", &[], true),
        ("t.p", &[], true),
    ]);
    fixture.connect(endpoint("a.c", &[1]), endpoint("a.inner.x", &[]), "a");
    fixture.connect(endpoint("a.c", &[1]), endpoint("t.p", &[]), "");

    fixture
        .run()
        .expect("nested connections of one element lower");
    let flat = fixture.flat;

    assert_eq!(
        rows(&flat),
        sorted(&[
            "sum -a.c[1] + a.inner.x = 0 (1)",
            "sum a.c[1] + t.p = 0 (1)",
            "zero a.c[2] (1)",
            "zero a.c[3] (1)",
        ])
    );
}

#[test]
fn a_non_literal_endpoint_index_is_still_refused_without_mutation() {
    let mut fixture = fixture(&[("a.c", &[3], true), ("s.p", &[], true)]);
    fixture.connect(
        ast::QualifiedName::from_dotted("a.c[n]"),
        endpoint("s.p", &[]),
        "",
    );
    let error = fixture
        .run()
        .expect_err("an index that is not statically known cannot select a connected domain");

    assert!(
        matches!(error, FlattenError::InvalidConnectionEvidence { .. }),
        "unexpected refusal: {error}"
    );
    assert!(
        error.to_string().contains("not concrete integer indices"),
        "{error}"
    );
    assert!(fixture.flat.equations.is_empty());
    assert!(fixture.flat.structured_equations.is_empty());
    assert!(
        fixture
            .flat
            .variables
            .values()
            .all(|variable| variable.connected.is_unconnected())
    );
}

#[test]
fn a_partial_stream_selection_is_still_refused() {
    let mut flat = connection_test_model();
    flat.add_variable(
        rumoca_core::VarName::new("port.h"),
        flat::Variable {
            stream: true,
            ..array_variable(vec![3], false)
        },
    );
    flat.add_variable(
        rumoca_core::VarName::new("port.i"),
        array_variable(vec![3], true),
    );

    assert_eq!(
        equation_generation::resolve_var_scalar_count(
            &flat,
            &rumoca_core::VarName::new("port.h[1]")
        ),
        None,
        "a strict subdomain of a compact stream array has no representable mixing owner"
    );
    assert_eq!(
        equation_generation::resolve_var_scalar_count(
            &flat,
            &rumoca_core::VarName::new("port.i[1]")
        ),
        Some(1)
    );
}

#[test]
fn the_same_element_summed_by_two_sets_at_one_scope_is_refused_without_mutation() {
    let mut fixture = fixture(&[
        ("a.c", &[2, 2], true),
        ("t.r", &[2], true),
        ("s.p", &[], true),
    ]);
    // `a.c[1]` and `a.c[1,2]` are different rendered members denoting an
    // overlapping block, so they cannot be merged by the union-find and would
    // otherwise both sum element `[1,2]`.
    fixture.connect(endpoint("a.c", &[1]), endpoint("t.r", &[]), "");
    fixture.connect(endpoint("a.c", &[1, 2]), endpoint("s.p", &[]), "");

    let error = fixture
        .run()
        .expect_err("an element summed by two sets at one scope is a double count");

    assert!(
        error.to_string().contains("summed by two connection sets"),
        "{error}"
    );
}
