use std::collections::{BTreeMap, BTreeSet};

use rumoca_core::{SourceId, Span};

use super::*;

fn provenance() -> DaeProvenance {
    DaeProvenance::source(Span::from_offsets(
        SourceId::from_source_name("function_reads.mo"),
        1,
        2,
    ))
    .expect("test provenance is source-backed")
}

const fn fact(value: u32, definition: u32, witness: u32) -> FunctionReadFact {
    FunctionReadFact {
        value,
        definition,
        witness,
    }
}

fn singleton(sets: &mut FunctionReadSets, value: u32, definition: u32) -> FunctionReadSet {
    sets.singleton(fact(value, definition, value), provenance())
        .expect("small test arena has capacity")
}

fn facts(sets: &FunctionReadSets, root: FunctionReadSet) -> Vec<FunctionReadFact> {
    let mut found = Vec::new();
    sets.try_for_each(root, &mut |fact| {
        found.push(fact);
        Ok::<_, ()>(())
    })
    .expect("infallible visitor");
    found
}

fn audit(sets: &FunctionReadSets, root: FunctionReadSet) {
    if root == FunctionReadSet::EMPTY {
        assert!(facts(sets, root).is_empty());
        return;
    }
    let mut visited = BTreeSet::new();
    let mut prefix = Vec::new();
    let minimum = audit_at(sets, root.raw(), 0, &mut prefix, &mut visited);
    assert_eq!(minimum, sets.nodes[root.raw() as usize].key);
    let values = facts(sets, root)
        .into_iter()
        .map(|fact| fact.value)
        .collect::<Vec<_>>();
    assert!(values.windows(2).all(|pair| pair[0] < pair[1]));
}

fn audit_at(
    sets: &FunctionReadSets,
    raw: u32,
    parent_depth: u32,
    prefix: &mut Vec<(u32, bool)>,
    visited: &mut BTreeSet<u32>,
) -> u32 {
    assert_ne!(raw, EMPTY);
    assert!((raw as usize) < sets.nodes.len());
    assert!(
        visited.insert(raw),
        "a trie root cannot reach one node twice"
    );
    let node = sets.nodes[raw as usize];
    for &(depth, side) in prefix.iter() {
        assert_eq!(bit(node.key, depth), side);
    }
    if node.is_leaf() {
        assert_eq!(node.depth(), u32::BITS);
        assert!(node.depth() >= parent_depth);
        return node.key;
    }
    assert!(node.branch < u32::BITS);
    assert!(node.branch >= parent_depth);
    let zero = node.zero_or_definition;
    let one = node.one_or_witness;
    assert!((zero as usize) < sets.nodes.len());
    assert!((one as usize) < sets.nodes.len());
    assert!(zero < raw);
    assert!(one < raw);
    let zero_node = sets.nodes[zero as usize];
    let one_node = sets.nodes[one as usize];
    assert!(zero_node.depth() > node.branch);
    assert!(one_node.depth() > node.branch);
    assert_eq!(node.key, zero_node.key);
    assert!(!bit(zero_node.key, node.branch));
    assert!(bit(one_node.key, node.branch));
    prefix.push((node.branch, false));
    let zero_min = audit_at(sets, zero, node.branch + 1, prefix, visited);
    prefix.pop();
    prefix.push((node.branch, true));
    let one_min = audit_at(sets, one, node.branch + 1, prefix, visited);
    prefix.pop();
    assert_eq!(node.key, zero_min);
    assert!(zero_min < one_min);
    zero_min
}

fn merged(
    sets: &mut FunctionReadSets,
    lhs: FunctionReadSet,
    rhs: FunctionReadSet,
) -> Result<FunctionReadSet, FunctionReadMergeError> {
    sets.merge(lhs, rhs, provenance())
}

fn oracle_conflict(
    oracle: &BTreeMap<u32, u32>,
    value: u32,
    definition: u32,
    witness: u32,
) -> Option<(u32, u32, u32, u32)> {
    let expected = *oracle.get(&value)?;
    (expected != definition).then_some((value, expected, definition, witness))
}

fn assert_conflicting_merge(
    sets: &mut FunctionReadSets,
    root: FunctionReadSet,
    candidate: FunctionReadSet,
    expected: (u32, u32, u32, u32),
) {
    let conflict = match merged(sets, root, candidate) {
        Err(FunctionReadMergeError::Conflict(conflict)) => conflict,
        _ => panic!("oracle conflict must be detected"),
    };
    assert_eq!(
        (
            conflict.found.value,
            conflict.expected.definition,
            conflict.found.definition,
            conflict.found.witness,
        ),
        expected
    );
}

#[test]
fn empty_and_boundary_keys_are_distinct() {
    let mut sets = FunctionReadSets::default();
    let zero = singleton(&mut sets, 0, 1);
    let maximum = singleton(&mut sets, u32::MAX, 2);
    let root = merged(&mut sets, zero, maximum).expect("boundary keys are disjoint");

    assert_eq!(facts(&sets, FunctionReadSet::EMPTY), []);
    assert_eq!(
        facts(&sets, root)
            .into_iter()
            .map(|fact| fact.value)
            .collect::<Vec<_>>(),
        [0, u32::MAX]
    );
    audit(&sets, root);
}

#[test]
fn equal_leaf_reuses_identity_or_reports_oriented_conflict() {
    let mut sets = FunctionReadSets::default();
    let first = sets
        .singleton(fact(7, 3, 11), provenance())
        .expect("small arena");
    let same = sets
        .singleton(fact(7, 3, 12), provenance())
        .expect("small arena");
    let different = sets
        .singleton(fact(7, 4, 13), provenance())
        .expect("small arena");
    let count = sets.nodes.len();

    assert_eq!(
        merged(&mut sets, first, same).expect("same definition"),
        first
    );
    assert_eq!(sets.nodes.len(), count);
    let Err(FunctionReadMergeError::Conflict(conflict)) = merged(&mut sets, first, different)
    else {
        panic!("different definitions must conflict");
    };
    assert_eq!(conflict.expected, fact(7, 3, 11));
    assert_eq!(conflict.found, fact(7, 4, 13));
    assert_eq!(sets.nodes.len(), count);
}

#[test]
fn disjoint_union_allocates_one_branch_and_never_a_leaf() {
    let mut sets = FunctionReadSets::default();
    let lhs = singleton(&mut sets, 0x1000_0000, 0);
    let rhs = singleton(&mut sets, 0xf000_0000, 1);
    let before = sets.nodes.len();
    let root = merged(&mut sets, lhs, rhs).expect("keys are disjoint");

    assert_eq!(sets.nodes.len(), before + 1);
    assert!(sets.nodes[before..].iter().all(|node| !node.is_leaf()));
    audit(&sets, root);
}

#[test]
fn union_handles_shallower_tree_on_either_side() {
    let mut sets = FunctionReadSets::default();
    let outer_zero = singleton(&mut sets, 0x0000_0000, 0);
    let outer_one = singleton(&mut sets, 0x8000_0000, 1);
    let shallow = merged(&mut sets, outer_zero, outer_one).expect("outer split");
    let inner_zero = singleton(&mut sets, 0x4000_0000, 2);
    let inner_one = singleton(&mut sets, 0x6000_0000, 3);
    let deep = merged(&mut sets, inner_zero, inner_one).expect("inner split");

    let lhs_shallow = merged(&mut sets, shallow, deep).expect("lhs shallower");
    audit(&sets, lhs_shallow);
    let rhs_shallow = merged(&mut sets, deep, shallow).expect("rhs shallower");
    audit(&sets, rhs_shallow);
    assert_eq!(
        facts(&sets, lhs_shallow)
            .into_iter()
            .map(|fact| fact.value)
            .collect::<Vec<_>>(),
        facts(&sets, rhs_shallow)
            .into_iter()
            .map(|fact| fact.value)
            .collect::<Vec<_>>()
    );
}

#[test]
fn equal_depth_union_visits_zero_before_one() {
    let mut sets = FunctionReadSets::default();
    let lhs_zero = sets
        .singleton(fact(0x0000_0000, 0, 1), provenance())
        .expect("small arena");
    let lhs_one = sets
        .singleton(fact(0x8000_0000, 0, 2), provenance())
        .expect("small arena");
    let lhs = merged(&mut sets, lhs_zero, lhs_one).expect("lhs branch");
    let rhs_zero = sets
        .singleton(fact(0x0000_0000, 1, 5), provenance())
        .expect("small arena");
    let rhs_one = sets
        .singleton(fact(0x8000_0000, 1, 7), provenance())
        .expect("small arena");
    let rhs = merged(&mut sets, rhs_zero, rhs_one).expect("rhs branch");

    let Err(FunctionReadMergeError::Conflict(conflict)) = merged(&mut sets, lhs, rhs) else {
        panic!("both keys conflict");
    };
    assert_eq!(conflict.found.value, 0);
    assert_eq!(conflict.found.witness, 5);
}

#[test]
fn subset_union_reuses_the_superset_without_allocating() {
    let mut sets = FunctionReadSets::default();
    let first = singleton(&mut sets, 1, 10);
    let second = singleton(&mut sets, 2, 20);
    let superset = merged(&mut sets, first, second).expect("disjoint");
    let duplicate = sets
        .singleton(fact(1, 10, 99), provenance())
        .expect("small arena");
    let before = sets.nodes.len();

    assert_eq!(
        merged(&mut sets, superset, duplicate).expect("same fact"),
        superset
    );
    assert_eq!(sets.nodes.len(), before);

    let shared_before = sets.nodes.len();
    assert_eq!(
        merged(&mut sets, first, superset).expect("shared subset in reverse"),
        superset
    );
    assert_eq!(sets.nodes.len(), shared_before);

    let reverse_before = sets.nodes.len();
    let rebuilt = merged(&mut sets, duplicate, superset).expect("same fact in reverse");
    assert_eq!(sets.nodes.len(), reverse_before + 1);
    assert!(
        sets.nodes[reverse_before..]
            .iter()
            .all(|node| !node.is_leaf())
    );
    assert_eq!(
        facts(&sets, rebuilt)
            .into_iter()
            .find(|fact| fact.value == 1)
            .expect("duplicate key remains present")
            .witness,
        99
    );
    audit(&sets, rebuilt);
}

#[test]
fn sequential_merge_matches_a_small_ordered_map_oracle() {
    let cases: &[&[(u32, u32)]] = &[
        &[(0, 0), (1, 1), (2, 2), (3, 3)],
        &[(3, 8), (1, 6), (3, 8), (0, 5), (u32::MAX, 9)],
        &[(0, 0), (8, 0), (4, 0), (12, 0), (8, 1)],
        &[(u32::MAX, 4), (0, 3), (u32::MAX, 5), (0, 6)],
    ];
    for sequence in cases {
        let mut sets = FunctionReadSets::default();
        let mut root = FunctionReadSet::EMPTY;
        let mut oracle = BTreeMap::new();
        let mut expected_conflict = None;
        for (witness, &(value, definition)) in sequence.iter().enumerate() {
            let witness = witness as u32;
            if let Some(conflict) = oracle_conflict(&oracle, value, definition, witness) {
                expected_conflict = Some(conflict);
                let singleton = sets
                    .singleton(fact(value, definition, witness), provenance())
                    .expect("small arena");
                assert_conflicting_merge(&mut sets, root, singleton, conflict);
                break;
            }
            oracle.entry(value).or_insert(definition);
            let singleton = sets
                .singleton(fact(value, definition, witness), provenance())
                .expect("small arena");
            root = merged(&mut sets, root, singleton).expect("oracle says compatible");
            audit(&sets, root);
        }
        if expected_conflict.is_none() {
            assert_eq!(
                facts(&sets, root)
                    .into_iter()
                    .map(|fact| (fact.value, fact.definition))
                    .collect::<BTreeMap<_, _>>(),
                oracle
            );
        }
    }
}
