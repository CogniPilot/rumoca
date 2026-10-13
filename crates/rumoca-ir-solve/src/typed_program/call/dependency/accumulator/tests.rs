use super::*;
use rumoca_core::AffineForm;
use std::collections::BTreeSet;

fn point(input: usize, coordinate: i64) -> SolveCallDependency {
    SolveCallDependency::exact(
        input,
        Coordinates::access(0, &[], vec![AffineForm::constant(coordinate, 0)]),
    )
}

/// Ordered distinct relations the sequence places on one leaf; empty when a
/// whole-input cover dominates it.
fn leaf_relations(sequence: &[SolveCallDependency], leaf: usize) -> Option<Vec<Coordinates>> {
    let covering = || {
        sequence
            .iter()
            .filter(|value| value.inputs().contains(&leaf))
    };
    if covering().any(SolveCallDependency::is_whole_input) {
        return None;
    }
    let mut relations: Vec<Coordinates> = Vec::new();
    for coordinates in covering().filter_map(|value| value.coordinates.clone()) {
        if !relations.contains(&coordinates) {
            relations.push(coordinates);
        }
    }
    Some(relations)
}

/// Declarative oracle over single leaves: a whole-input cover dominates;
/// otherwise keep the first occurrence of each distinct relation, in source
/// order. Consecutive leaves with the same outcome then form one run per
/// relation, ordered by first input.
fn expected(sequence: &[SolveCallDependency]) -> Vec<SolveCallDependency> {
    let leaves: BTreeSet<_> = sequence.iter().flat_map(|value| value.inputs()).collect();
    let mut output: Vec<SolveCallDependency> = Vec::new();
    // (first leaf, leaf count, relations) of the open exact group.
    let mut group: Option<(usize, usize, Vec<Coordinates>)> = None;
    let flush =
        |group: &mut Option<(usize, usize, Vec<Coordinates>)>,
         output: &mut Vec<SolveCallDependency>| {
            if let Some((start, count, relations)) = group.take() {
                output.extend(relations.into_iter().map(|relation| {
                    SolveCallDependency::exact_run(start, start + count, relation)
                }));
            }
        };
    for leaf in leaves {
        let Some(relations) = leaf_relations(sequence, leaf) else {
            flush(&mut group, &mut output);
            SolveCallDependency::push_whole_leaf(&mut output, leaf);
            continue;
        };
        group = match group.take() {
            Some((start, count, open)) if start + count == leaf && open == relations => {
                Some((start, count + 1, open))
            }
            previous => {
                group = previous;
                flush(&mut group, &mut output);
                Some((leaf, 1, relations))
            }
        };
    }
    flush(&mut group, &mut output);
    output
}

#[test]
fn exact_union_matches_order_and_absorption_for_every_small_sequence() {
    let alphabet = [
        point(1, 3),
        point(0, 2),
        point(0, 1),
        SolveCallDependency::whole(0),
        SolveCallDependency::whole(1),
        SolveCallDependency::whole_run(1, 3),
        SolveCallDependency::whole(3),
    ];
    for length in 0..=6 {
        for mut code in 0..alphabet.len().pow(length) {
            let mut sequence = Vec::new();
            let mut dependencies = Dependencies::default();
            for _ in 0..length {
                let value = alphabet[code % alphabet.len()].clone();
                code /= alphabet.len();
                sequence.push(value.clone());
                dependencies.insert(value);
            }
            assert_eq!(dependencies.finish(), expected(&sequence));
        }
    }
}

#[test]
fn wide_exact_union_retains_distinct_coordinates_and_first_occurrence_order() {
    let mut dependencies = Dependencies::default();
    for coordinate in (0..32768).rev() {
        dependencies.insert(point(4, coordinate));
    }
    for coordinate in 0..32768 {
        dependencies.insert(point(4, coordinate));
    }
    let output = dependencies.finish();
    assert_eq!(output.len(), 32768);
    assert!(
        output
            .iter()
            .enumerate()
            .all(|(index, value)| { *value == point(4, 32767 - i64::try_from(index).unwrap()) })
    );
}

#[test]
fn coordinate_identity_includes_rank_free_dimensions_and_affine_coefficients() {
    let variants = [
        Coordinates::access(1, &[2], vec![AffineForm::constant(1, 2)]),
        Coordinates::access(0, &[2], vec![AffineForm::constant(1, 1)]),
        Coordinates::access(1, &[3], vec![AffineForm::constant(1, 2)]),
        Coordinates::access(1, &[2], vec![AffineForm::constant(2, 2)]),
        Coordinates::access(1, &[2], vec![AffineForm::unit_binder(0, 2)]),
        Coordinates::access(1, &[2], vec![AffineForm::unit_binder(1, 2)]),
    ];
    let mut dependencies = Dependencies::default();
    for coordinates in variants.iter().chain(variants.iter().rev()) {
        dependencies.insert(SolveCallDependency::exact(0, coordinates.clone()));
    }
    assert_eq!(
        dependencies
            .finish()
            .into_iter()
            .map(|value| value.coordinates.unwrap())
            .collect::<Vec<_>>(),
        variants,
    );
}

#[test]
fn adjacent_and_overlapping_whole_inputs_merge_into_maximal_runs() {
    let mut dependencies = Dependencies::default();
    for input in [5, 3, 4, 10, 11, 20] {
        dependencies.insert(SolveCallDependency::whole(input));
    }
    dependencies.insert(SolveCallDependency::whole_run(9, 13));
    assert_eq!(
        dependencies.finish(),
        vec![
            SolveCallDependency::whole_run(3, 6),
            SolveCallDependency::whole_run(9, 13),
            SolveCallDependency::whole(20),
        ]
    );
}

#[test]
fn a_whole_input_run_absorbs_exact_relations_on_every_leaf_it_covers() {
    let mut dependencies = Dependencies::default();
    dependencies.insert(point(2, 7));
    dependencies.insert(point(9, 7));
    dependencies.insert(SolveCallDependency::whole_run(0, 4));
    dependencies.insert(point(3, 1));
    dependencies.insert(point(4, 1));
    assert_eq!(
        dependencies.finish(),
        vec![
            SolveCallDependency::whole_run(0, 4),
            point(4, 1),
            point(9, 7),
        ]
    );
}

#[test]
fn a_summary_over_many_leaves_is_one_run() {
    let mut dependencies = Dependencies::default();
    for input in (0..100_000).rev() {
        dependencies.insert(SolveCallDependency::whole(input));
    }
    assert_eq!(
        dependencies.finish(),
        vec![SolveCallDependency::whole_run(0, 100_000)]
    );
}

#[test]
fn consecutive_leaves_with_one_relation_store_it_once() {
    let mut dependencies = Dependencies::default();
    for input in 0..50 {
        dependencies.insert(point(input, 3));
    }
    dependencies.insert(point(60, 3));
    let output = dependencies.finish();
    assert_eq!(output.len(), 2);
    assert_eq!(output[0].inputs(), 0..50);
    assert_eq!(output[1].inputs(), 60..61);
    assert_eq!(output[0].coordinates, point(0, 3).coordinates);
}
