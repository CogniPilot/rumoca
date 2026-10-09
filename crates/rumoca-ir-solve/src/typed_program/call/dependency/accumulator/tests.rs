use super::*;
use rumoca_core::AffineForm;
use std::collections::BTreeSet;

fn point(input: usize, coordinate: i64) -> SolveCallDependency {
    SolveCallDependency {
        input,
        coordinates: Some(Coordinates::access(
            0,
            &[],
            vec![AffineForm::constant(coordinate, 0)],
        )),
    }
}

/// Declarative oracle: whole input dominates; otherwise keep the first
/// occurrence of each distinct relation, grouped by ascending input.
fn expected(sequence: &[SolveCallDependency]) -> Vec<SolveCallDependency> {
    let inputs: BTreeSet<_> = sequence.iter().map(|value| value.input).collect();
    let mut output = Vec::new();
    for input in inputs {
        if sequence
            .iter()
            .any(|value| value.input == input && value.is_whole_input())
        {
            output.push(SolveCallDependency::whole(input));
            continue;
        }
        for (index, value) in sequence.iter().enumerate() {
            if value.input == input && !sequence[..index].contains(value) {
                output.push(value.clone());
            }
        }
    }
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
        dependencies.insert(SolveCallDependency {
            input: 0,
            coordinates: Some(coordinates.clone()),
        });
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
