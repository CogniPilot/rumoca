//! Index-set operations checked against explicit element enumeration of
//! small sets (a test oracle only; the analysis never enumerates).

use super::*;
use std::collections::BTreeSet;

fn elements(progression: Progression) -> BTreeSet<i64> {
    (0..progression.count)
        .map(|k| progression.first + k * progression.step)
        .collect()
}

fn progressions() -> Vec<Progression> {
    let mut all = vec![Progression::EMPTY];
    for lower in -3..=4 {
        for step in [-3, -2, -1, 1, 2, 3] {
            for upper in -4..=6 {
                all.push(Progression::range(lower, step, upper).unwrap());
            }
        }
    }
    all
}

#[test]
fn progression_operations_match_their_elements() {
    let all = progressions();
    for lhs in &all {
        for rhs in &all {
            let expected = elements(*lhs)
                .intersection(&elements(*rhs))
                .copied()
                .collect::<BTreeSet<_>>();
            assert_eq!(elements(lhs.intersect(*rhs).unwrap()), expected);
            let rest = lhs
                .difference(*rhs)
                .unwrap()
                .into_iter()
                .flat_map(elements)
                .collect::<Vec<_>>();
            let unique = rest.iter().copied().collect::<BTreeSet<_>>();
            assert_eq!(unique.len(), rest.len(), "difference parts are disjoint");
            let expected = elements(*lhs)
                .difference(&elements(*rhs))
                .copied()
                .collect::<BTreeSet<_>>();
            assert_eq!(unique, expected, "{lhs:?} \\ {rhs:?}");
        }
    }
}

#[test]
fn affine_images_match_their_elements() {
    for progression in progressions() {
        for a in [-2, -1, 0, 1, 3] {
            let image = progression.affine_image(a, 5).unwrap();
            let expected = elements(progression)
                .into_iter()
                .map(|v| a * v + 5)
                .collect::<BTreeSet<_>>();
            assert_eq!(elements(image), expected);
        }
    }
}

#[test]
fn a_union_of_boxes_covers_exactly_its_elements() {
    let row = |r: i64| {
        IndexBox(vec![
            Progression::single(r),
            Progression::range(1, 1, 4).unwrap(),
        ])
    };
    let mut set = IndexUnion::of([row(1), row(3)]);
    let whole = IndexBox::whole(&[3, 4]).unwrap();
    assert!(!set.contains_box(&whole));
    set.extend(IndexUnion::of([row(2)]));
    assert!(set.contains_box(&whole));
    // Even and odd columns together cover the rows they span.
    let columns = IndexUnion::of([
        IndexBox(vec![
            Progression::range(1, 1, 2).unwrap(),
            Progression::range(1, 2, 4).unwrap(),
        ]),
        IndexBox(vec![
            Progression::range(1, 1, 2).unwrap(),
            Progression::range(2, 2, 4).unwrap(),
        ]),
    ]);
    assert!(columns.contains_box(&IndexBox::whole(&[2, 4]).unwrap()));
    assert!(!columns.contains_box(&whole));
}

fn axis(coeffs: Vec<i64>, constant: i64, offsets: Vec<Progression>) -> IndexAxis {
    IndexAxis {
        form: AffineForm { constant, coeffs },
        offsets,
    }
}

/// Every index tuple the axes name at one binder point (test oracle).
fn named_at(axes: &[IndexAxis], point: &[i64]) -> Vec<Vec<i64>> {
    let mut tuples = vec![Vec::new()];
    for axis in axes {
        let base = axis.form.constant
            + axis
                .form
                .coeffs
                .iter()
                .zip(point)
                .map(|(scale, value)| scale * value)
                .sum::<i64>();
        let values = axis
            .offsets
            .iter()
            .flat_map(|offsets| elements(*offsets))
            .map(|offset| base + offset)
            .collect::<Vec<_>>();
        tuples = tuples
            .iter()
            .flat_map(|prefix| {
                values.iter().map(move |value| {
                    let mut next = prefix.clone();
                    next.push(*value);
                    next
                })
            })
            .collect();
    }
    tuples
}

fn points(region: &IndexBox) -> Vec<Vec<i64>> {
    let mut all = vec![Vec::new()];
    for values in &region.0 {
        let mut extended = Vec::new();
        for prefix in &all {
            for value in elements(*values) {
                let mut next: Vec<i64> = prefix.clone();
                next.push(value);
                extended.push(next);
            }
        }
        all = extended;
    }
    all
}

fn set_elements(set: &IndexUnion, dims: usize, lo: i64, hi: i64) -> BTreeSet<Vec<i64>> {
    let mut all = BTreeSet::new();
    let mut cursor = vec![lo; dims];
    loop {
        if set.contains_box(&IndexBox(
            cursor
                .iter()
                .map(|value| Progression::single(*value))
                .collect(),
        )) {
            all.insert(cursor.clone());
        }
        let mut d = 0;
        loop {
            if d == dims {
                return all;
            }
            cursor[d] += 1;
            if cursor[d] <= hi {
                break;
            }
            cursor[d] = lo;
            d += 1;
        }
    }
}

fn shapes() -> Vec<(Vec<IndexAxis>, IndexBox)> {
    let region = IndexBox(vec![
        Progression::range(1, 2, 7).unwrap(),
        Progression::range(4, -1, 2).unwrap(),
    ]);
    vec![
        // y[i, j]
        (
            vec![
                axis(vec![1, 0], 0, vec![Progression::single(0)]),
                axis(vec![0, 1], 0, vec![Progression::single(0)]),
            ],
            region.clone(),
        ),
        // y[2*i - 1 : 2*i, 5 - j] (a block per i, reversed j)
        (
            vec![
                axis(vec![2, 0], -1, vec![Progression::range(0, 1, 1).unwrap()]),
                axis(vec![0, -1], 5, vec![Progression::single(0)]),
            ],
            region.clone(),
        ),
        // y[i, i] (a diagonal: superset)
        (
            vec![
                axis(vec![1, 0], 0, vec![Progression::single(0)]),
                axis(vec![1, 0], 0, vec![Progression::single(0)]),
            ],
            region.clone(),
        ),
        // y[2*i + j] (a tiled row-major index: exact) and y[i + j] (superset)
        (
            vec![axis(vec![2, 1], 0, vec![Progression::single(0)])],
            IndexBox(vec![
                Progression::range(0, 1, 3).unwrap(),
                Progression::range(1, 1, 2).unwrap(),
            ]),
        ),
        (
            vec![axis(vec![1, 1], 0, vec![Progression::single(0)])],
            region,
        ),
    ]
}

#[test]
fn images_contain_every_named_index_and_are_exact_when_declared() {
    for (axes, region) in shapes() {
        let expected = points(&region)
            .iter()
            .flat_map(|point| named_at(&axes, point))
            .collect::<BTreeSet<_>>();
        let answer = image(&axes, &region).unwrap();
        let exact = matches!(answer, SetAnswer::Exact(_));
        let found = set_elements(&answer.bound(), axes.len(), -2, 16);
        assert!(expected.is_subset(&found), "{axes:?}");
        if exact {
            assert_eq!(found, expected, "{axes:?}");
        }
    }
}

#[test]
fn point_solutions_are_exactly_the_writing_iterations() {
    for (axes, region) in shapes() {
        for index in points(&IndexBox(vec![
            Progression::range(-1, 1, 15).unwrap();
            axes.len()
        ])) {
            let expected = points(&region)
                .into_iter()
                .filter(|point| named_at(&axes, point).contains(&index))
                .collect::<BTreeSet<_>>();
            let answer = solve_point(&axes, &region, &index).unwrap();
            let exact = matches!(answer, SetAnswer::Exact(_));
            let found = set_elements(&answer.bound(), region.0.len(), -2, 16);
            assert!(expected.is_subset(&found), "{axes:?} {index:?}");
            if exact {
                assert_eq!(found, expected, "{axes:?} {index:?}");
            }
        }
    }
}

#[test]
fn a_meet_holds_exactly_the_common_indices() {
    let lhs = IndexUnion::of([IndexBox(vec![
        Progression::range(1, 1, 6).unwrap(),
        Progression::range(1, 2, 7).unwrap(),
    ])]);
    let rhs = IndexUnion::of([
        IndexBox(vec![
            Progression::range(4, 1, 9).unwrap(),
            Progression::range(1, 3, 10).unwrap(),
        ]),
        IndexBox(vec![Progression::single(2), Progression::single(3)]),
    ]);
    let meet = lhs.meet(&rhs).unwrap();
    let expected = set_elements(&lhs, 2, -2, 16)
        .intersection(&set_elements(&rhs, 2, -2, 16))
        .cloned()
        .collect::<BTreeSet<_>>();
    assert_eq!(set_elements(&meet, 2, -2, 16), expected);
    assert!(lhs.meet(&IndexUnion::default()).unwrap().is_empty());
}

#[test]
fn a_point_solution_refuses_a_form_wider_than_its_region() {
    let axes = [axis(vec![1, 1], 0, vec![Progression::single(0)])];
    let region = IndexBox(vec![Progression::range(1, 1, 3).unwrap()]);
    assert_eq!(solve_point(&axes, &region, &[2]), None);
}

#[test]
fn cost_does_not_follow_extent() {
    // A billion-element range and its strided image stay single boxes.
    let region = IndexBox(vec![Progression::range(1, 1, 1_000_000_000).unwrap()]);
    let axes = [axis(
        vec![2],
        -1,
        vec![Progression::range(0, 1, 1).unwrap()],
    )];
    let SetAnswer::Exact(set) = image(&axes, &region).unwrap() else {
        panic!("a tiled block image is exact");
    };
    assert_eq!(set.boxes().len(), 1);
    assert!(set.contains_box(&IndexBox::whole(&[2_000_000_000]).unwrap()));
    let SetAnswer::Exact(writers) = solve_point(&axes, &region, &[1_234_567_890]).unwrap() else {
        panic!("a one-binder solution is exact");
    };
    assert_eq!(
        writers.boxes(),
        &[IndexBox(vec![Progression::single(617_283_945)])]
    );
}
