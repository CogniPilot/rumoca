use super::{SignatureEntry, assignment::maximum_weight_matching, offsets};

fn signature(matrix: &[&[i64]]) -> Vec<Vec<SignatureEntry>> {
    matrix
        .iter()
        .map(|row| {
            row.iter()
                .enumerate()
                .filter_map(|(column, &order)| {
                    u32::try_from(order)
                        .ok()
                        .map(|order| SignatureEntry { column, order })
                })
                .collect()
        })
        .collect()
}

// Independent enumeration includes unmatched rows, so singular graphs exercise
// the cardinality objective as well as the derivative-order objective.
fn oracle(rows: &[Vec<SignatureEntry>], row: usize, used: u32) -> (usize, u64) {
    if row == rows.len() {
        return (0, 0);
    }
    let mut best = oracle(rows, row + 1, used);
    for entry in &rows[row] {
        let bit = 1 << entry.column;
        if used & bit == 0 {
            let (count, weight) = oracle(rows, row + 1, used | bit);
            best = best.max((count + 1, weight + u64::from(entry.order)));
        }
    }
    best
}

fn decode_signature(mut remaining: u32) -> Vec<Vec<SignatureEntry>> {
    let mut rows = vec![Vec::new(); 3];
    for row in &mut rows {
        for column in 0..3 {
            let digit = remaining % 3;
            remaining /= 3;
            if digit != 0 {
                row.push(SignatureEntry {
                    column,
                    order: digit - 1,
                });
            }
        }
    }
    rows
}

#[test]
fn every_three_by_three_signature_agrees_with_enumeration() {
    for encoded in 0..3_u32.pow(9) {
        let rows = decode_signature(encoded);
        let matching = maximum_weight_matching(&rows, 3).unwrap();
        let actual =
            matching
                .iter()
                .enumerate()
                .fold((0, 0_u64), |(count, weight), (row, column)| {
                    column.map_or((count, weight), |column| {
                        let entry = rows[row]
                            .iter()
                            .find(|entry| entry.column == column)
                            .unwrap();
                        (count + 1, weight + u64::from(entry.order))
                    })
                });
        assert_eq!(actual, oracle(&rows, 0, 0), "signature {encoded}");
        if actual.0 == 3 {
            let matching: Vec<_> = matching.into_iter().map(Option::unwrap).collect();
            let (c, d) = offsets::least_offsets(&rows, &matching).unwrap();
            assert_eq!(
                offsets::certify(&rows, &matching, &c, &d),
                Some(actual.1 as usize)
            );
        }
    }
}

#[test]
fn pendulum_offsets_account_for_hidden_velocity_constraint() {
    // x'=u, y'=v, u'=-lambda*x, v'=-lambda*y-g, x^2+y^2=L^2.
    let rows = signature(&[
        &[1, -1, 0, -1, -1],
        &[-1, 1, -1, 0, -1],
        &[0, -1, 1, -1, 0],
        &[-1, 0, -1, 1, 0],
        &[0, 0, -1, -1, -1],
    ]);
    let matching: Vec<_> = maximum_weight_matching(&rows, 5)
        .unwrap()
        .into_iter()
        .map(Option::unwrap)
        .collect();
    let (c, d) = offsets::least_offsets(&rows, &matching).unwrap();
    assert_eq!(c, [1, 1, 0, 0, 2]);
    assert_eq!(d, [2, 2, 1, 1, 0]);
    assert_eq!(offsets::certify(&rows, &matching, &c, &d), Some(2));
}

#[test]
fn second_order_form_has_the_same_formal_dimension() {
    let rows = signature(&[&[2, -1, 0], &[-1, 2, 0], &[0, 0, -1]]);
    let matching: Vec<_> = maximum_weight_matching(&rows, 3)
        .unwrap()
        .into_iter()
        .map(Option::unwrap)
        .collect();
    let (c, d) = offsets::least_offsets(&rows, &matching).unwrap();
    assert_eq!(c, [0, 0, 2]);
    assert_eq!(d, [2, 2, 0]);
    assert_eq!(offsets::certify(&rows, &matching, &c, &d), Some(2));
}

#[test]
fn certificate_rejects_corrupt_matching_and_offsets() {
    let rows = signature(&[&[1, 0, -1], &[0, 1, -1], &[-1, -1, 0]]);
    assert_eq!(
        offsets::certify(&rows, &[0, 1, 2], &[0; 3], &[1, 1, 0]),
        Some(2)
    );
    for matching in [&[0, 0, 2][..], &[2, 1, 0], &[0, 1, 3], &[0, 1]] {
        assert_eq!(offsets::certify(&rows, matching, &[0; 3], &[1, 1, 0]), None);
    }
    for variables in [&[0, 1, 0][..], &[2, 1, 0], &[1, 1]] {
        assert_eq!(
            offsets::certify(&rows, &[0, 1, 2], &[0; 3], variables),
            None
        );
    }
    // Every selected edge is tight, but the off-diagonal inequalities fail.
    assert_eq!(offsets::certify(&rows, &[1, 0, 2], &[0; 3], &[0; 3]), None);
    assert_eq!(
        offsets::certify(&rows, &[0, 1, 2], &[2, 0, 0], &[1, 1, 0]),
        None
    );
}

#[test]
fn nonoptimal_assignment_cannot_issue_offsets() {
    let rows = signature(&[&[0, 1], &[1, 0]]);
    assert_eq!(
        offsets::least_offsets(&rows, &[0, 1]),
        Err("differential offsets exceed the simple-path bound")
    );
}

#[test]
fn offset_arithmetic_cannot_wrap() {
    let rows = signature(&[&[0, i64::from(u32::MAX)], &[0, -1]]);
    assert_eq!(
        offsets::least_offsets(&rows, &[1, 0]).unwrap(),
        (vec![0, 0], vec![0, u32::MAX])
    );
    let rows = signature(&[&[0, i64::from(u32::MAX)], &[1, 0]]);
    assert_eq!(
        offsets::least_offsets(&rows, &[0, 1]),
        Err("differential order overflow")
    );
}

#[test]
fn rectangular_and_empty_graphs_preserve_maximum_cardinality() {
    for (rows, columns) in [
        (signature(&[&[1, 0], &[0, -1], &[-1, 1]]), 2),
        (signature(&[&[0, 1, -1]]), 3),
        (vec![], 0),
        (vec![vec![]], 0),
        (vec![], 2),
    ] {
        let matching = maximum_weight_matching(&rows, columns).unwrap();
        assert_eq!(matching.iter().flatten().count(), oracle(&rows, 0, 0).0);
        assert_eq!(matching, maximum_weight_matching(&rows, columns).unwrap());
    }
}
