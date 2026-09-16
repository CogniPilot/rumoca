use super::*;
use crate::differential_structure::assignment::maximum_weight_matching;

fn signature(mut code: u32) -> Vec<Vec<SignatureEntry>> {
    let mut rows = vec![Vec::new(); 2];
    for row in &mut rows {
        for column in 0..2 {
            let digit = code % 3;
            code /= 3;
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

/// Enumerate equation orders independently; the least permissible variable
/// orders follow directly from the dual inequalities and column equalities.
fn oracle(
    rows: &[Vec<SignatureEntry>],
    matching: &[usize],
    groups: OwnerGroups<'_>,
) -> Option<OffsetVectors> {
    let mut best: Option<OffsetVectors> = None;
    for encoded in 0..25 {
        let c = vec![encoded / 5, encoded % 5];
        if !uniform(&c, groups.rows) {
            continue;
        }
        let mut d = vec![0; 2];
        for (i, row) in rows.iter().enumerate() {
            for entry in row {
                d[entry.column] = d[entry.column].max(c[i] + entry.order);
            }
        }
        for group in groups.columns {
            let value = d[group.clone()].iter().copied().max().unwrap();
            d[group.clone()].fill(value);
        }
        if offsets::certify(rows, matching, &c, &d).is_none() {
            continue;
        }
        if best.as_ref().is_none_or(|(bc, bd)| {
            c.iter().chain(&d).sum::<u32>() < bc.iter().chain(bd).sum::<u32>()
        }) {
            best = Some((c, d));
        }
    }
    best
}

#[test]
fn all_two_by_two_signatures_and_owner_partitions_match_enumeration() {
    let partitions = [vec![0..1, 1..2], std::iter::once(0..2).collect()];
    for code in 0..3_u32.pow(4) {
        let rows = signature(code);
        let Some(matching) = maximum_weight_matching(&rows, 2)
            .unwrap()
            .into_iter()
            .collect::<Option<Vec<_>>>()
        else {
            continue;
        };
        let initial = offsets::least_offsets(&rows, &matching).unwrap();
        for row_groups in &partitions {
            for column_groups in &partitions {
                let groups = OwnerGroups {
                    rows: row_groups,
                    columns: column_groups,
                };
                let actual = refine(&rows, &matching, (&initial.0, &initial.1), groups).unwrap();
                assert_eq!(
                    actual,
                    oracle(&rows, &matching, groups),
                    "signature {code}, row groups {row_groups:?}, column groups {column_groups:?}"
                );
            }
        }
    }
}

#[test]
fn zero_length_owners_do_not_create_equations_or_break_the_partition() {
    let rows = vec![vec![SignatureEntry {
        column: 0,
        order: 1,
    }]];
    let groups = [0..0, 0..1, 1..1];
    assert_eq!(
        refine(
            &rows,
            &[0],
            (&[0], &[1]),
            OwnerGroups {
                rows: &groups,
                columns: &groups
            }
        )
        .unwrap(),
        Some((vec![0], vec![1]))
    );
    assert!(uniform(&[1], &groups));
}

#[test]
fn incompatible_uniform_owners_do_not_reject_the_scalar_signature() {
    let rows = vec![
        vec![
            SignatureEntry {
                column: 0,
                order: 1,
            },
            SignatureEntry {
                column: 1,
                order: 0,
            },
        ],
        vec![SignatureEntry {
            column: 1,
            order: 0,
        }],
    ];
    let groups = std::iter::once(0..2).collect::<Vec<_>>();
    assert_eq!(offsets::certify(&rows, &[0, 1], &[0, 0], &[1, 0]), Some(1));
    assert!(
        refine(
            &rows,
            &[0, 1],
            (&[0, 0], &[1, 0]),
            OwnerGroups {
                rows: &groups,
                columns: &groups
            }
        )
        .unwrap()
        .is_none()
    );
}

#[test]
fn refinement_rejects_invalid_partitions_and_checked_order_overflow() {
    let rows = vec![
        vec![SignatureEntry {
            column: 0,
            order: u32::MAX,
        }],
        vec![
            SignatureEntry {
                column: 0,
                order: 0,
            },
            SignatureEntry {
                column: 1,
                order: 0,
            },
        ],
    ];
    let groups = std::iter::once(0..2).collect::<Vec<_>>();
    assert!(
        refine(
            &rows,
            &[0, 1],
            (&[0, 0], &[u32::MAX, 0]),
            OwnerGroups {
                rows: &groups,
                columns: &groups
            }
        )
        .unwrap_err()
        .contains("overflow")
    );
    for bad in [
        std::iter::once(1..2).collect(),
        vec![0..1, 0..2],
        std::iter::once(0..3).collect(),
    ] {
        assert!(
            refine(
                &rows,
                &[0, 1],
                (&[0, 0], &[u32::MAX, 0]),
                OwnerGroups {
                    rows: &bad,
                    columns: &groups
                }
            )
            .unwrap_err()
            .contains("cover")
        );
    }
}
