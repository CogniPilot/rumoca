use super::*;

#[test]
fn closure_selects_safe_consumers_but_not_upstream_or_history_rows() {
    let rows = vec![
        ObservationRow {
            row: 0,
            span: rumoca_core::Span::DUMMY,
            target: HistoryDependencySlot::P(9),
            reads_y: Arc::default(),
            reads_p: Arc::new(solve::IndexIntervals::singleton(200)),
            safe: true,
            seed: false,
        },
        ObservationRow {
            row: 1,
            span: rumoca_core::Span::DUMMY,
            target: HistoryDependencySlot::P(10),
            reads_y: Arc::default(),
            reads_p: Arc::new(solve::IndexIntervals::of([9, 100])),
            safe: true,
            seed: true,
        },
        ObservationRow {
            row: 2,
            span: rumoca_core::Span::DUMMY,
            target: HistoryDependencySlot::P(11),
            reads_y: Arc::default(),
            reads_p: Arc::new(solve::IndexIntervals::singleton(10)),
            safe: true,
            seed: false,
        },
        ObservationRow {
            row: 3,
            span: rumoca_core::Span::DUMMY,
            target: HistoryDependencySlot::P(12),
            reads_y: Arc::default(),
            reads_p: Arc::new(solve::IndexIntervals::singleton(11)),
            safe: false,
            seed: false,
        },
    ];

    assert_eq!(select_refresh_closure(&rows), [false, true, true, false]);
}

fn scalar_reference(rows: &[ObservationRow]) -> Vec<bool> {
    let dependencies = rows
        .iter()
        .map(|row| {
            row.reads_y
                .iter()
                .map(HistoryDependencySlot::Y)
                .chain(row.reads_p.iter().map(HistoryDependencySlot::P))
                .collect::<BTreeSet<_>>()
        })
        .collect::<Vec<_>>();
    let mut selected = rows
        .iter()
        .map(|row| row.safe && row.seed)
        .collect::<Vec<_>>();
    loop {
        let mut changed = false;
        for (index, row) in rows.iter().enumerate() {
            if selected[index] || !row.safe {
                continue;
            }
            if rows
                .iter()
                .zip(&selected)
                .any(|(active, selected)| *selected && dependencies[index].contains(&active.target))
            {
                selected[index] = true;
                changed = true;
            }
        }
        if !changed {
            return selected;
        }
    }
}

#[test]
fn compact_closure_matches_scalar_reference_for_mixed_cyclic_graphs() {
    let mut bits = 0x3157_u64;
    for case in 0..512 {
        let mut next = || {
            bits ^= bits << 13;
            bits ^= bits >> 7;
            bits ^= bits << 17;
            bits
        };
        let rows = (0..8)
            .map(|index| {
                let y = next();
                let p = next();
                ObservationRow {
                    row: index,
                    span: rumoca_core::Span::DUMMY,
                    target: if index % 2 == 0 {
                        HistoryDependencySlot::Y(index / 2)
                    } else {
                        HistoryDependencySlot::P(index / 2)
                    },
                    reads_y: Arc::new(solve::IndexIntervals::of(
                        (0..5).filter(|bit| y & (1 << bit) != 0),
                    )),
                    reads_p: Arc::new(solve::IndexIntervals::of(
                        (0..5).filter(|bit| p & (1 << bit) != 0),
                    )),
                    safe: next() & 3 != 0,
                    seed: next() & 7 == 0,
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(
            select_refresh_closure(&rows),
            scalar_reference(&rows),
            "case {case}"
        );
    }
}

#[test]
fn wide_shared_inputs_do_not_expand_during_closure() {
    let reads_p = Arc::new(solve::IndexIntervals::range(0, 14400));
    let rows = (0..14400)
        .map(|index| ObservationRow {
            row: index,
            span: rumoca_core::Span::DUMMY,
            target: HistoryDependencySlot::P(index),
            reads_y: Arc::default(),
            reads_p: Arc::clone(&reads_p),
            safe: index != 14399,
            seed: index == 7200,
        })
        .collect::<Vec<_>>();
    let selected = select_refresh_closure(&rows);
    assert!(selected[..14399].iter().all(|selected| *selected));
    assert!(!selected[14399]);
    assert_eq!(reads_p.intervals().count(), 1);
    assert!(rows.iter().all(|row| Arc::ptr_eq(&reads_p, &row.reads_p)));
}
