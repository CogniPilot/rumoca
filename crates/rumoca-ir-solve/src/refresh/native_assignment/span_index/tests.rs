use super::super::{
    Checked, ComputeBlock, Family, NativeRefreshAssignmentRefusal, NativeRefreshAssignmentStage,
    SourceProjection, coverage, coverage::Coverage, dependencies, refused,
};
use super::*;

fn family(targets: Coverage, reads: Vec<Coverage>) -> Family {
    Family {
        stage: NativeRefreshAssignmentStage {
            source_node: 0,
            source_projection: SourceProjection::Scalar {
                program: 0,
                output: 0,
                stores: Vec::new(),
            },
            targets,
            value_kernel: ComputeBlock::default(),
        },
        reads,
        outputs: 0..0,
    }
}

// Independent frozen original all-pairs algorithms. These deliberately do not
// use the new candidate index or scheduler when determining expected results.
fn old_complete(sets: &[Coverage], count: usize) -> Checked<bool> {
    let mut cardinality = 0usize;
    for (index, set) in sets.iter().enumerate() {
        if set.count == 0
            || set.stride == 0
            || set.width == 0
            || set.width > set.stride
            || set.count % set.width != 0
            || set.span.end > count
        {
            return Ok(false);
        }
        cardinality = cardinality
            .checked_add(set.count)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native target cardinality overflows",
            ))?;
        for previous in &sets[..index] {
            if set.overlaps(previous)? {
                return Ok(false);
            }
        }
    }
    Ok(cardinality == count)
}

fn old_dependencies(family: &Family, families: &[Family]) -> Checked<Vec<usize>> {
    let mut dependencies = Vec::new();
    for (index, producer) in families.iter().enumerate() {
        let mut overlaps = false;
        for read in &family.reads {
            overlaps |= read.overlaps(&producer.stage.targets)?;
        }
        if overlaps {
            dependencies.push(index);
        }
    }
    Ok(dependencies)
}

fn old_order(families: &[Family]) -> Checked<Vec<usize>> {
    let dependencies = families
        .iter()
        .map(|family| old_dependencies(family, families))
        .collect::<Checked<Vec<_>>>()?;
    let mut emitted = vec![false; families.len()];
    let mut order = Vec::with_capacity(families.len());
    while order.len() < families.len() {
        let Some(next) = (0..families.len()).find(|&index| {
            !emitted[index]
                && dependencies[index]
                    .iter()
                    .all(|&producer| emitted[producer])
        }) else {
            return refused("native assignment dependency cycle");
        };
        emitted[next] = true;
        order.push(next);
    }
    Ok(order)
}

fn patterns(limit: usize) -> Vec<Coverage> {
    let mut sets = Vec::new();
    for start in 0..limit {
        for width in 1..=3 {
            for stride in width..=4 {
                append_blocks(&mut sets, limit, start, width, stride);
            }
        }
    }
    sets
}

fn append_blocks(
    sets: &mut Vec<Coverage>,
    limit: usize,
    start: usize,
    width: usize,
    stride: usize,
) {
    for blocks in 1..=3 {
        let end = start + (blocks - 1) * stride + width;
        if end <= limit {
            sets.push(Coverage {
                span: start..end,
                count: blocks * width,
                stride,
                width,
            });
        }
    }
}

fn same_complete(sets: &[Coverage], count: usize) {
    assert_eq!(
        coverage::complete(sets.iter(), count),
        old_complete(sets, count)
    );
}

#[test]
fn bounding_index_matches_every_small_range_query_in_source_order() {
    let spans = [
        4..7,
        0..3,
        2..9,
        3..3,
        7..8,
        1..2,
        std::ops::Range { start: 8, end: 6 },
    ];
    let index = SpanIndex::new(spans.iter().cloned());
    for start in 0..=10 {
        for end in 0..=10 {
            let range = start..end;
            let mut actual = Vec::new();
            index.visit(&range, &mut |source| actual.push(source));
            actual.sort_unstable();
            let expected = spans
                .iter()
                .enumerate()
                .filter_map(|(source, span)| {
                    (start < end && span.start < end && start < span.end).then_some(source)
                })
                .collect::<Vec<_>>();
            assert_eq!(actual, expected, "{range:?}");
        }
    }
}

#[test]
fn exhaustive_small_periodic_coverage_preserves_complete_all_pairs_decisions() {
    let sets = patterns(5);
    for count in 0..=6 {
        same_complete(&[], count);
    }
    for a in &sets {
        for count in 0..=6 {
            same_complete(std::slice::from_ref(a), count);
        }
        for b in &sets {
            for count in 0..=6 {
                same_complete(&[a.clone(), b.clone()], count);
            }
            for c in &sets {
                same_complete(&[a.clone(), b.clone(), c.clone()], 5);
            }
        }
    }
    // Overlapping bounding spans do not own each other's periodic gaps.
    let complementary = [
        Coverage {
            span: 0..6,
            count: 4,
            stride: 4,
            width: 2,
        },
        Coverage::dense(2..4),
    ];
    assert_eq!(coverage::complete(complementary.iter(), 6), Ok(true));
}

#[test]
fn periodic_candidates_preserve_original_dependencies_for_all_small_pattern_pairs() {
    let sets = patterns(5);
    for a in &sets {
        for b in &sets {
            let mut families = vec![family(a.clone(), Vec::new()), family(b.clone(), Vec::new())];
            let index = SpanIndex::new(
                families
                    .iter()
                    .map(|family| family.stage.targets.span.clone()),
            );
            let mut candidates = dependencies::CandidateOrdinals::new(families.len());
            for read in &sets {
                families[0].reads = vec![read.clone(), read.clone(), Coverage::dense(2..2)];
                assert_eq!(
                    dependencies::collect(&families[0], &families, &index, &mut candidates),
                    old_dependencies(&families[0], &families)
                );
            }
        }
    }
}

#[test]
fn all_directed_graphs_up_to_four_nodes_preserve_lowest_ready_order_and_cycles() {
    for size in 0..=4 {
        for graph in 0u32..1u32 << (size * size) {
            let families = (0..size)
                .map(|consumer| {
                    let reads = (0..size)
                        .filter(|&producer| graph & (1 << (consumer * size + producer)) != 0)
                        .map(|producer| Coverage::dense(producer..producer + 1))
                        .collect();
                    family(Coverage::dense(consumer..consumer + 1), reads)
                })
                .collect::<Vec<_>>();
            assert_eq!(
                dependencies::order(&families),
                old_order(&families),
                "size={size} graph={graph}"
            );
        }
    }
    let families = vec![
        family(Coverage::dense(0..1), vec![Coverage::dense(1..2)]),
        family(Coverage::dense(1..2), Vec::new()),
        family(Coverage::dense(2..3), Vec::new()),
    ];
    assert_eq!(dependencies::order(&families), Ok(vec![1, 0, 2]));
}

#[test]
fn validation_cardinality_and_late_read_overflow_keep_original_refusals() {
    let valid = Coverage::dense(0..1);
    for invalid in [
        Coverage {
            count: 0,
            ..valid.clone()
        },
        Coverage {
            stride: 0,
            ..valid.clone()
        },
        Coverage {
            width: 0,
            ..valid.clone()
        },
        Coverage {
            width: 2,
            ..valid.clone()
        },
        Coverage {
            count: 3,
            stride: 2,
            width: 2,
            ..valid.clone()
        },
        Coverage::dense(0..9),
    ] {
        same_complete(&[invalid.clone(), valid.clone()], 5);
        same_complete(&[valid.clone(), valid.clone(), invalid], 5);
    }
    let maximal = Coverage {
        span: 0..usize::MAX,
        count: usize::MAX,
        stride: 1,
        width: 1,
    };
    same_complete(&[maximal, valid.clone()], usize::MAX);
    if usize::BITS == 64 {
        let a = Coverage {
            span: 0..usize::MAX - 1,
            count: 2,
            stride: usize::MAX - 2,
            width: 1,
        };
        let b = Coverage {
            span: 1..usize::MAX - 2,
            count: 2,
            stride: usize::MAX - 4,
            width: 1,
        };
        same_complete(&[a.clone(), b.clone()], usize::MAX);
        assert_eq!(
            old_complete(&[a.clone(), b.clone()], usize::MAX),
            Err(NativeRefreshAssignmentRefusal(
                "native target intersection overflows"
            ))
        );
        let families = vec![
            family(a, Vec::new()),
            family(Coverage::dense(9..10), vec![valid, b]),
        ];
        assert_eq!(dependencies::order(&families), old_order(&families));
        assert!(
            old_order(&families).is_err(),
            "a true early read must not hide the later overflow"
        );
    }
}

#[test]
fn reused_family_ordinals_reset_touched_candidates_and_preserve_source_order() {
    let mut families = (0..7)
        .map(|producer| family(Coverage::dense(producer..producer + 1), Vec::new()))
        .collect::<Vec<_>>();
    let index = SpanIndex::new(
        families
            .iter()
            .map(|family| family.stage.targets.span.clone()),
    );
    let mut candidates = dependencies::CandidateOrdinals::new(families.len());
    for reads in [
        vec![Coverage::dense(0..7); 32],
        Vec::new(),
        vec![
            Coverage::dense(6..7),
            Coverage::dense(2..3),
            Coverage::dense(6..7),
        ],
        vec![Coverage::dense(3..4)],
        Vec::new(),
        vec![Coverage::dense(0..7)],
    ] {
        families[0].reads = reads;
        assert_eq!(
            dependencies::collect(&families[0], &families, &index, &mut candidates),
            old_dependencies(&families[0], &families)
        );
    }
}

#[test]
fn completed_discovery_keeps_later_periodic_refusal_and_resets_after_error() {
    if usize::BITS != 64 {
        return;
    }
    let a = Coverage {
        span: 0..usize::MAX - 1,
        count: 2,
        stride: usize::MAX - 2,
        width: 1,
    };
    let b = Coverage {
        span: 1..usize::MAX - 2,
        count: 2,
        stride: usize::MAX - 4,
        width: 1,
    };
    let mut families = vec![
        family(a, Vec::new()),
        family(
            Coverage::dense(9..10),
            vec![Coverage::dense(0..usize::MAX), b],
        ),
    ];
    let index = SpanIndex::new(
        families
            .iter()
            .map(|family| family.stage.targets.span.clone()),
    );
    let mut candidates = dependencies::CandidateOrdinals::new(families.len());
    let expected = Err(NativeRefreshAssignmentRefusal(
        "native target intersection overflows",
    ));
    assert_eq!(old_dependencies(&families[1], &families), expected);
    assert_eq!(
        dependencies::collect(&families[1], &families, &index, &mut candidates),
        expected
    );
    // The first dense read discovers every producer. Skipped discovery of b
    // cannot elide its later exact arithmetic check, even after a true read.
    families[1].reads.clear();
    assert_eq!(
        dependencies::collect(&families[1], &families, &index, &mut candidates),
        Ok(Vec::new())
    );
    families[1].reads = vec![Coverage::dense(9..10)];
    assert_eq!(
        dependencies::collect(&families[1], &families, &index, &mut candidates),
        old_dependencies(&families[1], &families)
    );
}
