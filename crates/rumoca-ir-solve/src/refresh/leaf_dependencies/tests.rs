use super::*;

fn reads(indices: &[usize]) -> IndexIntervals {
    IndexIntervals::of(indices.iter().copied())
}

fn bounded(indices: &[usize]) -> LeafDependencies {
    LeafDependencies::Bounded(reads(indices))
}

/// States y0..y3. y3 <- y0; y4 <- {y3, y1}; y5 <- {y6} where y6 is assigned by
/// nothing (a leaf that is not a state); y7 <- {} (constant).
#[test]
fn leaves_follow_every_assignment_chain_to_unassigned_slots() {
    let (r3, r4, r5, r7) = (reads(&[0]), reads(&[1, 3]), reads(&[6]), reads(&[]));
    let programs = [
        ProgramReads {
            targets: &[3],
            reads: vec![Some(&r3)],
        },
        ProgramReads {
            targets: &[4, 5],
            reads: vec![Some(&r4), Some(&r5)],
        },
        ProgramReads {
            targets: &[7],
            reads: vec![Some(&r7)],
        },
    ];
    let leaves = issue_reads(&programs);
    assert_eq!(&*leaves[0], &[bounded(&[0])]);
    assert_eq!(&*leaves[1], &[bounded(&[0, 1]), bounded(&[6])]);
    assert_eq!(&*leaves[2], &[bounded(&[])]);
    assert!(leaves[1][0].meets(&IndexIntervals::range(0, 1)));
    assert!(!leaves[1][1].meets(&IndexIntervals::range(0, 3)));
}

#[test]
fn a_read_range_splits_around_the_assigned_slots_inside_it() {
    let (r5, r9) = (reads(&[0]), IndexIntervals::range(2, 9));
    let programs = [
        ProgramReads {
            targets: &[5],
            reads: vec![Some(&r5)],
        },
        ProgramReads {
            targets: &[9],
            reads: vec![Some(&r9)],
        },
    ];
    let leaves = issue_reads(&programs);
    assert_eq!(&*leaves[1], &[bounded(&[0, 2, 3, 4, 6, 7, 8])]);
}

#[test]
fn a_cycle_or_an_unissued_read_is_unbounded_and_so_is_every_reader() {
    let (a, b, c) = (reads(&[2]), reads(&[1]), reads(&[1]));
    let programs = [
        ProgramReads {
            targets: &[1],
            reads: vec![Some(&a)],
        },
        ProgramReads {
            targets: &[2],
            reads: vec![Some(&b)],
        },
        ProgramReads {
            targets: &[3],
            reads: vec![Some(&c)],
        },
        ProgramReads {
            targets: &[4],
            reads: vec![None],
        },
    ];
    let leaves = issue_reads(&programs);
    assert!(
        leaves
            .iter()
            .all(|targets| targets[0] == LeafDependencies::Unbounded)
    );
}

#[test]
fn a_target_assigned_twice_depends_on_both_assignments() {
    let (first, second, reader) = (reads(&[0]), reads(&[1]), reads(&[5]));
    let programs = [
        ProgramReads {
            targets: &[5],
            reads: vec![Some(&first)],
        },
        ProgramReads {
            targets: &[5],
            reads: vec![Some(&second)],
        },
        ProgramReads {
            targets: &[6],
            reads: vec![Some(&reader)],
        },
    ];
    let leaves = issue_reads(&programs);
    assert_eq!(&*leaves[2], &[bounded(&[0, 1])]);
}
