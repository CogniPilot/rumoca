use super::*;
use solve::{LinearOp, TensorInputKind};

fn tensor(input: TensorInputKind, count: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: 0,
        input,
        input_start: 100,
        count,
        seed_start: None,
        lanes: 1,
    }
}

fn canonical_depends_on(source: &[LinearOp], output: usize, target: usize) -> bool {
    match solve::output_y_reads(source, output) {
        solve::OutputYReads::Bounded(reads) => reads.contains(target),
        solve::OutputYReads::Absent | solve::OutputYReads::Unbounded => true,
    }
}

#[test]
fn seven_million_register_source_shares_one_proof_across_ranged_and_scalar_rows() {
    let mut source = vec![
        tensor(TensorInputKind::Y, 7_000_000),
        LinearOp::StoreOutputRange {
            start: 0,
            count: 14400,
            stride: 1,
        },
    ];
    source.extend((0..32).map(|offset| LinearOp::StoreOutput {
        src: 6_999_968 + offset,
    }));
    let mut reads = OutputDependencies::new(&source);
    for output in 0..14432 {
        let target = 100
            + if output < 14400 {
                output
            } else {
                6_999_968 + output - 14400
            };
        assert!(reads.depends_on(output, target));
        assert!(!reads.depends_on(output, target + 1));
    }
    assert!(reads.depends_on(14432, 0), "missing output is fail-closed");
    assert_eq!(
        reads.complete_builds, 1,
        "one source proof, not one seven-million-register inventory per row"
    );
    assert_eq!(reads.prefix_builds, 0);
    assert_eq!(reads.stores.as_ref().unwrap().store_count(), 33);
}

#[test]
fn stored_register_versions_survive_later_overwrites_and_zero_stride_outputs() {
    let source = [
        LinearOp::LoadY { dst: 0, index: 5 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::Const { dst: 0, value: 4.0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 3,
            stride: 0,
        },
        LinearOp::LoadY { dst: 0, index: 7 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let mut reads = OutputDependencies::new(&source);
    for (output, target, expected) in [
        (0, 5, true),
        (0, 7, false),
        (1, 5, false),
        (2, 7, false),
        (3, 7, false),
        (4, 7, true),
    ] {
        assert_eq!(reads.depends_on(output, target), expected);
        assert_eq!(
            reads.depends_on(output, target),
            canonical_depends_on(&source, output, target)
        );
    }
    assert!(
        !crate::prepared::row_output_depends_on_y_index(&source, 0, 5),
        "old helper reads the final overwritten register"
    );
    assert!(
        crate::prepared::row_output_depends_on_y_index(&source, 1, 7),
        "old helper skips the ranged stores"
    );
    assert!(!reads.depends_on(1, 7));
    assert!(reads.depends_on(5, 0));
    assert_eq!(reads.complete_builds, 1);
    assert_eq!(reads.prefix_builds, 0);
}

#[test]
fn refused_suffix_keeps_earlier_prefix_and_refuses_later_or_missing_outputs() {
    for refused in [
        LinearOp::LoadFoldCarried { dst: 3, index: 0 },
        LinearOp::Move { dst: 3, src: 99 },
    ] {
        let source = [
            LinearOp::LoadY { dst: 0, index: 5 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
            refused,
            LinearOp::StoreOutput { src: 1 },
        ];
        let mut reads = OutputDependencies::new(&source);
        for (output, target) in [(0, 5), (0, 6), (1, 5), (1, 6), (2, 5), (2, 6), (3, 5)] {
            assert_eq!(
                reads.depends_on(output, target),
                canonical_depends_on(&source, output, target)
            );
        }
        assert!(!reads.depends_on(1, 5));
        assert!(reads.depends_on(2, 5));
        assert!(reads.depends_on(3, 5));
        assert_eq!(
            reads.complete_builds, 1,
            "refused complete proof is retained too"
        );
        assert_eq!(
            reads.prefix_builds, 4,
            "one active prefix, replaced when returning to an earlier store"
        );
    }
}

#[test]
fn failable_operation_dependencies_keep_their_exact_inputs_and_source_identity() {
    let source = [
        LinearOp::LoadY { dst: 0, index: 5 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::StoreOutput { src: 1 },
        LinearOp::LinearSolveComponent {
            dst: 2,
            matrix_start: 0,
            rhs_start: 1,
            n: 1,
            component: 0,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let other = [
        LinearOp::LoadY { dst: 0, index: 9 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let mut reads = OutputDependencies::new(&source);
    let mut other_reads = OutputDependencies::new(&other);
    assert!(!reads.depends_on(0, 5));
    assert!(reads.depends_on(1, 5));
    assert!(!reads.depends_on(1, 9));
    assert!(!other_reads.depends_on(0, 5));
    assert!(other_reads.depends_on(0, 9));
    // Dependency analysis cannot execute or erase the singular-solve check.
    for output in 0..2 {
        for target in [5, 9] {
            assert_eq!(
                reads.depends_on(output, target),
                canonical_depends_on(&source, output, target)
            );
        }
    }
}

#[test]
fn refresh_row_uses_ranged_output_facts_and_preserves_absent_refusal() {
    let source = [
        tensor(TensorInputKind::P, 4),
        LinearOp::StoreOutputRange {
            start: 0,
            count: 4,
            stride: 1,
        },
    ];
    let program = super::super::CanonicalScalarProgram {
        source: solve::RefreshScalarProgramSource::checked(0, 0).unwrap(),
        operations: &source,
        span: rumoca_core::Span::DUMMY,
    };
    let mut cache = super::super::RowAnalysisCache::default();
    for output in 0..4 {
        let analysis =
            super::super::analyze_refresh_row(&program, (0, output, output, 200), None, &mut cache)
                .unwrap()
                .expect("parameter-only ranged output can evaluate its target");
        assert!(analysis.shape.is_none());
        assert!(!analysis.direct && !analysis.exact);
    }
    assert!(
        super::super::analyze_refresh_row(&program, (0, 4, 4, 200), None, &mut cache)
            .unwrap()
            .is_none()
    );
    let facts = cache.programs[0].as_ref().unwrap();
    assert_eq!(facts.output_dependencies.complete_builds, 1);
    assert_eq!(facts.output_dependencies.prefix_builds, 0);
}

#[test]
fn another_targets_shape_cannot_admit_an_independent_requested_target() {
    let source = [
        LinearOp::LoadY { dst: 0, index: 5 },
        LinearOp::LoadY { dst: 1, index: 7 },
        LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let program = super::super::CanonicalScalarProgram {
        source: solve::RefreshScalarProgramSource::checked(0, 0).unwrap(),
        operations: &source,
        span: rumoca_core::Span::DUMMY,
    };
    let mut cache = super::super::RowAnalysisCache::default();
    for target in [5, 7] {
        let row = super::super::analyze_refresh_row(&program, (0, 0, 0, target), None, &mut cache)
            .unwrap()
            .expect("exact requested isolator");
        assert_eq!(row.shape.unwrap().target_y_index(), target);
    }
    assert!(
        super::super::analyze_refresh_row(&program, (0, 0, 0, 9), None, &mut cache)
            .unwrap()
            .is_none()
    );
}
