use std::cell::Cell;

use super::*;

thread_local! {
    static CAUSALITY_VISITS: Cell<usize> = const { Cell::new(0) };
}

pub(super) fn record_causality_visit() {
    CAUSALITY_VISITS.set(CAUSALITY_VISITS.get() + 1);
}

/// Causality visits on this thread since the last call.
pub(super) fn take_causality_visits() -> usize {
    CAUSALITY_VISITS.replace(0)
}

fn fixture() -> PreparedScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("prepared_capabilities.mo"),
        0,
        1,
    );
    let rows = vec![
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
            LinearOp::LoadY { dst: 3, index: 1 },
            LinearOp::LoadP { dst: 4, index: 1 },
            LinearOp::Binary {
                dst: 5,
                op: BinaryOp::Sub,
                lhs: 3,
                rhs: 4,
            },
            LinearOp::StoreOutput { src: 5 },
        ],
        vec![
            LinearOp::LoadY { dst: 0, index: 2 },
            LinearOp::LoadSeed { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    ];
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(
            rows,
            span.require_provenance("prepared capability fixture")
                .unwrap(),
        )
        .unwrap(),
    )
    .unwrap()
}

fn assert_assignment_capabilities(prepared: &PreparedScalarProgramBlock) {
    for (output, target) in [(0, 0), (1, 1)] {
        assert!(prepared.certifies_direct_target_assignment(0, output, target));
        assert!(prepared.certifies_exact_target_assignment_output(0, output, target));
        assert!(
            prepared
                .exact_target_assignment_output_program(0, output, target)
                .is_some()
        );
    }
    assert!(
        prepared
            .exact_target_assignment_group_program(0, &[(0, 0), (1, 1)])
            .is_some()
    );
    // A seed-dependent residual has an isolatable shape but cannot certify
    // an ordinary value assignment.
    assert!(prepared.assignment_shape_for_output(1, 0, 2).is_some());
    for (row, output, target) in [(0, 0, 1), (0, 2, 0), (1, 0, 2), (2, 0, 0)] {
        assert!(!prepared.certifies_direct_target_assignment(row, output, target));
        assert!(!prepared.certifies_exact_target_assignment_output(row, output, target));
        assert!(
            prepared
                .exact_target_assignment_output_program(row, output, target)
                .is_none()
        );
    }
}

#[test]
fn assignment_capability_queries_do_not_rescan_prepared_programs() {
    CAUSALITY_VISITS.set(0);
    let prepared = fixture();
    assert!(CAUSALITY_VISITS.get() > 0, "preparation checks causality");
    CAUSALITY_VISITS.set(0);
    let cloned = prepared.clone();
    for block in [&prepared, &cloned] {
        for _ in 0..3 {
            assert_assignment_capabilities(block);
        }
    }
    assert_eq!(
        CAUSALITY_VISITS.get(),
        0,
        "immutable program causality must be established during preparation"
    );
}
