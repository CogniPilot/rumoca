//! Executable isolation programs agree with the unchecked isolation evaluator.

use super::*;

fn prepare(program: Vec<LinearOp>) -> PreparedScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("isolation_program.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_source_span(
            vec![program],
            span.require_provenance("isolation program").unwrap(),
        )
        .unwrap(),
    )
    .unwrap()
}

/// Three outputs of one row: `y0 - p0`, `y1 - 3`, and `y0 * y0`.
fn three_output_row() -> Vec<LinearOp> {
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
        LinearOp::Const { dst: 4, value: 3.0 },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Sub,
            lhs: 3,
            rhs: 4,
        },
        LinearOp::StoreOutput { src: 5 },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::StoreOutput { src: 6 },
    ]
}

const Y: [f64; 2] = [11.0, -7.0];
const P: [f64; 1] = [5.0];

fn unchecked_isolation(
    prepared: &PreparedScalarProgramBlock,
    output_offset: usize,
    target_y_index: usize,
) -> Option<f64> {
    prepared
        .eval_target_assignment_output_unchecked_with_context(TargetAssignmentOutputRequest {
            row_idx: 0,
            output_offset,
            target_y_index,
            y: &Y,
            p: &P,
            t: 0.0,
            context: RowEvalContext::default(),
        })
        .unwrap()
}

fn eval_program(program: Vec<LinearOp>, outputs: usize) -> Vec<f64> {
    let mut out = vec![f64::NAN; outputs];
    prepare(program)
        .eval_with_context(&Y, &P, 0.0, RowEvalContext::default(), &mut out)
        .unwrap();
    out
}

fn is_store(op: &LinearOp) -> bool {
    matches!(
        op,
        LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
    )
}

#[test]
fn shaped_output_isolator_replays_its_prefix_and_matches_the_evaluator() {
    let row = three_output_row();
    let prepared = prepare(row.clone());
    for (output, target, expected) in [(0, 0, P[0]), (1, 1, 3.0)] {
        let prefix_len = prepared
            .target_isolation_prefix_len(0, output, target)
            .expect("an affine output has an assignment shape");
        let TargetIsolationProgram::Isolator(program) =
            prepared.target_isolation_output_program(0, output, target)
        else {
            panic!("output {output} isolates target {target}");
        };
        let prefix = row[..prefix_len]
            .iter()
            .filter(|op| !is_store(op))
            .cloned()
            .collect::<Vec<_>>();
        assert_eq!(&program[..prefix.len()], prefix.as_slice());
        assert!(matches!(program.last(), Some(LinearOp::StoreOutput { .. })));
        assert_eq!(
            program.iter().filter(|op| is_store(op)).count(),
            1,
            "an isolator stores exactly its one isolated value"
        );
        let isolated = eval_program(program, 1)[0];
        assert_eq!(isolated, expected);
        assert_eq!(
            unchecked_isolation(&prepared, output, target),
            Some(isolated)
        );
    }
}

#[test]
fn unshaped_outputs_are_their_own_value_or_unavailable() {
    let prepared = prepare(three_output_row());
    // Output 0 is owned by its shape for `y0`; no other target isolates it.
    assert_eq!(
        prepared.target_isolation_output_program(0, 0, 1),
        TargetIsolationProgram::Unavailable
    );
    assert_eq!(unchecked_isolation(&prepared, 0, 1), None);
    // `y0 * y0` reads `y0` without an assignment shape.
    assert_eq!(
        prepared.target_isolation_output_program(0, 2, 0),
        TargetIsolationProgram::Unavailable
    );
    assert_eq!(prepared.target_isolation_prefix_len(0, 2, 0), None);
    // `y0 * y0` does not read `y1`, so its isolation is its own value.
    assert_eq!(
        prepared.target_isolation_output_program(0, 2, 1),
        TargetIsolationProgram::OutputValue
    );
    assert_eq!(unchecked_isolation(&prepared, 2, 1), Some(Y[0] * Y[0]));
    // A row outside the block has no isolation at all.
    assert_eq!(
        prepared.target_isolation_output_program(1, 0, 0),
        TargetIsolationProgram::Unavailable
    );
    assert_eq!(prepared.target_isolation_prefix_len(1, 0, 0), None);
    assert_eq!(prepared.target_isolation_group_program(1, &[(0, 0)]), None);
}

#[test]
fn group_program_requires_one_shaped_prefix_for_every_pair() {
    let prepared = prepare(three_output_row());
    let TargetIsolationProgram::Isolator(single) =
        prepared.target_isolation_output_program(0, 0, 0)
    else {
        panic!("output 0 isolates y0");
    };
    assert_eq!(
        prepared.target_isolation_group_program(0, &[(0, 0)]),
        Some(single),
        "a one-pair group is exactly that pair's isolator"
    );
    assert_ne!(
        prepared.target_isolation_prefix_len(0, 0, 0),
        prepared.target_isolation_prefix_len(0, 1, 1)
    );
    assert_eq!(
        prepared.target_isolation_group_program(0, &[(0, 0), (1, 1)]),
        None,
        "pairs over different prefix lengths do not share one evaluation"
    );
    assert_eq!(
        prepared.target_isolation_group_program(0, &[(0, 0), (2, 0)]),
        None,
        "a pair without an assignment shape refuses the group"
    );
    assert_eq!(prepared.target_isolation_group_program(0, &[]), None);
}

#[test]
fn group_program_stores_each_isolator_of_one_shared_tensor_prefix() {
    let prepared = prepare(vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: 1,
        },
    ]);
    let pairs = [(0, 0), (1, 1)];
    let prefix_len = prepared.target_isolation_prefix_len(0, 0, 0);
    assert!(prefix_len.is_some());
    assert_eq!(prepared.target_isolation_prefix_len(0, 1, 1), prefix_len);
    let group = prepared
        .target_isolation_group_program(0, &pairs)
        .expect("zero assignments over one tensor load share their prefix");
    assert_eq!(group.iter().filter(|op| is_store(op)).count(), pairs.len());
    let grouped = eval_program(group, pairs.len());
    for (slot, &(output, target)) in pairs.iter().enumerate() {
        let TargetIsolationProgram::Isolator(single) =
            prepared.target_isolation_output_program(0, output, target)
        else {
            panic!("tensor output {output} isolates y{target}");
        };
        let isolated = eval_program(single, 1)[0];
        assert_eq!(grouped[slot], isolated);
        assert_eq!(
            unchecked_isolation(&prepared, output, target),
            Some(isolated)
        );
    }
}
