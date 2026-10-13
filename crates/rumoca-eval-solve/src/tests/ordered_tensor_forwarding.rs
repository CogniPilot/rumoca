use rumoca_core::{SourceId, Span};
use rumoca_ir_solve::{
    AssignmentProgram, BinaryOp, LinearOp, ScalarProgramBlock, SharedValueSegments,
    TensorInputKind, UnaryOp,
};

use crate::{PreparedEvaluationBlock, RowEvalContext};

fn load_matrix(first: usize, dst: u32) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: dst,
        input: TensorInputKind::Y,
        input_start: first,
        count: 9,
        seed_start: None,
        lanes: 1,
    }
}

fn scalar_column(mut ops: Vec<LinearOp>, first: u32) -> Vec<LinearOp> {
    ops.pop();
    ops.extend([0, 3, 6].map(|lane| LinearOp::StoreOutput { src: first + lane }));
    ops
}

fn selected_cosine() -> Vec<LinearOp> {
    let mut ops = vec![
        LinearOp::LoadY { dst: 0, index: 45 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Cos,
            arg: 0,
        },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::Select {
            dst: 3,
            cond: 2,
            if_true: 1,
            if_false: 0,
        },
        LinearOp::Const { dst: 4, value: 0.0 },
    ];
    // Retain a long dependency chain so checked sharing still reaches the
    // existing lazy-plan admission threshold without changing positive cos.
    let mut previous = 3;
    for dst in 5..75 {
        ops.push(LinearOp::Binary {
            dst,
            op: BinaryOp::Add,
            lhs: previous,
            rhs: 4,
        });
        previous = dst;
    }
    ops.push(LinearOp::StoreOutput { src: previous });
    ops
}

#[test]
fn ordered_scalar_column_tensor_forwarding_preserves_lazy_range_versions() {
    let programs = vec![
        (scalar_column(copy_matrix(0), 0), vec![27, 30, 33]),
        (selected_cosine(), vec![0]),
        (scalar_column(copy_matrix(0), 0), vec![9, 12, 15]),
        (scalar_column(matrix_product(), 18), vec![36, 39, 42]),
    ];
    let requests = programs
        .iter()
        .map(|(ops, targets)| AssignmentProgram { ops, targets })
        .collect::<Vec<_>>();
    let shared = SharedValueSegments::derive(&requests);
    shared.check(&requests).unwrap();
    let segments = shared
        .segments()
        .iter()
        .map(|segment| (segment.ops().to_vec(), segment.targets().to_vec()))
        .collect::<Vec<_>>();
    assert!(segments.iter().any(|(ops, _)| {
        ops.len() >= 64
            && ops.iter().any(|op| matches!(op, LinearOp::Select { .. }))
            && ops
                .iter()
                .any(|op| matches!(op, LinearOp::MatrixMultiply { .. }))
    }));
    for angle in [0.0_f64, 0.75] {
        let mut y = vec![0.0; 46];
        for index in [4, 8, 13, 17, 18, 22, 26, 40, 44] {
            y[index] = 1.0;
        }
        y[45] = angle;
        let original = run(&programs, &y);
        let fused = run(&segments, &y);
        assert_eq!(original[27].to_bits(), 0.0_f64.to_bits());
        for start in [0, 9, 36] {
            for lane in 0..9 {
                let expected = match lane {
                    0 => angle.cos(),
                    4 | 8 => 1.0,
                    _ => 0.0,
                };
                assert_eq!(original[start + lane].to_bits(), expected.to_bits());
            }
        }
        assert_eq!(
            original.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            fused.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
        );
    }
}

fn copy_matrix(first: usize) -> Vec<LinearOp> {
    vec![
        load_matrix(first, 0),
        LinearOp::StoreOutputRange {
            start: 0,
            count: 9,
            stride: 1,
        },
    ]
}

fn matrix_product() -> Vec<LinearOp> {
    vec![
        load_matrix(0, 0),
        load_matrix(18, 9),
        LinearOp::MatrixMultiply {
            dst_start: 18,
            lhs_start: 0,
            rhs_start: 9,
            rows: 3,
            inner: 3,
            columns: 3,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 18,
            count: 9,
            stride: 1,
        },
    ]
}

fn run(programs: &[(Vec<LinearOp>, Vec<usize>)], initial: &[f64]) -> Vec<f64> {
    let mut y = initial.to_vec();
    let span = Span::from_offsets(
        SourceId::from_source_name("OrderedTensorForwarding.mo"),
        0,
        1,
    );
    let mut outputs = Vec::new();
    for (ops, targets) in programs {
        let block = ScalarProgramBlock::with_program_spans(vec![ops.clone()], vec![span]).unwrap();
        let prepared = PreparedEvaluationBlock::new(block).unwrap();
        prepared
            .eval_row_outputs_unchecked_with_context(
                0,
                &y,
                &[],
                0.0,
                RowEvalContext::default(),
                &mut outputs,
            )
            .unwrap();
        assert_eq!(outputs.len(), targets.len());
        for (&target, &value) in targets.iter().zip(&outputs) {
            y[target] = value;
        }
    }
    y
}

#[test]
fn ordered_tensor_reread_observes_scalar_lane_overwrite() {
    let programs = vec![
        (copy_matrix(0), (27..36).collect()),
        (
            vec![
                LinearOp::LoadY { dst: 0, index: 45 },
                LinearOp::Unary {
                    dst: 1,
                    op: UnaryOp::Cos,
                    arg: 0,
                },
                LinearOp::StoreOutput { src: 1 },
            ],
            vec![0],
        ),
        (copy_matrix(0), (9..18).collect()),
        (matrix_product(), (36..45).collect()),
    ];
    let requests = programs
        .iter()
        .map(|(ops, targets)| AssignmentProgram { ops, targets })
        .collect::<Vec<_>>();
    let shared = SharedValueSegments::derive(&requests);
    shared.check(&requests).unwrap();
    let segments = shared
        .segments()
        .iter()
        .map(|segment| (segment.ops().to_vec(), segment.targets().to_vec()))
        .collect::<Vec<_>>();
    for angle in [0.0_f64, 0.75] {
        let mut y = vec![0.0; 46];
        y[4] = 1.0;
        y[8] = 1.0;
        y[18] = 1.0;
        y[22] = 1.0;
        y[26] = 1.0;
        y[45] = angle;
        let original = run(&programs, &y);
        let fused = run(&segments, &y);
        assert_eq!(
            original.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            fused.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
        );
        assert_eq!(original[27].to_bits(), 0.0_f64.to_bits());
        for start in [0, 9, 36] {
            for lane in 0..9 {
                let expected = match lane {
                    0 => angle.cos(),
                    4 | 8 => 1.0,
                    _ => 0.0,
                };
                assert_eq!(original[start + lane].to_bits(), expected.to_bits());
            }
        }
    }
}

#[test]
fn lazy_output_store_keeps_its_authored_register_version() {
    let mut operations = vec![
        load_matrix(0, 0),
        LinearOp::StoreOutput { src: 0 },
        LinearOp::Const { dst: 0, value: 9.0 },
        LinearOp::Const { dst: 9, value: 1.0 },
        LinearOp::Select {
            dst: 10,
            cond: 9,
            if_true: 0,
            if_false: 1,
        },
    ];
    operations.extend((11..80).map(|dst| LinearOp::Const {
        dst,
        value: dst as f64,
    }));
    operations.push(LinearOp::StoreOutput { src: 10 });
    let initial = vec![0.0; 9];
    let programs = vec![(operations, vec![0, 1])];
    let final_values = run(&programs, &initial);
    assert_eq!(final_values[0].to_bits(), 0.0_f64.to_bits());
    assert_eq!(final_values[1].to_bits(), 9.0_f64.to_bits());
}
