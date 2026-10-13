use crate::{
    BinaryOp, LinearOp, TargetAssignmentShape, TensorConcatenateSource, TensorInputKind,
    derive_target_assignment_shapes,
};

fn load(dst_start: u32, input: TensorInputKind, input_start: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start,
        input,
        input_start,
        count: 3,
        seed_start: None,
        lanes: 1,
    }
}

fn binary(dst_start: u32, op: BinaryOp, lhs_start: u32, rhs_start: u32) -> LinearOp {
    LinearOp::TensorBinary {
        dst_start,
        op,
        lhs_start,
        rhs_start,
        count: 3,
        lhs_stride: 1,
        rhs_stride: 1,
        lanes: 1,
    }
}

fn store(start: u32) -> LinearOp {
    LinearOp::StoreOutputRange {
        start,
        count: 3,
        stride: 1,
    }
}

/// `0 - lane` per coordinate, reassembled by a concatenation: the scalar
/// residual lowering of `zeros(3) = <sum>`. The sum occupies registers
/// `sum..sum + 3`; the residual vector starts at register 40.
fn zero_minus_sum_residual(sum: u32) -> Vec<LinearOp> {
    let mut operations = vec![LinearOp::Const {
        dst: 30,
        value: 0.0,
    }];
    operations.extend((0..3).map(|lane| LinearOp::Binary {
        dst: 31 + lane,
        op: BinaryOp::Sub,
        lhs: 30,
        rhs: sum + lane,
    }));
    operations.push(LinearOp::TensorConcatenate {
        dst_start: 40,
        sources: (0..3)
            .map(|lane| TensorConcatenateSource {
                start: 31 + lane,
                dimensions: [1].into(),
            })
            .collect(),
        dimensions: [3].into(),
        axis: 0,
        lanes: 1,
    });
    operations.push(store(40));
    operations
}

fn shapes_of(program: &[LinearOp], output: usize) -> Vec<TargetAssignmentShape> {
    derive_target_assignment_shapes(program)
        .into_iter()
        .filter_map(|(offset, shape)| (offset == output).then_some(shape))
        .collect()
}

#[test]
fn zeros_equal_sum_isolates_the_one_unknown_array_coordinate() {
    let mut program = vec![
        load(0, TensorInputKind::Y, 0),
        load(3, TensorInputKind::P, 0),
        binary(6, BinaryOp::Add, 0, 3),
    ];
    program.extend(zero_minus_sum_residual(6));
    for lane in 0..3_u32 {
        let shapes = shapes_of(&program, lane as usize);
        let Some(TargetAssignmentShape::Additive {
            target_y_index,
            offset_terms,
            coefficient,
            ..
        }) = shapes.first()
        else {
            panic!("coordinate {lane} has no additive shape: {shapes:?}");
        };
        assert_eq!(*target_y_index, lane as usize);
        assert_eq!(*coefficient, -1.0);
        assert_eq!(&offset_terms[..], [(3 + lane, -1.0), (30, 1.0)]);
    }
}

#[test]
fn array_equal_zeros_isolates_every_coordinate() {
    let program = vec![
        load(0, TensorInputKind::Y, 0),
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::TensorFill {
            dst_start: 4,
            value_start: 3,
            count: 3,
            lanes: 1,
        },
        binary(7, BinaryOp::Sub, 0, 4),
        store(7),
    ];
    for lane in 0..3 {
        assert!(
            shapes_of(&program, lane)
                .iter()
                .any(|shape| shape.target_y_index() == lane),
            "coordinate {lane}"
        );
    }
}

#[test]
fn filled_constant_equal_difference_isolates_the_unknown_coordinate() {
    let program = vec![
        load(0, TensorInputKind::P, 0),
        LinearOp::TensorFill {
            dst_start: 3,
            value_start: 0,
            count: 3,
            lanes: 1,
        },
        load(6, TensorInputKind::Y, 0),
        load(9, TensorInputKind::P, 3),
        binary(12, BinaryOp::Sub, 6, 9),
        binary(15, BinaryOp::Sub, 3, 12),
        store(15),
    ];
    for lane in 0..3 {
        assert!(
            shapes_of(&program, lane)
                .iter()
                .any(|shape| shape.target_y_index() == lane),
            "coordinate {lane}"
        );
    }
}

#[test]
fn unknown_on_both_sides_stays_implicit() {
    let cancelling = {
        let mut program = vec![
            load(0, TensorInputKind::Y, 0),
            binary(6, BinaryOp::Sub, 0, 0),
        ];
        program.extend(zero_minus_sum_residual(6));
        program
    };
    let product = {
        let mut program = vec![
            load(0, TensorInputKind::Y, 0),
            binary(6, BinaryOp::Mul, 0, 0),
        ];
        program.extend(zero_minus_sum_residual(6));
        program
    };
    for program in [cancelling, product] {
        assert!(derive_target_assignment_shapes(&program).is_empty());
    }
}
