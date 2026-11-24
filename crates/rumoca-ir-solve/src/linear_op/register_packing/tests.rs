use super::super::random_programs::{Stream, random_defined_program, run};
use super::*;

fn width(program: &[LinearOp]) -> usize {
    ScalarProgramRegisterFlow::derive(program)
        .expect("a generated program reads only written registers")
        .register_count()
}

#[test]
fn short_lived_values_share_registers() {
    // r0 = y0; r1 = -r0; r2 = y1; r3 = -r2; r4 = r1 + r3; store r4.
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::LoadY { dst: 2, index: 1 },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Neg,
            arg: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let packed = pack_registers(&program).expect("the program narrows");
    assert_eq!(width(&packed), 3);
    assert_eq!(run(&packed), run(&program));
}

#[test]
fn a_range_operand_keeps_its_registers_contiguous() {
    // Two scalars read as one range must stay adjacent.
    let program = vec![
        LinearOp::Const { dst: 5, value: 2.0 },
        LinearOp::Const { dst: 9, value: 3.0 },
        LinearOp::Move { dst: 10, src: 5 },
        LinearOp::Move { dst: 11, src: 9 },
        LinearOp::TensorBinary {
            dst_start: 20,
            op: BinaryOp::Mul,
            lhs_start: 10,
            rhs_start: 10,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 20,
            count: 2,
            stride: 1,
        },
    ];
    let packed = pack_registers(&program).expect("the program narrows");
    assert!(width(&packed) <= 6, "{packed:?}");
    assert_eq!(run(&packed), run(&program));
}

#[test]
fn a_dense_program_or_a_nested_one_is_kept() {
    let dense = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    assert!(pack_registers(&dense).is_none());
}

#[test]
fn every_packed_random_program_stores_the_same_outputs() {
    let mut stream = Stream(0x2545_f491_4f6c_dd1d);
    let (mut packed_count, mut kept) = (0usize, 0usize);
    let (mut before, mut after) = (0usize, 0usize);
    for _ in 0..3000 {
        let program = random_defined_program(&mut stream);
        let Some(packed) = pack_registers(&program) else {
            kept += 1;
            continue;
        };
        packed_count += 1;
        let (full, narrow) = (width(&program), width(&packed));
        assert!(narrow < full, "{program:?}");
        assert_eq!(run(&packed), run(&program), "{program:?}\n{packed:?}");
        before += full;
        after += narrow;
    }
    assert!(packed_count > kept, "packed {packed_count}, kept {kept}");
    assert!(4 * after < 3 * before, "registers {before} -> {after}");
}

#[test]
fn a_range_keeps_its_registers_after_its_last_use() {
    // A range of two registers dies before a scalar is defined; the scalar
    // does not reuse its registers, while two short-lived scalars share one.
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::TensorBinary {
            dst_start: 10,
            op: BinaryOp::Add,
            lhs_start: 0,
            rhs_start: 0,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 10,
            count: 2,
            stride: 1,
        },
        LinearOp::LoadY { dst: 20, index: 0 },
        LinearOp::StoreOutput { src: 20 },
        LinearOp::LoadY { dst: 21, index: 1 },
        LinearOp::StoreOutput { src: 21 },
    ];
    let packed = pack_registers(&program).expect("the program narrows");
    assert_eq!(run(&packed), run(&program));
    let range = |op: &LinearOp| match *op {
        LinearOp::TensorBinary {
            dst_start,
            lhs_start,
            ..
        } => (dst_start, lhs_start),
        _ => unreachable!(),
    };
    let (sum, operand) = range(&packed[2]);
    let scalar = |op: &LinearOp| match *op {
        LinearOp::LoadY { dst, .. } => dst,
        _ => unreachable!(),
    };
    let (late, later) = (scalar(&packed[4]), scalar(&packed[6]));
    assert_eq!(late, later, "two short-lived scalars share a register");
    for register in [late, later] {
        assert!(
            !(sum..sum + 2).contains(&register) && !(operand..operand + 2).contains(&register),
            "a scalar reuses a range register: {packed:?}"
        );
    }
}

#[test]
fn an_empty_range_declines() {
    // A zero-count range covers no register, so its start belongs to no
    // chunk: the program is outside what packing models.
    let program = vec![
        LinearOp::Const { dst: 5, value: 1.0 },
        LinearOp::TensorBinary {
            dst_start: 0,
            op: BinaryOp::Add,
            lhs_start: 5,
            rhs_start: 5,
            count: 0,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 5 },
    ];
    assert!(pack_registers(&program).is_none());
}

mod operation_kinds;
