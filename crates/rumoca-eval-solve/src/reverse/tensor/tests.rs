//! Each tensor reverse rule against central finite differences of the canonical
//! evaluator, plus the tape's primal against the canonical primal bit for bit.

use super::super::{ReverseScratch, ScalarVjpProgram, forward_row_tape, reverse_scalar_block_vjp};
use super::*;
use crate::RowEvalContext;
use rumoca_ir_solve::{BinaryOp, ScalarProgramBlock, UnaryOp};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn block(row: Vec<LinearOp>) -> ScalarProgramBlock {
    ScalarProgramBlock::with_output_indices(vec![row], vec![span()], vec![0]).expect("valid block")
}

fn canonical(block: &ScalarProgramBlock, y: &[f64], p: &[f64]) -> f64 {
    let mut out = [0.0];
    crate::eval_scalar_program_block(block, y, p, 0.0, None, &mut out).expect("canonical eval");
    out[0]
}

fn reverse_gradient(block: &ScalarProgramBlock, y: &[f64], p: &[f64]) -> (Vec<f64>, Vec<f64>) {
    let row_registers: Vec<usize> = block
        .programs()
        .iter()
        .map(|row| crate::required_registers(row).expect("register count"))
        .collect();
    let requirements = crate::scalar_program_block_input_requirements(block).expect("requirements");
    let (mut gy, mut gp) = (vec![0.0; y.len()], vec![0.0; p.len()]);
    reverse_scalar_block_vjp(
        &ScalarVjpProgram {
            block,
            row_registers: &row_registers,
            requirements,
        },
        &ReverseInputs {
            y,
            p,
            t: 0.0,
            context: RowEvalContext::default(),
        },
        &[1.0],
        &mut ReverseCotangents {
            y: &mut gy,
            p: &mut gp,
            seed: &mut [],
        },
        &mut ReverseScratch::default(),
    )
    .expect("reverse sweep");
    (gy, gp)
}

fn central_difference(f: impl Fn(&[f64]) -> f64, x: &[f64]) -> Vec<f64> {
    (0..x.len())
        .map(|i| {
            let h = 1e-6 * x[i].abs().max(1.0);
            let (mut up, mut down) = (x.to_vec(), x.to_vec());
            up[i] += h;
            down[i] -= h;
            (f(&up) - f(&down)) / (2.0 * h)
        })
        .collect()
}

fn assert_close(label: &str, reverse: &[f64], fd: &[f64]) {
    for (index, (r, f)) in reverse.iter().zip(fd).enumerate() {
        let tolerance = 1e-6 * f.abs().max(1.0);
        assert!(
            (r - f).abs() <= tolerance,
            "{label}[{index}]: reverse {r}, finite difference {f}"
        );
    }
}

/// Registers: A = y[0..6] as 2x3, B = p[0..6] as 3x2, v = y[6..9].
/// f = Σ(((Cᵀ .* C) ./ fill(0.5)) .- 0.5 .+ I)² + v·A[0,:] + v[0:2]·A[:,1] + tanh(C[0,0]),
/// with C = A·B, so every supported rule (both load spaces, a matrix product, a
/// transpose, a fill, a scalar broadcast, an identity, three dot products
/// including a strided and a self-product) sits on the path to the output.
fn every_tensor_rule() -> ScalarProgramBlock {
    let mut row = loads_products_and_fill();
    row.extend(elementwise_reductions_and_output());
    block(row)
}

/// `A`, `B`, `C = A·B`, `Cᵀ`, `v`, and `fill(0.5)`.
fn loads_products_and_fill() -> Vec<LinearOp> {
    use LinearOp::*;
    vec![
        TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 6,
            seed_start: None,
            lanes: 1,
        },
        TensorLoad {
            dst_start: 6,
            input: TensorInputKind::P,
            input_start: 0,
            count: 6,
            seed_start: None,
            lanes: 1,
        },
        MatrixMultiply {
            dst_start: 12,
            lhs_start: 0,
            rhs_start: 6,
            rows: 2,
            inner: 3,
            columns: 2,
            lanes: 1,
        },
        TensorTranspose {
            dst_start: 16,
            src_start: 12,
            rows: 2,
            columns: 2,
            element_width: 1,
            lanes: 1,
        },
        TensorLoad {
            dst_start: 20,
            input: TensorInputKind::Y,
            input_start: 6,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        Const {
            dst: 23,
            value: 0.5,
        },
        TensorFill {
            dst_start: 24,
            value_start: 23,
            count: 4,
            lanes: 1,
        },
    ]
}

/// The elementwise chain, the three dot products, and the stored sum.
fn elementwise_reductions_and_output() -> Vec<LinearOp> {
    use LinearOp::*;
    vec![
        TensorBinary {
            dst_start: 28,
            op: BinaryOp::Mul,
            lhs_start: 16,
            rhs_start: 12,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        TensorBinary {
            dst_start: 32,
            op: BinaryOp::Div,
            lhs_start: 28,
            rhs_start: 24,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        TensorBinary {
            dst_start: 36,
            op: BinaryOp::Sub,
            lhs_start: 32,
            rhs_start: 23,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 1,
        },
        TensorIdentity {
            dst_start: 40,
            size: 2,
            lanes: 1,
        },
        TensorBinary {
            dst_start: 44,
            op: BinaryOp::Add,
            lhs_start: 36,
            rhs_start: 40,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        DotProduct {
            dst: 48,
            lhs_start: 44,
            rhs_start: 44,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 1,
        },
        DotProduct {
            dst: 49,
            lhs_start: 20,
            rhs_start: 0,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
        },
        DotProduct {
            dst: 50,
            lhs_start: 20,
            rhs_start: 1,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 3,
        },
        Unary {
            dst: 51,
            op: UnaryOp::Tanh,
            arg: 12,
        },
        Binary {
            dst: 52,
            op: BinaryOp::Add,
            lhs: 48,
            rhs: 49,
        },
        Binary {
            dst: 53,
            op: BinaryOp::Add,
            lhs: 52,
            rhs: 50,
        },
        Binary {
            dst: 54,
            op: BinaryOp::Add,
            lhs: 53,
            rhs: 51,
        },
        StoreOutput { src: 54 },
    ]
}

const Y: [f64; 9] = [0.3, -0.7, 1.1, 0.4, 0.9, -0.2, 0.6, -1.3, 0.8];
const P: [f64; 6] = [0.5, -0.1, 0.2, 0.7, -0.4, 0.3];

#[test]
fn every_tensor_rule_matches_central_differences() {
    let block = every_tensor_rule();
    let (gy, gp) = reverse_gradient(&block, &Y, &P);
    let fd_y = central_difference(|y| canonical(&block, y, &P), &Y);
    let fd_p = central_difference(|p| canonical(&block, &Y, p), &P);
    assert_close("df/dy", &gy, &fd_y);
    assert_close("df/dp", &gp, &fd_p);
}

#[test]
fn the_tape_records_the_canonical_primal_bit_for_bit() {
    let block = every_tensor_rule();
    let row = &block.programs()[0];
    let mut regs = vec![0.0; crate::required_registers(row).expect("register count")];
    forward_row_tape(
        row,
        &ReverseInputs {
            y: &Y,
            p: &P,
            t: 0.0,
            context: RowEvalContext::default(),
        },
        &mut regs,
    )
    .expect("tape");
    assert_eq!(regs[54].to_bits(), canonical(&block, &Y, &P).to_bits());
}

/// The adjoint of an unselected arm that divides by a zero element must add
/// nothing, element by element, exactly as the scalar `Binary` rule does.
#[test]
fn an_unselected_tensor_quotient_leaves_the_gradient_finite() {
    use LinearOp::*;
    let block = block(vec![
        TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        Const { dst: 2, value: 1.0 },
        TensorBinary {
            dst_start: 3,
            op: BinaryOp::Div,
            lhs_start: 2,
            rhs_start: 0,
            count: 2,
            lhs_stride: 0,
            rhs_stride: 1,
            lanes: 1,
        },
        DotProduct {
            dst: 5,
            lhs_start: 3,
            rhs_start: 0,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
        },
        Const { dst: 6, value: 1.0 },
        Binary {
            dst: 7,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        Select {
            dst: 8,
            cond: 6,
            if_true: 7,
            if_false: 5,
        },
        StoreOutput { src: 8 },
    ]);
    let (gy, _) = reverse_gradient(&block, &[3.0, 0.0], &[]);
    assert_eq!(gy, vec![6.0, 0.0]);
}

#[test]
fn dual_lanes_and_seeded_loads_stay_refused() {
    assert!(!supported(&LinearOp::MatrixMultiply {
        dst_start: 0,
        lhs_start: 0,
        rhs_start: 0,
        rows: 1,
        inner: 1,
        columns: 1,
        lanes: 2
    }));
    assert!(!supported(&LinearOp::TensorLoad {
        dst_start: 0,
        input: TensorInputKind::Y,
        input_start: 0,
        count: 1,
        seed_start: Some(0),
        lanes: 1
    }));
}

/// A strided output range seeds each stored register with its own output
/// cotangent: `f = w·(x .* x)[odd outputs]` checked against differences of the
/// same weighted sum of the canonical outputs.
#[test]
fn a_strided_output_range_seeds_each_stored_register() {
    use LinearOp::*;
    let block = ScalarProgramBlock::with_output_indices(
        vec![vec![
            TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            TensorBinary {
                dst_start: 4,
                op: BinaryOp::Mul,
                lhs_start: 0,
                rhs_start: 0,
                count: 4,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            StoreOutputRange {
                start: 5,
                count: 2,
                stride: 2,
            },
        ]],
        vec![span()],
        vec![0, 1],
    )
    .expect("valid block");
    let weights = [2.0, -3.0];
    let y = [0.4, 1.5, -0.9, 0.7];
    let row_registers = vec![crate::required_registers(&block.programs()[0]).expect("registers")];
    let requirements =
        crate::scalar_program_block_input_requirements(&block).expect("requirements");
    let mut gy = [0.0; 4];
    reverse_scalar_block_vjp(
        &ScalarVjpProgram {
            block: &block,
            row_registers: &row_registers,
            requirements,
        },
        &ReverseInputs {
            y: &y,
            p: &[],
            t: 0.0,
            context: RowEvalContext::default(),
        },
        &weights,
        &mut ReverseCotangents {
            y: &mut gy,
            p: &mut [],
            seed: &mut [],
        },
        &mut ReverseScratch::default(),
    )
    .expect("reverse sweep");
    let weighted = |y: &[f64]| {
        let mut out = [0.0; 2];
        crate::eval_scalar_program_block(&block, y, &[], 0.0, None, &mut out).expect("eval");
        weights[0] * out[0] + weights[1] * out[1]
    };
    assert_close("d(w·out)/dy", &gy, &central_difference(weighted, &y));
}

/// `q = 1 ./ v` with `v[1] = 0`, then `t = transpose(q)` and `m = q * I`, and the
/// output selects `x` over `t[1]` and over `m[1]`: the infinite elements sit on
/// unselected arms, so their marks must pass element by element through the
/// transpose and the product, leaving `dv` finite (zero), as the scalar form does.
#[test]
fn unselected_elements_pass_their_marks_through_transpose_and_matmul() {
    use LinearOp::*;
    let block = block(vec![
        TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        Const { dst: 2, value: 1.0 },
        TensorBinary {
            dst_start: 3,
            op: BinaryOp::Div,
            lhs_start: 2,
            rhs_start: 0,
            count: 2,
            lhs_stride: 0,
            rhs_stride: 1,
            lanes: 1,
        },
        TensorTranspose {
            dst_start: 5,
            src_start: 3,
            rows: 2,
            columns: 1,
            element_width: 1,
            lanes: 1,
        },
        TensorIdentity {
            dst_start: 7,
            size: 2,
            lanes: 1,
        },
        MatrixMultiply {
            dst_start: 11,
            lhs_start: 3,
            rhs_start: 7,
            rows: 1,
            inner: 2,
            columns: 2,
            lanes: 1,
        },
        Const {
            dst: 13,
            value: 1.0,
        },
        LoadY { dst: 14, index: 2 },
        Select {
            dst: 15,
            cond: 13,
            if_true: 14,
            if_false: 6,
        },
        Select {
            dst: 16,
            cond: 13,
            if_true: 15,
            if_false: 12,
        },
        Binary {
            dst: 17,
            op: BinaryOp::Mul,
            lhs: 16,
            rhs: 14,
        },
        StoreOutput { src: 17 },
    ]);
    let (gy, _) = reverse_gradient(&block, &[3.0, 0.0, 5.0], &[]);
    assert_eq!(gy, vec![0.0, 0.0, 10.0]);
}

#[test]
fn a_destination_that_overlaps_a_source_is_refused() {
    use LinearOp::*;
    let block = block(vec![
        TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 4,
            seed_start: None,
            lanes: 1,
        },
        TensorTranspose {
            dst_start: 2,
            src_start: 0,
            rows: 2,
            columns: 2,
            element_width: 1,
            lanes: 1,
        },
        DotProduct {
            dst: 6,
            lhs_start: 2,
            rhs_start: 2,
            count: 4,
            lhs_stride: 1,
            rhs_stride: 1,
        },
        StoreOutput { src: 6 },
    ]);
    let row_registers = vec![crate::required_registers(&block.programs()[0]).expect("registers")];
    let requirements =
        crate::scalar_program_block_input_requirements(&block).expect("requirements");
    let mut gy = [0.0; 4];
    let error = reverse_scalar_block_vjp(
        &ScalarVjpProgram {
            block: &block,
            row_registers: &row_registers,
            requirements,
        },
        &ReverseInputs {
            y: &[1.0, 2.0, 3.0, 4.0],
            p: &[],
            t: 0.0,
            context: RowEvalContext::default(),
        },
        &[1.0],
        &mut ReverseCotangents {
            y: &mut gy,
            p: &mut [],
            seed: &mut [],
        },
        &mut ReverseScratch::default(),
    )
    .expect_err("an overlapping transpose is refused");
    assert!(
        error.to_string().contains("overlaps a source range"),
        "{error}"
    );
}
