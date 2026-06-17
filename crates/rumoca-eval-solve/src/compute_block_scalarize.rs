use std::convert::Infallible;

use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, ComputeNode, LinearOp, Reg, ScalarProgramBlock, SolveVisitor,
};

/// Expand tensor `ComputeBlock` nodes to flat scalar programs.
///
/// This is the scalar fallback for evaluators and backends that cannot consume
/// tensor-level Solve-IR nodes directly. It lives with Solve-IR evaluation, not
/// in the IR data crate or DAE-to-Solve lowering phase.
pub fn to_scalar_program_block(block: &ComputeBlock) -> ScalarProgramBlock {
    let mut collector = ScalarProgramCollector::default();
    let result: Result<(), Infallible> = collector.visit_compute_block(block);
    match result {
        Ok(()) => ScalarProgramBlock::with_program_spans(collector.rows, collector.program_spans),
        Err(error) => match error {},
    }
}

#[derive(Default)]
struct ScalarProgramCollector {
    rows: Vec<Vec<LinearOp>>,
    program_spans: Vec<rumoca_core::Span>,
}

impl SolveVisitor for ScalarProgramCollector {
    type Error = Infallible;

    fn visit_compute_node(
        &mut self,
        _node_index: usize,
        node: &ComputeNode,
    ) -> Result<(), Self::Error> {
        match node {
            ComputeNode::ScalarPrograms(block) => {
                self.visit_scalar_program_block(block)?;
            }
            ComputeNode::MatMul {
                lhs_ops,
                lhs_start,
                rhs_ops,
                rhs_start,
                m,
                k,
                n,
                span,
                ..
            } => {
                // One self-contained program: operands computed once, then one
                // `StoreOutput` per output element (running-counter invariant).
                let program =
                    scalarize_matmul(lhs_ops, *lhs_start, rhs_ops, *rhs_start, *m, *k, *n);
                self.rows.push(program);
                self.program_spans.push(*span);
            }
            ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                next_reg,
                span,
                ..
            } => {
                let program =
                    scalarize_linsolve(setup_ops, *matrix_start, *rhs_start, *n, *next_reg);
                self.rows.push(program);
                self.program_spans.push(*span);
            }
            ComputeNode::AffineStencil {
                count,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => {
                let scalar_programs =
                    scalarize_affine_stencil(base_ops, load_strides, const_strides, *count);
                self.program_spans
                    .extend(std::iter::repeat_n(*span, scalar_programs.len()));
                self.rows.extend(scalar_programs);
            }
        }
        Ok(())
    }

    fn visit_scalar_program(
        &mut self,
        _program_index: usize,
        span: Option<rumoca_core::Span>,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        self.rows.push(ops.to_vec());
        self.program_spans
            .push(span.unwrap_or(rumoca_core::Span::DUMMY));
        Ok(())
    }
}

pub(crate) fn scalarize_affine_stencil(
    base_ops: &[LinearOp],
    load_strides: &[rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &[rumoca_ir_solve::AffineStencilConstStride],
    count: usize,
) -> Vec<Vec<LinearOp>> {
    (0..count)
        .map(|row_offset| {
            let mut ops = base_ops.to_vec();
            for stride in load_strides {
                apply_affine_load_stride(&mut ops, stride, row_offset);
            }
            for stride in const_strides {
                apply_affine_const_stride(&mut ops, stride, row_offset);
            }
            ops
        })
        .collect()
}

fn apply_affine_load_stride(
    ops: &mut [LinearOp],
    stride: &rumoca_ir_solve::AffineStencilLoadStride,
    row_offset: usize,
) {
    match &mut ops[stride.op_position] {
        LinearOp::LoadY { index, .. } | LinearOp::LoadP { index, .. } => {
            *index += row_offset * stride.stride;
        }
        _ => unreachable!("AffineStencil load_strides only point at load ops"),
    }
}

fn apply_affine_const_stride(
    ops: &mut [LinearOp],
    stride: &rumoca_ir_solve::AffineStencilConstStride,
    row_offset: usize,
) {
    match &mut ops[stride.op_position] {
        LinearOp::Const { value, .. } => {
            *value += row_offset as f64 * stride.stride;
        }
        _ => unreachable!("AffineStencil const_strides only point at const ops"),
    }
}

/// Scalarize a `LinSolve` node into a SINGLE self-contained program.
///
/// `setup_ops` (which compute the matrix `A` and rhs `b` into the register file)
/// are emitted ONCE, followed by one `LinearSolveComponent` + `StoreOutput` per
/// solution component. Each component's destination register is allocated
/// monotonically so every `dst` is unique within the program — required so the
/// AD lowering's per-program `bind` never sees a duplicate destination.
fn scalarize_linsolve(
    setup_ops: &[LinearOp],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    next_reg: Reg,
) -> Vec<LinearOp> {
    let mut ops = setup_ops.to_vec();
    // `next_reg` is the producer's first-free register after setup; guard against
    // any mismatch by also clearing the registers actually present in `ops`.
    let next = next_reg.max(next_free_reg(&ops));
    for component in 0..n {
        let dst = next + component as Reg;
        ops.push(LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            component,
        });
        ops.push(LinearOp::StoreOutput { src: dst });
    }
    ops
}

/// Scalarize a `MatMul` node into a SINGLE self-contained program.
///
/// The operand op-lists (`lhs_ops`, `rhs_ops`) are emitted ONCE — this is the
/// fix for the multiplicative memory explosion, which previously copied them
/// into every one of the `m*n` output programs. The dot-product registers are
/// allocated from a monotonic cursor that never resets across outputs, so each
/// `dst` is unique within the program (required by the AD lowering). Outputs are
/// stored in row-major order via consecutive `StoreOutput` ops.
fn scalarize_matmul(
    lhs_ops: &[LinearOp],
    lhs_start: Reg,
    rhs_ops: &[LinearOp],
    rhs_start: Reg,
    m: usize,
    k: usize,
    n: usize,
) -> Vec<LinearOp> {
    let mut ops = lhs_ops.to_vec();
    ops.extend_from_slice(rhs_ops);
    let mut next = next_free_reg(&ops);

    if k == 0 {
        // All m*n outputs are zero; one shared zero register is enough since
        // `StoreOutput` only reads (it never `bind`s a destination).
        let zero = next;
        ops.push(LinearOp::Const {
            dst: zero,
            value: 0.0,
        });
        for _ in 0..(m * n) {
            ops.push(LinearOp::StoreOutput { src: zero });
        }
        return ops;
    }

    for row in 0..m {
        for col in 0..n {
            let first_lhs = lhs_start + (row * k) as Reg;
            let first_rhs = rhs_start + col as Reg;
            let mut acc = next;
            next += 1;
            ops.push(LinearOp::Binary {
                dst: acc,
                op: BinaryOp::Mul,
                lhs: first_lhs,
                rhs: first_rhs,
            });

            for ki in 1..k {
                let lhs = lhs_start + (row * k + ki) as Reg;
                let rhs = rhs_start + (ki * n + col) as Reg;
                let product = next;
                next += 1;
                ops.push(LinearOp::Binary {
                    dst: product,
                    op: BinaryOp::Mul,
                    lhs,
                    rhs,
                });
                let sum = next;
                next += 1;
                ops.push(LinearOp::Binary {
                    dst: sum,
                    op: BinaryOp::Add,
                    lhs: acc,
                    rhs: product,
                });
                acc = sum;
            }

            ops.push(LinearOp::StoreOutput { src: acc });
        }
    }
    ops
}

fn next_free_reg(ops: &[LinearOp]) -> Reg {
    ops.iter().map(max_reg_in_op).max().unwrap_or(0) + 1
}

fn max_reg_in_op(op: &LinearOp) -> Reg {
    match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::Move { dst, .. }
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::Select { dst, .. }
        | LinearOp::LinearSolveComponent { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. }
        | LinearOp::RandomInitialState { dst, .. }
        | LinearOp::RandomResult { dst, .. }
        | LinearOp::RandomState { dst, .. }
        | LinearOp::ImpureRandomInit { dst, .. }
        | LinearOp::ImpureRandom { dst, .. }
        | LinearOp::ImpureRandomInteger { dst, .. }
        | LinearOp::LoadIndexedP { dst, .. }
        | LinearOp::LoadIndexedSeed { dst, .. } => dst,
        LinearOp::StoreOutput { src } => src,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{
        AffineStencilDomain, AffineStencilIteration, AffineStencilLoadStride, SparsityPattern,
        TensorNodeMetadata, UnaryOp,
    };

    fn load_p_ops(start: Reg, count: usize) -> Vec<LinearOp> {
        (0..count)
            .map(|i| LinearOp::LoadP {
                dst: start + i as Reg,
                index: i,
            })
            .collect()
    }

    fn store_output_count(program: &[LinearOp]) -> usize {
        program
            .iter()
            .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
            .count()
    }

    fn test_stencil_domain(count: usize) -> AffineStencilDomain {
        AffineStencilDomain {
            index_names: vec!["i".to_string()],
            iterations: (0..count)
                .map(|idx| AffineStencilIteration {
                    index_values: vec![idx as i64 + 1],
                })
                .collect(),
        }
    }

    /// A matmul must scalarize to a SINGLE program whose operand ops appear once,
    /// not once-per-output. This is the fix for the multiplicative memory blowup.
    #[test]
    fn matmul_scalarizes_to_one_program_without_operand_duplication() {
        let (m, k, n) = (2usize, 2usize, 2usize);
        let lhs_ops = load_p_ops(0, m * k); // regs 0..4
        let rhs_ops = load_p_ops(4, k * n); // regs 4..8
        let block = ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops,
                lhs_start: 0,
                rhs_ops,
                rhs_start: 4,
                m,
                k,
                n,
                lhs_sparsity: SparsityPattern::Dense,
                rhs_sparsity: SparsityPattern::Dense,
                metadata: TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let scalar = to_scalar_program_block(&block);

        // One self-contained program, m*n outputs.
        assert_eq!(scalar.programs.len(), 1);
        assert_eq!(scalar.output_count(), m * n);
        assert_eq!(scalar.output_count(), block.len());
        assert_eq!(store_output_count(&scalar.programs[0]), m * n);

        // Operand loads appear exactly ONCE (8 = m*k + k*n), not m*n times.
        let load_p_count = scalar.programs[0]
            .iter()
            .filter(|op| matches!(op, LinearOp::LoadP { .. }))
            .count();
        assert_eq!(
            load_p_count,
            m * k + k * n,
            "operands must not be duplicated"
        );
    }

    /// Every computed destination register in the single program must be unique
    /// and monotonic across outputs, so the AD lowering's per-program `bind`
    /// never sees a duplicate destination.
    #[test]
    fn matmul_program_has_unique_monotonic_destination_registers() {
        let (m, k, n) = (3usize, 3usize, 3usize);
        let block = ComputeBlock {
            nodes: vec![ComputeNode::MatMul {
                lhs_ops: load_p_ops(0, m * k),
                lhs_start: 0,
                rhs_ops: load_p_ops((m * k) as Reg, k * n),
                rhs_start: (m * k) as Reg,
                m,
                k,
                n,
                lhs_sparsity: SparsityPattern::Dense,
                rhs_sparsity: SparsityPattern::Dense,
                metadata: TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let scalar = to_scalar_program_block(&block);
        let mut seen = std::collections::HashSet::new();
        for op in &scalar.programs[0] {
            if let LinearOp::Binary { dst, .. } = op {
                assert!(seen.insert(*dst), "duplicate destination register {dst}");
            }
        }
    }

    /// A LinSolve scalarizes to one program: setup ops once, then one solve
    /// component + StoreOutput per row, with unique destination registers.
    #[test]
    fn linsolve_scalarizes_to_one_program_with_unique_components() {
        let n = 3usize;
        // setup writes the n*n matrix then the n rhs entries: regs 0..(n*n+n)
        let setup_ops = load_p_ops(0, n * n + n);
        let next_reg = (n * n + n) as Reg;
        let block = ComputeBlock {
            nodes: vec![ComputeNode::LinSolve {
                setup_ops,
                matrix_start: 0,
                rhs_start: (n * n) as Reg,
                n,
                next_reg,
                metadata: TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let scalar = to_scalar_program_block(&block);
        assert_eq!(scalar.programs.len(), 1);
        assert_eq!(scalar.output_count(), n);
        assert_eq!(scalar.output_count(), block.len());

        let mut seen = std::collections::HashSet::new();
        let mut components = 0;
        for op in &scalar.programs[0] {
            if let LinearOp::LinearSolveComponent { dst, component, .. } = op {
                assert!(
                    seen.insert(*dst),
                    "duplicate solve-component register {dst}"
                );
                assert_eq!(*component, components);
                components += 1;
            }
        }
        assert_eq!(components, n);
    }

    #[test]
    fn affine_stencil_expands_to_exact_scalar_rows() {
        let block = ComputeBlock {
            nodes: vec![ComputeNode::AffineStencil {
                count: 3,
                domain: test_stencil_domain(3),
                base_ops: vec![
                    LinearOp::LoadP { dst: 0, index: 2 },
                    LinearOp::LoadY { dst: 1, index: 10 },
                    LinearOp::Unary {
                        dst: 2,
                        op: UnaryOp::Neg,
                        arg: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ],
                load_strides: vec![
                    AffineStencilLoadStride {
                        op_position: 0,
                        stride: 0,
                    },
                    AffineStencilLoadStride {
                        op_position: 1,
                        stride: 2,
                    },
                ],
                const_strides: Vec::new(),
                metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
                span: rumoca_core::Span::DUMMY,
            }],
        };

        let rows = to_scalar_program_block(&block);
        assert_eq!(rows.programs.len(), 3);
        assert!(matches!(
            rows.programs[0][1],
            LinearOp::LoadY { dst: 1, index: 10 }
        ));
        assert!(matches!(
            rows.programs[1][1],
            LinearOp::LoadY { dst: 1, index: 12 }
        ));
        assert!(matches!(
            rows.programs[2][1],
            LinearOp::LoadY { dst: 1, index: 14 }
        ));
        assert!(matches!(
            rows.programs[2][0],
            LinearOp::LoadP { dst: 0, index: 2 }
        ));
    }
}
