//! Aggregate constructions and reductions, each the WASM form of its
//! `rumoca_eval_solve` typed definition: element order, operation order and
//! Integer overflow faults match the reference evaluator cell for cell.
use super::emit::{CELL, Emitter};
use super::layout::CellRange;
use super::numbers::{Operand, binary_supported};
use super::{TypedCallCompileError, TypedCallFaultKind};
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

impl Emitter<'_> {
    pub(super) fn aggregate_operation(
        &mut self,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        use solve::SolveOperation as O;
        match spanned.operation() {
            O::Cross {
                destination,
                lhs,
                rhs,
            } => self.cross(index, spanned, *destination, (*lhs, *rhs)),
            O::Reduce {
                destination,
                operator,
                operand,
            } => self.reduce(index, spanned, *destination, *operator, *operand),
            O::Diagonal {
                destination,
                operand,
            } => self.diagonal(index, spanned, *destination, *operand),
            O::Concatenate {
                destination,
                axis,
                operands,
            } => {
                self.concatenate(*destination, *axis, operands);
                Ok(())
            }
            O::SelectElement {
                destination,
                aggregate,
                indices,
                out_of_range,
            } => {
                self.select_element(*destination, *aggregate, indices, *out_of_range);
                Ok(())
            }
            _ => unreachable!("checked aggregate dispatch"),
        }
    }

    /// `lhs x rhs` over Real three-vectors (the IR constructs no other):
    /// component `k` is `lhs[a]*rhs[b] - lhs[c]*rhs[d]`.
    fn cross(
        &mut self,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
        destination: solve::SolveRegisterId,
        (lhs, rhs): (solve::SolveRegisterId, solve::SolveRegisterId),
    ) -> Result<(), TypedCallCompileError> {
        if !matches!(self.scalar(lhs), solve::SolveScalarType::Real { .. }) {
            return Err(self.unsupported(index, "cross", spanned.provenance()));
        }
        let (left, right, output) = (self.reg(lhs), self.reg(rhs), self.reg(destination));
        for (component, [a, b, c, d]) in [[1, 2, 2, 1], [2, 0, 0, 2], [0, 1, 1, 0]]
            .into_iter()
            .enumerate()
        {
            let cell = |range: CellRange, at: u32| CellRange {
                offset: range.offset + at * 8,
                bytes: 8,
                ..range
            };
            self.address(cell(output, component as u32));
            for (factor, other) in [(a, b), (c, d)] {
                self.address(cell(left, factor));
                self.push(I::F64Load(CELL));
                self.address(cell(right, other));
                self.push(I::F64Load(CELL));
                self.push(I::F64Mul);
            }
            self.push(I::F64Sub);
            self.push(I::F64Store(CELL));
        }
        Ok(())
    }

    /// The left fold of the reduction operator from the first element.
    fn reduce(
        &mut self,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
        destination: solve::SolveRegisterId,
        operator: solve::SolveReductionOperator,
        operand: solve::SolveRegisterId,
    ) -> Result<(), TypedCallCompileError> {
        use solve::SolveBinaryOperator as B;
        let scalar = self.scalar(operand);
        let boolean = scalar == solve::SolveScalarType::Boolean;
        // MLS §10.3.4 orders `false < true`: Boolean extrema are `and`/`or`.
        let binary = match operator {
            solve::SolveReductionOperator::Sum => B::Add,
            solve::SolveReductionOperator::Product => B::Multiply,
            solve::SolveReductionOperator::Minimum if boolean => B::And,
            solve::SolveReductionOperator::Maximum if boolean => B::Or,
            solve::SolveReductionOperator::Minimum => B::Min,
            solve::SolveReductionOperator::Maximum => B::Max,
            solve::SolveReductionOperator::All => B::And,
        };
        let source = self.reg(operand);
        if source.bytes == 0 || !binary_supported(binary, scalar) {
            return Err(self.unsupported(index, "reduce", spanned.provenance()));
        }
        let status = self.fault(
            Some(index),
            "reduce",
            TypedCallFaultKind::IntegerArithmetic,
            spanned.provenance(),
        );
        let real = matches!(scalar, solve::SolveScalarType::Real { .. });
        let output = self.reg(destination);
        self.copy(output, CellRange { bytes: 8, ..source });
        self.cells_from(1, source.bytes / 8, |e| {
            e.address(output);
            e.binary(
                binary,
                scalar,
                (Operand::Fixed(output), Operand::Cells(source)),
                status,
            );
            e.store(real);
        });
        Ok(())
    }

    /// A square matrix holding `operand` on its diagonal and zero elsewhere.
    fn diagonal(
        &mut self,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
        destination: solve::SolveRegisterId,
        operand: solve::SolveRegisterId,
    ) -> Result<(), TypedCallCompileError> {
        // Real zero and Integer zero share the all-zero cell bits.
        let zero_valid = match self.scalar(operand) {
            solve::SolveScalarType::Real { format, .. } => {
                format == solve::SolveRealFormat::Binary64
            }
            solve::SolveScalarType::Integer(domain) => domain.contains(0),
            solve::SolveScalarType::Boolean => false,
        };
        if !zero_valid {
            return Err(self.unsupported(index, "diagonal", spanned.provenance()));
        }
        let extent = self.program.register_types()[operand.index()].dimensions()[0];
        let (source, output) = (self.reg(operand), self.reg(destination));
        self.cells(output.bytes / 8, |e| {
            e.cell_address(output);
            e.address(source);
            e.push(I::LocalGet(3));
            e.push(I::I32Const(extent as i32));
            e.push(I::I32DivU);
            e.push(I::I32Const(8));
            e.push(I::I32Mul);
            e.push(I::I32Add);
            e.push(I::I64Load(CELL));
            e.push(I::I64Const(0));
            e.push(I::LocalGet(3));
            e.push(I::I32Const(extent as i32));
            e.push(I::I32DivU);
            e.push(I::LocalGet(3));
            e.push(I::I32Const(extent as i32));
            e.push(I::I32RemU);
            e.push(I::I32Eq);
            e.push(I::Select);
            e.push(I::I64Store(CELL));
        });
        Ok(())
    }

    /// Row-major concatenation along `axis`: for each index of the leading
    /// axes, each operand's block in operand order.
    fn concatenate(
        &mut self,
        destination: solve::SolveRegisterId,
        axis: u32,
        operands: &[solve::SolveRegisterId],
    ) {
        let types = self.program.register_types();
        let dimensions = types[destination.index()].dimensions();
        let axis = axis as usize;
        let inner: u32 = dimensions[axis + 1..].iter().product();
        let outer: u32 = dimensions[..axis].iter().product();
        // Each operand block with its byte width and its offset in a row.
        let mut row = 0;
        let mut blocks = Vec::with_capacity(operands.len());
        for operand in operands {
            let extent = types[operand.index()]
                .dimensions()
                .get(axis)
                .copied()
                .unwrap_or(1);
            let bytes = extent * inner * 8;
            blocks.push((self.reg(*operand), bytes, row));
            row += bytes;
        }
        blocks.retain(|(_, bytes, _)| *bytes > 0);
        let output = self.reg(destination);
        self.cells(outer, |e| {
            for (source, bytes, prefix) in &blocks {
                e.row_address(output, row, *prefix);
                e.row_address(*source, *bytes, 0);
                e.copy_addresses(*bytes);
            }
        });
    }

    /// The element at one-based runtime coordinates, or `out_of_range` when
    /// any coordinate lies outside its axis.
    fn select_element(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        indices: &[solve::SolveRegisterId],
        out_of_range: solve::SolveRegisterId,
    ) {
        let dimensions = self.program.register_types()[aggregate.index()]
            .dimensions()
            .to_vec();
        // Local 6 holds whether every coordinate is in range; local 4 the
        // row-major cell when it is.
        self.push(I::I64Const(1));
        self.push(I::LocalSet(6));
        self.push(I::I64Const(0));
        self.push(I::LocalSet(4));
        for (index, extent) in indices.iter().zip(dimensions) {
            self.address(self.reg(*index));
            self.push(I::I64Load(CELL));
            self.push(I::LocalSet(5));
            self.push(I::LocalGet(6));
            self.push(I::LocalGet(5));
            self.push(I::I64Const(1));
            self.push(I::I64GeS);
            self.push(I::LocalGet(5));
            self.push(I::I64Const(i64::from(extent)));
            self.push(I::I64LeS);
            self.push(I::I32And);
            self.push(I::I64ExtendI32U);
            self.push(I::I64And);
            self.push(I::LocalSet(6));
            self.push(I::LocalGet(4));
            self.push(I::I64Const(i64::from(extent)));
            self.push(I::I64Mul);
            self.push(I::LocalGet(5));
            self.push(I::I64Const(1));
            self.push(I::I64Sub);
            self.push(I::I64Add);
            self.push(I::LocalSet(4));
        }
        let (output, fallback) = (self.reg(destination), self.reg(out_of_range));
        self.push(I::LocalGet(6));
        self.push(I::I32WrapI64);
        self.push(I::If(BlockType::Empty));
        self.address(output);
        self.indexed_address(self.reg(aggregate));
        self.copy_addresses(8);
        self.push(I::Else);
        self.copy(output, fallback);
        self.push(I::End);
    }

    fn store(&mut self, real: bool) {
        self.push(if real {
            I::F64Store(CELL)
        } else {
            I::I64Store(CELL)
        });
    }

    /// `range` advanced by `row` bytes per cell index (local 3) plus `offset`.
    fn row_address(&mut self, range: CellRange, row: u32, offset: u32) {
        self.address(range);
        self.push(I::LocalGet(3));
        self.push(I::I32Const(row as i32));
        self.push(I::I32Mul);
        self.push(I::I32Add);
        if offset > 0 {
            self.push(I::I32Const(offset as i32));
            self.push(I::I32Add);
        }
    }
}
