//! Final native emission for the checked aggregate matrix product.

use super::super::ValueLocation;
use super::{CompileError, ProgramLowerer, scalar_cranelift_type, zero_scalar};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, Value, types};
use rumoca_ir_solve as solve;

struct MatrixIndexRequest {
    row: Value,
    column: Value,
    shared: Value,
    inner: i64,
    columns: i64,
    lhs_vector: bool,
    rhs_vector: bool,
}

impl ProgramLowerer<'_, '_> {
    pub(in super::super) fn lower_matrix_multiply(
        &mut self,
        destination: solve::SolveRegisterId,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        let shape = matrix_extents(lhs.value_type.dimensions(), rhs.value_type.dimensions())?;
        let work = shape
            .0
            .checked_mul(shape.2)
            .and_then(|outputs| outputs.checked_mul(shape.1.max(1)));
        if work.is_some_and(|work| work <= crate::emit::MAX_STATIC_MATRIX_WORK) {
            self.lower_static_matrix_multiply(&destination, &lhs, &rhs, shape)
        } else {
            self.lower_loop_matrix_multiply(&destination, &lhs, &rhs, shape)
        }
    }

    fn lower_static_matrix_multiply(
        &mut self,
        destination: &ValueLocation,
        lhs: &ValueLocation,
        rhs: &ValueLocation,
        (rows, inner, columns): (usize, usize, usize),
    ) -> Result<(), CompileError> {
        let scalar = destination.value_type.element_type();
        for output in 0..rows * columns {
            let mut sum = zero_scalar(self, scalar);
            for term in 0..inner {
                let left = (output / columns) * inner + term;
                let right = term * columns + output % columns;
                let left = self.builder.ins().iconst(types::I64, left as i64);
                let right = self.builder.ins().iconst(types::I64, right as i64);
                let left = self.load_scalar(lhs, left)?;
                let right = self.load_scalar(rhs, right)?;
                let product =
                    self.binary_element(solve::SolveBinaryOperator::Multiply, scalar, left, right)?;
                sum = self.binary_element(solve::SolveBinaryOperator::Add, scalar, sum, product)?;
            }
            let index = self.builder.ins().iconst(types::I64, output as i64);
            self.store_scalar(destination, index, sum)?;
        }
        Ok(())
    }

    fn lower_loop_matrix_multiply(
        &mut self,
        destination: &ValueLocation,
        lhs: &ValueLocation,
        rhs: &ValueLocation,
        (rows, inner, columns): (usize, usize, usize),
    ) -> Result<(), CompileError> {
        let output_count = rows
            .checked_mul(columns)
            .ok_or_else(|| CompileError::Backend("typed matrix output size overflows".into()))?;
        let output_count = i64::try_from(output_count)
            .map_err(|_| CompileError::Backend("typed matrix output exceeds i64".into()))?;
        let inner = i64::try_from(inner)
            .map_err(|_| CompileError::Backend("typed matrix inner extent exceeds i64".into()))?;
        let columns = i64::try_from(columns)
            .map_err(|_| CompileError::Backend("typed matrix column extent exceeds i64".into()))?;

        let outer_header = self.builder.create_block();
        let outer_body = self.builder.create_block();
        let outer_exit = self.builder.create_block();
        self.builder.append_block_param(outer_header, types::I64);
        let zero_index = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(outer_header, &[zero_index.into()]);

        self.builder.switch_to_block(outer_header);
        let output = self.builder.block_params(outer_header)[0];
        let output_in_range =
            self.builder
                .ins()
                .icmp_imm(IntCC::UnsignedLessThan, output, output_count);
        self.builder
            .ins()
            .brif(output_in_range, outer_body, &[], outer_exit, &[]);

        self.builder.switch_to_block(outer_body);
        self.builder.seal_block(outer_body);
        let row = self.builder.ins().udiv_imm(output, columns);
        let column = self.builder.ins().urem_imm(output, columns);
        let inner_header = self.builder.create_block();
        let inner_body = self.builder.create_block();
        let inner_exit = self.builder.create_block();
        let scalar_type = destination.value_type.element_type();
        self.builder.append_block_param(inner_header, types::I64);
        self.builder
            .append_block_param(inner_header, scalar_cranelift_type(scalar_type));
        self.builder
            .append_block_param(inner_exit, scalar_cranelift_type(scalar_type));
        let zero_value = zero_scalar(self, scalar_type);
        self.builder
            .ins()
            .jump(inner_header, &[zero_index.into(), zero_value.into()]);

        self.builder.switch_to_block(inner_header);
        let shared = self.builder.block_params(inner_header)[0];
        let accumulator = self.builder.block_params(inner_header)[1];
        let term_in_range = self
            .builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, shared, inner);
        self.builder.ins().brif(
            term_in_range,
            inner_body,
            &[],
            inner_exit,
            &[accumulator.into()],
        );

        self.builder.switch_to_block(inner_body);
        self.builder.seal_block(inner_body);
        let (lhs_index, rhs_index) = self.matrix_multiply_indices(MatrixIndexRequest {
            row,
            column,
            shared,
            inner,
            columns,
            lhs_vector: lhs.value_type.dimensions().len() == 1,
            rhs_vector: rhs.value_type.dimensions().len() == 1,
        });
        let lhs_value = self.load_scalar(lhs, lhs_index)?;
        let rhs_value = self.load_scalar(rhs, rhs_index)?;
        let product = self.binary_element(
            solve::SolveBinaryOperator::Multiply,
            scalar_type,
            lhs_value,
            rhs_value,
        )?;
        let next_accumulator = self.binary_element(
            solve::SolveBinaryOperator::Add,
            scalar_type,
            accumulator,
            product,
        )?;
        let next_shared = self.builder.ins().iadd_imm(shared, 1);
        self.builder
            .ins()
            .jump(inner_header, &[next_shared.into(), next_accumulator.into()]);
        self.builder.seal_block(inner_header);

        self.builder.switch_to_block(inner_exit);
        self.builder.seal_block(inner_exit);
        let result = self.builder.block_params(inner_exit)[0];
        self.store_scalar(destination, output, result)?;
        let next_output = self.builder.ins().iadd_imm(output, 1);
        self.builder.ins().jump(outer_header, &[next_output.into()]);
        self.builder.seal_block(outer_header);

        self.builder.switch_to_block(outer_exit);
        self.builder.seal_block(outer_exit);
        Ok(())
    }

    fn matrix_multiply_indices(&mut self, request: MatrixIndexRequest) -> (Value, Value) {
        let lhs = if request.lhs_vector {
            request.shared
        } else {
            let start = self.builder.ins().imul_imm(request.row, request.inner);
            self.builder.ins().iadd(start, request.shared)
        };
        let rhs = if request.rhs_vector {
            request.shared
        } else {
            let start = self.builder.ins().imul_imm(request.shared, request.columns);
            self.builder.ins().iadd(start, request.column)
        };
        (lhs, rhs)
    }
}

fn matrix_extents(lhs: &[u32], rhs: &[u32]) -> Result<(usize, usize, usize), CompileError> {
    match (lhs, rhs) {
        ([inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => Ok((1, *inner_lhs as usize, 1)),
        ([rows, inner_lhs], [inner_rhs]) if inner_lhs == inner_rhs => {
            Ok((*rows as usize, *inner_lhs as usize, 1))
        }
        ([inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => {
            Ok((1, *inner_lhs as usize, *columns as usize))
        }
        ([rows, inner_lhs], [inner_rhs, columns]) if inner_lhs == inner_rhs => {
            Ok((*rows as usize, *inner_lhs as usize, *columns as usize))
        }
        _ => Err(CompileError::Backend(
            "checked typed matrix extents are inconsistent".into(),
        )),
    }
}
