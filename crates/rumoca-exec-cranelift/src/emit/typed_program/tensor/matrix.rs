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
        // A fused transpose is read in place, stored transposed, by the kernel.
        let stored = |this: &Self, register: solve::SolveRegisterId| -> Result<_, CompileError> {
            match this.layout.fused.get(register.index()).cloned().flatten() {
                Some(solve::SolveOperation::Transpose { operand, .. }) => {
                    Ok(Some(this.register(operand)?.clone()))
                }
                Some(_) => Err(CompileError::Backend(
                    "fused product operand is not a transpose".into(),
                )),
                None => Ok(None),
            }
        };
        let stored_lhs = stored(self, lhs)?;
        let stored_rhs = stored(self, rhs)?;
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        let shape = matrix_extents(lhs.value_type.dimensions(), rhs.value_type.dimensions())?;
        if stored_lhs.is_some() || stored_rhs.is_some() {
            let flags = u64::from(stored_lhs.is_some()) | (u64::from(stored_rhs.is_some()) << 1);
            return self.lower_host_matrix_multiply_stored(
                &destination,
                stored_lhs.as_ref().unwrap_or(&lhs),
                stored_rhs.as_ref().unwrap_or(&rhs),
                shape,
                flags,
            );
        }
        let work = shape
            .0
            .checked_mul(shape.2)
            .and_then(|outputs| outputs.checked_mul(shape.1.max(1)));
        if work.is_some_and(|work| work <= crate::emit::MAX_STATIC_MATRIX_WORK) {
            self.lower_static_matrix_multiply(&destination, &lhs, &rhs, shape)
        } else if [&destination, &lhs, &rhs]
            .iter()
            .all(|location| is_binary64(location.value_type.element_type()))
        {
            self.lower_host_matrix_multiply(&destination, &lhs, &rhs, shape)
        } else {
            self.lower_loop_matrix_multiply(&destination, &lhs, &rhs, shape)
        }
    }

    /// Call the host kernel, which keeps the generated loop's per-output
    /// summation order exactly and so returns the same bits, but sweeps each
    /// output row contiguously so the inner loop vectorizes.
    fn lower_host_matrix_multiply(
        &mut self,
        destination: &ValueLocation,
        lhs: &ValueLocation,
        rhs: &ValueLocation,
        (rows, inner, columns): (usize, usize, usize),
    ) -> Result<(), CompileError> {
        use cranelift_codegen::ir::AbiParam;
        use cranelift_module::{Linkage, Module};
        let extent = |value: usize| {
            i64::try_from(value)
                .map_err(|_| CompileError::Backend("typed matrix extent exceeds i64".into()))
        };
        let (rows, inner, columns) = (extent(rows)?, extent(inner)?, extent(columns)?);
        let pointer = self.pointer_type;
        let mut signature = self.module.make_signature();
        signature.params.extend(
            [
                pointer,
                pointer,
                pointer,
                types::I64,
                types::I64,
                types::I64,
            ]
            .map(AbiParam::new),
        );
        let function = self
            .module
            .declare_function("rumoca_host_matmul_f64", Linkage::Import, &signature)
            .map_err(crate::emit::to_backend_err)?;
        let callee = crate::emit::owned_jit_module::declare_far_call_in_func(
            self.module,
            function,
            self.builder.func,
        );
        let zero = self.builder.ins().iconst(types::I64, 0);
        let address = |lowerer: &mut Self, location: &ValueLocation| {
            let base = lowerer.base(location);
            lowerer.cell_address(base, location.cell, zero)
        };
        let destination = address(self, destination);
        let lhs = address(self, lhs);
        let rhs = address(self, rhs);
        let rows = self.builder.ins().iconst(types::I64, rows);
        let inner = self.builder.ins().iconst(types::I64, inner);
        let columns = self.builder.ins().iconst(types::I64, columns);
        self.builder
            .ins()
            .call(callee, &[destination, lhs, rhs, rows, inner, columns]);
        Ok(())
    }

    /// The host kernel on operands of which some are stored transposed.
    fn lower_host_matrix_multiply_stored(
        &mut self,
        destination: &ValueLocation,
        lhs: &ValueLocation,
        rhs: &ValueLocation,
        (rows, inner, columns): (usize, usize, usize),
        transposed: u64,
    ) -> Result<(), CompileError> {
        use cranelift_codegen::ir::AbiParam;
        use cranelift_module::{Linkage, Module};
        let extent = |value: usize| {
            i64::try_from(value)
                .map_err(|_| CompileError::Backend("typed matrix extent exceeds i64".into()))
        };
        let (rows, inner, columns) = (extent(rows)?, extent(inner)?, extent(columns)?);
        let pointer = self.pointer_type;
        let mut signature = self.module.make_signature();
        signature.params.extend(
            [
                pointer,
                pointer,
                pointer,
                types::I64,
                types::I64,
                types::I64,
                types::I64,
            ]
            .map(AbiParam::new),
        );
        let function = self
            .module
            .declare_function("rumoca_host_matmul_t_f64", Linkage::Import, &signature)
            .map_err(crate::emit::to_backend_err)?;
        let callee = crate::emit::owned_jit_module::declare_far_call_in_func(
            self.module,
            function,
            self.builder.func,
        );
        let zero = self.builder.ins().iconst(types::I64, 0);
        let address = |lowerer: &mut Self, location: &ValueLocation| {
            let base = lowerer.base(location);
            lowerer.cell_address(base, location.cell, zero)
        };
        let destination = address(self, destination);
        let lhs = address(self, lhs);
        let rhs = address(self, rhs);
        let rows = self.builder.ins().iconst(types::I64, rows);
        let inner = self.builder.ins().iconst(types::I64, inner);
        let columns = self.builder.ins().iconst(types::I64, columns);
        let transposed = self.builder.ins().iconst(types::I64, transposed as i64);
        self.builder.ins().call(
            callee,
            &[destination, lhs, rhs, rows, inner, columns, transposed],
        );
        Ok(())
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
            // The first product is its own sum, as in the scalar references:
            // seeding with +0.0 would turn an all -0.0 dot into +0.0. An
            // empty dot has no product and is +0.0.
            let mut sum = None;
            for term in 0..inner {
                let left = (output / columns) * inner + term;
                let right = term * columns + output % columns;
                let left = self.builder.ins().iconst(types::I64, left as i64);
                let right = self.builder.ins().iconst(types::I64, right as i64);
                let left = self.load_scalar(lhs, left)?;
                let right = self.load_scalar(rhs, right)?;
                let product =
                    self.binary_element(solve::SolveBinaryOperator::Multiply, scalar, left, right)?;
                sum = Some(self.accumulate_term(sum, product, scalar)?);
            }
            let sum = match sum {
                Some(sum) => sum,
                None => zero_scalar(self, scalar),
            };
            let index = self.builder.ins().iconst(types::I64, output as i64);
            self.store_scalar(destination, index, sum)?;
        }
        Ok(())
    }

    /// `sum + product`, where a dot's first product is its own sum.
    fn accumulate_term(
        &mut self,
        sum: Option<Value>,
        product: Value,
        scalar: solve::SolveScalarType,
    ) -> Result<Value, CompileError> {
        match sum {
            Some(sum) => self.binary_element(solve::SolveBinaryOperator::Add, scalar, sum, product),
            None => Ok(product),
        }
    }

    /// The product `lhs[row, shared] * rhs[shared, column]` of one dot term.
    fn matrix_product_term(
        &mut self,
        (lhs, rhs): (&ValueLocation, &ValueLocation),
        (row, column, shared): (Value, Value, Value),
        (inner, columns): (i64, i64),
        scalar: solve::SolveScalarType,
    ) -> Result<Value, CompileError> {
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
        self.binary_element(
            solve::SolveBinaryOperator::Multiply,
            scalar,
            lhs_value,
            rhs_value,
        )
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
        // The first product is its own sum, as in the scalar references:
        // seeding with +0.0 would turn an all -0.0 dot into +0.0. An empty
        // dot has no product and is +0.0.
        let (first_term, first_value) = if inner == 0 {
            (zero_index, zero_scalar(self, scalar_type))
        } else {
            let first = self.matrix_product_term(
                (lhs, rhs),
                (row, column, zero_index),
                (inner, columns),
                scalar_type,
            )?;
            (self.builder.ins().iconst(types::I64, 1), first)
        };
        self.builder
            .ins()
            .jump(inner_header, &[first_term.into(), first_value.into()]);

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
        let product = self.matrix_product_term(
            (lhs, rhs),
            (row, column, shared),
            (inner, columns),
            scalar_type,
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

fn is_binary64(scalar: solve::SolveScalarType) -> bool {
    matches!(
        scalar,
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        }
    )
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
