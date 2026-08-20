use super::{CompileError, ProgramLowerer, scalar_cranelift_type};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, TrapCode, Value, types};
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
    pub(super) fn lower_project_element(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        indices: &[u32],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let offset = static_row_major_offset(aggregate.value_type.dimensions(), indices)?;
        let source = self.builder.ins().iconst(types::I64, i64::from(offset));
        let zero = self.builder.ins().iconst(types::I64, 0);
        let value = self.load_scalar(&aggregate, source)?;
        self.store_scalar(&destination, zero, value)
    }

    pub(super) fn lower_project_element_dynamic(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        indices: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let (offset, valid) =
            self.dynamic_row_major_offset(aggregate.value_type.dimensions(), indices)?;
        self.builder
            .ins()
            .trapz(valid, TrapCode::HEAP_OUT_OF_BOUNDS);
        let value = self.load_scalar(&aggregate, offset)?;
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.store_scalar(&destination, zero, value)
    }

    pub(super) fn lower_project_slice(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        origin: &[u32],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let destination_dimensions = destination.value_type.dimensions().to_vec();
        let aggregate_dimensions = aggregate.value_type.dimensions().to_vec();
        self.for_each_element(destination.value_type.scalar_count(), |this, output| {
            let source = remap_offset(
                this,
                output,
                &destination_dimensions,
                &aggregate_dimensions,
                origin,
            )?;
            let value = this.load_scalar(&aggregate, source)?;
            this.store_scalar(&destination, output, value)
        })
    }

    pub(super) fn lower_project_view(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        axes: &[solve::SolveTensorViewAxis],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let destination_dimensions = destination.value_type.dimensions().to_vec();
        let aggregate_dimensions = aggregate.value_type.dimensions().to_vec();
        self.for_each_element(destination.value_type.scalar_count(), |this, output| {
            let (source, valid) = this.tensor_view_offset(
                output,
                &destination_dimensions,
                &aggregate_dimensions,
                axes,
            )?;
            this.builder
                .ins()
                .trapz(valid, TrapCode::HEAP_OUT_OF_BOUNDS);
            let value = this.load_scalar(&aggregate, source)?;
            this.store_scalar(&destination, output, value)
        })
    }

    pub(super) fn lower_select_element(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        indices: &[solve::SolveRegisterId],
        out_of_range: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let fallback = self.register(out_of_range)?.clone();
        let (offset, valid) =
            self.dynamic_row_major_offset(aggregate.value_type.dimensions(), indices)?;
        let zero = self.builder.ins().iconst(types::I64, 0);
        let safe_offset = self.builder.ins().select(valid, offset, zero);
        let selected = self.load_scalar(&aggregate, safe_offset)?;
        let fallback = self.load_scalar(&fallback, zero)?;
        let value = self.builder.ins().select(valid, selected, fallback);
        self.store_scalar(&destination, zero, value)
    }

    pub(super) fn lower_update_element(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        value: solve::SolveRegisterId,
        indices: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let value = self.register(value)?.clone();
        self.copy(&aggregate, &destination)?;
        let (offset, valid) =
            self.dynamic_row_major_offset(aggregate.value_type.dimensions(), indices)?;
        self.builder
            .ins()
            .trapz(valid, TrapCode::HEAP_OUT_OF_BOUNDS);
        let zero = self.builder.ins().iconst(types::I64, 0);
        let value = self.load_scalar(&value, zero)?;
        self.store_scalar(&destination, offset, value)
    }

    pub(super) fn lower_update_slice(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        value: solve::SolveRegisterId,
        origin: &[u32],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let value = self.register(value)?.clone();
        self.copy(&aggregate, &destination)?;
        let value_dimensions = value.value_type.dimensions().to_vec();
        let destination_dimensions = destination.value_type.dimensions().to_vec();
        self.for_each_element(value.value_type.scalar_count(), |this, source| {
            let target = remap_offset(
                this,
                source,
                &value_dimensions,
                &destination_dimensions,
                origin,
            )?;
            let element = this.load_scalar(&value, source)?;
            this.store_scalar(&destination, target, element)
        })
    }

    pub(super) fn lower_update_view(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        value: solve::SolveRegisterId,
        axes: &[solve::SolveTensorViewAxis],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let value = self.register(value)?.clone();
        self.copy(&aggregate, &destination)?;
        let value_dimensions = value.value_type.dimensions().to_vec();
        let destination_dimensions = destination.value_type.dimensions().to_vec();
        self.for_each_element(value.value_type.scalar_count(), |this, source| {
            let (target, valid) =
                this.tensor_view_offset(source, &value_dimensions, &destination_dimensions, axes)?;
            this.builder
                .ins()
                .trapz(valid, TrapCode::HEAP_OUT_OF_BOUNDS);
            let element = this.load_scalar(&value, source)?;
            this.store_scalar(&destination, target, element)
        })
    }

    fn dynamic_row_major_offset(
        &mut self,
        dimensions: &[u32],
        indices: &[solve::SolveRegisterId],
    ) -> Result<(Value, Value), CompileError> {
        if dimensions.len() != indices.len() {
            return Err(CompileError::Backend(
                "checked dynamic projection rank differs".into(),
            ));
        }
        let mut offset = self.builder.ins().iconst(types::I64, 0);
        let mut valid = self.builder.ins().iconst(types::I8, 1);
        for (&extent, index) in dimensions.iter().zip(indices) {
            let location = self.register(*index)?.clone();
            let zero = self.builder.ins().iconst(types::I64, 0);
            let coordinate = self.load_scalar(&location, zero)?;
            let coordinate = self.builder.ins().iadd_imm(coordinate, -1);
            let nonnegative =
                self.builder
                    .ins()
                    .icmp_imm(IntCC::SignedGreaterThanOrEqual, coordinate, 0);
            let below_extent =
                self.builder
                    .ins()
                    .icmp_imm(IntCC::SignedLessThan, coordinate, i64::from(extent));
            valid = self.builder.ins().band(valid, nonnegative);
            valid = self.builder.ins().band(valid, below_extent);
            offset = self.builder.ins().imul_imm(offset, i64::from(extent));
            offset = self.builder.ins().iadd(offset, coordinate);
        }
        Ok((offset, valid))
    }

    fn tensor_view_offset(
        &mut self,
        output: Value,
        output_dimensions: &[u32],
        aggregate_dimensions: &[u32],
        axes: &[solve::SolveTensorViewAxis],
    ) -> Result<(Value, Value), CompileError> {
        if axes.len() != aggregate_dimensions.len() {
            return Err(CompileError::Backend(
                "checked tensor view rank differs".into(),
            ));
        }
        let mut source = self.builder.ins().iconst(types::I64, 0);
        let mut valid = self.builder.ins().iconst(types::I8, 1);
        let mut retained_axis = 0usize;
        for (axis, &aggregate_extent) in axes.iter().zip(aggregate_dimensions) {
            let coordinate = match axis {
                solve::SolveTensorViewAxis::Index(register) => {
                    let location = self.register(*register)?.clone();
                    let zero = self.builder.ins().iconst(types::I64, 0);
                    let coordinate = self.load_scalar(&location, zero)?;
                    let coordinate = self.builder.ins().iadd_imm(coordinate, -1);
                    let nonnegative =
                        self.builder
                            .ins()
                            .icmp_imm(IntCC::SignedGreaterThanOrEqual, coordinate, 0);
                    let below_extent = self.builder.ins().icmp_imm(
                        IntCC::SignedLessThan,
                        coordinate,
                        i64::from(aggregate_extent),
                    );
                    valid = self.builder.ins().band(valid, nonnegative);
                    valid = self.builder.ins().band(valid, below_extent);
                    coordinate
                }
                solve::SolveTensorViewAxis::Span { origin, .. } => {
                    let coordinate = coordinate_at(self, output, output_dimensions, retained_axis)?;
                    retained_axis += 1;
                    self.builder.ins().iadd_imm(coordinate, i64::from(*origin))
                }
            };
            source = self
                .builder
                .ins()
                .imul_imm(source, i64::from(aggregate_extent));
            source = self.builder.ins().iadd(source, coordinate);
        }
        Ok((source, valid))
    }

    pub(super) fn lower_fill(
        &mut self,
        destination: solve::SolveRegisterId,
        value: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let value = self.register(value)?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let value = self.load_scalar(&value, zero)?;
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            this.store_scalar(&destination, index, value)
        })
    }

    pub(super) fn lower_construct_aggregate(
        &mut self,
        destination: solve::SolveRegisterId,
        elements: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let mut prefix = 0u32;
        for element in elements {
            let element = self.register(*element)?.clone();
            let element_prefix = prefix;
            self.for_each_element(element.value_type.scalar_count(), |this, index| {
                let value = this.load_scalar(&element, index)?;
                let destination_index = this
                    .builder
                    .ins()
                    .iadd_imm(index, i64::from(element_prefix));
                this.store_scalar(&destination, destination_index, value)
            })?;
            prefix = prefix
                .checked_add(element.value_type.scalar_count())
                .ok_or_else(|| {
                    CompileError::Backend("typed aggregate construction overflows".into())
                })?;
        }
        if prefix != destination.value_type.scalar_count() {
            return Err(CompileError::Backend(
                "checked typed aggregate construction width differs".into(),
            ));
        }
        Ok(())
    }

    pub(super) fn lower_scale(
        &mut self,
        destination: solve::SolveRegisterId,
        aggregate: solve::SolveRegisterId,
        scalar: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        self.lower_broadcast_binary(
            destination,
            solve::SolveBinaryOperator::Multiply,
            aggregate,
            scalar,
            false,
        )
    }

    pub(super) fn lower_broadcast_binary(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveBinaryOperator,
        aggregate: solve::SolveRegisterId,
        scalar: solve::SolveRegisterId,
        scalar_on_lhs: bool,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let aggregate = self.register(aggregate)?.clone();
        let scalar = self.register(scalar)?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let scalar_value = self.load_scalar(&scalar, zero)?;
        let scalar_type = destination.value_type.element_type();
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let aggregate_value = this.load_scalar(&aggregate, index)?;
            let (lhs, rhs) = if scalar_on_lhs {
                (scalar_value, aggregate_value)
            } else {
                (aggregate_value, scalar_value)
            };
            let value = this.binary_element(operator, scalar_type, lhs, rhs)?;
            this.store_scalar(&destination, index, value)
        })
    }

    pub(super) fn lower_transpose(
        &mut self,
        destination: solve::SolveRegisterId,
        operand: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let operand = self.register(operand)?.clone();
        let [rows, columns, trailing @ ..] = operand.value_type.dimensions() else {
            return Err(CompileError::Backend(
                "checked transpose rank is below two".into(),
            ));
        };
        let trailing_width = trailing
            .iter()
            .try_fold(1u32, |count, extent| count.checked_mul(*extent))
            .ok_or_else(|| {
                CompileError::Backend("typed transpose trailing width overflows".into())
            })?;
        let rows = i64::from(*rows);
        let columns = i64::from(*columns);
        let trailing_width_i64 = i64::from(trailing_width);
        self.for_each_element(destination.value_type.scalar_count(), |this, output| {
            let tail = if trailing_width == 1 {
                this.builder.ins().iconst(types::I64, 0)
            } else {
                this.builder.ins().urem_imm(output, trailing_width_i64)
            };
            let matrix_output = if trailing_width == 1 {
                output
            } else {
                this.builder.ins().udiv_imm(output, trailing_width_i64)
            };
            let output_row = this.builder.ins().udiv_imm(matrix_output, rows);
            let output_column = this.builder.ins().urem_imm(matrix_output, rows);
            let source_matrix = this.builder.ins().imul_imm(output_column, columns);
            let source_matrix = this.builder.ins().iadd(source_matrix, output_row);
            let source = this
                .builder
                .ins()
                .imul_imm(source_matrix, trailing_width_i64);
            let source = this.builder.ins().iadd(source, tail);
            let value = this.load_scalar(&operand, source)?;
            this.store_scalar(&destination, output, value)
        })
    }

    pub(super) fn lower_cross(
        &mut self,
        destination: solve::SolveRegisterId,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        let scalar = destination.value_type.element_type();
        for (output, first, second, other_first, other_second) in
            [(0, 1, 2, 2, 1), (1, 2, 0, 0, 2), (2, 0, 1, 1, 0)]
        {
            let first_index = self.builder.ins().iconst(types::I64, first);
            let second_index = self.builder.ins().iconst(types::I64, second);
            let other_first_index = self.builder.ins().iconst(types::I64, other_first);
            let other_second_index = self.builder.ins().iconst(types::I64, other_second);
            let positive_lhs = self.load_scalar(&lhs, first_index)?;
            let positive_rhs = self.load_scalar(&rhs, second_index)?;
            let negative_lhs = self.load_scalar(&lhs, other_first_index)?;
            let negative_rhs = self.load_scalar(&rhs, other_second_index)?;
            let positive = self.binary_element(
                solve::SolveBinaryOperator::Multiply,
                scalar,
                positive_lhs,
                positive_rhs,
            )?;
            let negative = self.binary_element(
                solve::SolveBinaryOperator::Multiply,
                scalar,
                negative_lhs,
                negative_rhs,
            )?;
            let value = self.binary_element(
                solve::SolveBinaryOperator::Subtract,
                scalar,
                positive,
                negative,
            )?;
            let output = self.builder.ins().iconst(types::I64, output);
            self.store_scalar(&destination, output, value)?;
        }
        Ok(())
    }

    pub(super) fn lower_reduce(
        &mut self,
        destination: solve::SolveRegisterId,
        operator: solve::SolveReductionOperator,
        operand: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let operand = self.register(operand)?.clone();
        let count = operand.value_type.scalar_count();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let initial = self.load_scalar(&operand, zero)?;
        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let exit = self.builder.create_block();
        let scalar = operand.value_type.element_type();
        self.builder.append_block_param(header, types::I64);
        self.builder
            .append_block_param(header, scalar_cranelift_type(scalar));
        self.builder
            .append_block_param(exit, scalar_cranelift_type(scalar));
        let one = self.builder.ins().iconst(types::I64, 1);
        self.builder
            .ins()
            .jump(header, &[one.into(), initial.into()]);
        self.builder.switch_to_block(header);
        let index = self.builder.block_params(header)[0];
        let accumulator = self.builder.block_params(header)[1];
        let in_range =
            self.builder
                .ins()
                .icmp_imm(IntCC::UnsignedLessThan, index, i64::from(count));
        self.builder
            .ins()
            .brif(in_range, body, &[], exit, &[accumulator.into()]);
        self.builder.switch_to_block(body);
        self.builder.seal_block(body);
        let element = self.load_scalar(&operand, index)?;
        let next_accumulator =
            self.binary_element(reduction_binary(operator), scalar, accumulator, element)?;
        let next = self.builder.ins().iadd_imm(index, 1);
        self.builder
            .ins()
            .jump(header, &[next.into(), next_accumulator.into()]);
        self.builder.seal_block(header);
        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        let result = self.builder.block_params(exit)[0];
        self.store_scalar(&destination, zero, result)
    }

    pub(super) fn lower_identity(
        &mut self,
        destination: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let [size, size_again] = destination.value_type.dimensions() else {
            return Err(CompileError::Backend(
                "checked identity is not a matrix".into(),
            ));
        };
        if size != size_again {
            return Err(CompileError::Backend(
                "checked identity is not square".into(),
            ));
        }
        let size = i64::from(*size);
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let row = this.builder.ins().udiv_imm(index, size);
            let column = this.builder.ins().urem_imm(index, size);
            let diagonal = this.builder.ins().icmp(IntCC::Equal, row, column);
            let one = one_scalar(this, destination.value_type.element_type());
            let zero = zero_scalar(this, destination.value_type.element_type());
            let value = this.builder.ins().select(diagonal, one, zero);
            this.store_scalar(&destination, index, value)
        })
    }

    pub(super) fn lower_diagonal(
        &mut self,
        destination: solve::SolveRegisterId,
        operand: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let operand = self.register(operand)?.clone();
        let [size] = operand.value_type.dimensions() else {
            return Err(CompileError::Backend(
                "checked diagonal input is not a vector".into(),
            ));
        };
        let size = i64::from(*size);
        self.for_each_element(destination.value_type.scalar_count(), |this, index| {
            let row = this.builder.ins().udiv_imm(index, size);
            let column = this.builder.ins().urem_imm(index, size);
            let diagonal = this.builder.ins().icmp(IntCC::Equal, row, column);
            let source = this.load_scalar(&operand, row)?;
            let zero = zero_scalar(this, destination.value_type.element_type());
            let value = this.builder.ins().select(diagonal, source, zero);
            this.store_scalar(&destination, index, value)
        })
    }

    pub(super) fn lower_concatenate(
        &mut self,
        destination: solve::SolveRegisterId,
        axis: u32,
        operands: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let axis = axis as usize;
        let destination_dimensions = promoted_dimensions(&destination.value_type);
        let destination_axis = *destination_dimensions.get(axis).ok_or_else(|| {
            CompileError::Backend("checked concatenate axis is out of range".into())
        })?;
        let inner = destination_dimensions[axis + 1..]
            .iter()
            .try_fold(1u32, |count, extent| count.checked_mul(*extent))
            .ok_or_else(|| {
                CompileError::Backend("typed concatenate inner width overflows".into())
            })?;
        let mut axis_prefix = 0u32;
        for operand in operands {
            let operand = self.register(*operand)?.clone();
            let dimensions = promoted_dimensions(&operand.value_type);
            let source_axis = dimensions[axis];
            let source_block = source_axis.checked_mul(inner).ok_or_else(|| {
                CompileError::Backend("typed concatenate source width overflows".into())
            })?;
            let result_block = destination_axis.checked_mul(inner).ok_or_else(|| {
                CompileError::Backend("typed concatenate result width overflows".into())
            })?;
            let prefix = axis_prefix.checked_mul(inner).ok_or_else(|| {
                CompileError::Backend("typed concatenate prefix overflows".into())
            })?;
            self.for_each_element(operand.value_type.scalar_count(), |this, source| {
                let outer = this.builder.ins().udiv_imm(source, i64::from(source_block));
                let within = this.builder.ins().urem_imm(source, i64::from(source_block));
                let target = this.builder.ins().imul_imm(outer, i64::from(result_block));
                let target = this.builder.ins().iadd_imm(target, i64::from(prefix));
                let target = this.builder.ins().iadd(target, within);
                let value = this.load_scalar(&operand, source)?;
                this.store_scalar(&destination, target, value)
            })?;
            axis_prefix = axis_prefix
                .checked_add(source_axis)
                .ok_or_else(|| CompileError::Backend("typed concatenate axis overflows".into()))?;
        }
        Ok(())
    }

    pub(super) fn lower_matrix_multiply(
        &mut self,
        destination: solve::SolveRegisterId,
        lhs: solve::SolveRegisterId,
        rhs: solve::SolveRegisterId,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let lhs = self.register(lhs)?.clone();
        let rhs = self.register(rhs)?.clone();
        let (rows, inner, columns) =
            matrix_extents(lhs.value_type.dimensions(), rhs.value_type.dimensions())?;
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
        let lhs_value = self.load_scalar(&lhs, lhs_index)?;
        let rhs_value = self.load_scalar(&rhs, rhs_index)?;
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
        self.store_scalar(&destination, output, result)?;
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

fn zero_scalar(lowerer: &mut ProgramLowerer<'_, '_>, scalar: solve::SolveScalarType) -> Value {
    match scalar {
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary32,
            ..
        } => lowerer.builder.ins().f32const(0.0),
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        } => lowerer.builder.ins().f64const(0.0),
        solve::SolveScalarType::Integer(_) | solve::SolveScalarType::Boolean => {
            lowerer.builder.ins().iconst(types::I64, 0)
        }
    }
}

fn one_scalar(lowerer: &mut ProgramLowerer<'_, '_>, scalar: solve::SolveScalarType) -> Value {
    match scalar {
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary32,
            ..
        } => lowerer.builder.ins().f32const(1.0),
        solve::SolveScalarType::Real {
            format: solve::SolveRealFormat::Binary64,
            ..
        } => lowerer.builder.ins().f64const(1.0),
        solve::SolveScalarType::Integer(_) | solve::SolveScalarType::Boolean => {
            lowerer.builder.ins().iconst(types::I64, 1)
        }
    }
}

fn reduction_binary(operator: solve::SolveReductionOperator) -> solve::SolveBinaryOperator {
    match operator {
        solve::SolveReductionOperator::Sum => solve::SolveBinaryOperator::Add,
        solve::SolveReductionOperator::Product => solve::SolveBinaryOperator::Multiply,
        solve::SolveReductionOperator::Minimum => solve::SolveBinaryOperator::Min,
        solve::SolveReductionOperator::Maximum => solve::SolveBinaryOperator::Max,
        solve::SolveReductionOperator::All => solve::SolveBinaryOperator::And,
    }
}

fn promoted_dimensions(value_type: &solve::SolveValueType) -> Vec<u32> {
    match value_type.dimensions() {
        [] => vec![1, 1],
        [extent] => vec![1, *extent],
        dimensions => dimensions.to_vec(),
    }
}

fn static_row_major_offset(dimensions: &[u32], indices: &[u32]) -> Result<u32, CompileError> {
    if dimensions.len() != indices.len() {
        return Err(CompileError::Backend(
            "checked static projection rank differs".into(),
        ));
    }
    dimensions
        .iter()
        .copied()
        .zip(indices.iter().copied())
        .try_fold(0u32, |offset, (extent, index)| {
            if index >= extent {
                return Err(CompileError::Backend(
                    "checked static projection coordinate is out of range".into(),
                ));
            }
            offset
                .checked_mul(extent)
                .and_then(|offset| offset.checked_add(index))
                .ok_or_else(|| CompileError::Backend("static projection offset overflows".into()))
        })
}

fn remap_offset(
    lowerer: &mut ProgramLowerer<'_, '_>,
    output: Value,
    output_dimensions: &[u32],
    aggregate_dimensions: &[u32],
    origin: &[u32],
) -> Result<Value, CompileError> {
    if output_dimensions.len() != aggregate_dimensions.len()
        || output_dimensions.len() != origin.len()
    {
        return Err(CompileError::Backend(
            "checked tensor slice rank differs".into(),
        ));
    }
    let mut source = lowerer.builder.ins().iconst(types::I64, 0);
    for (axis, (&extent, &axis_origin)) in aggregate_dimensions.iter().zip(origin).enumerate() {
        let coordinate = coordinate_at(lowerer, output, output_dimensions, axis)?;
        let coordinate = lowerer
            .builder
            .ins()
            .iadd_imm(coordinate, i64::from(axis_origin));
        source = lowerer.builder.ins().imul_imm(source, i64::from(extent));
        source = lowerer.builder.ins().iadd(source, coordinate);
    }
    Ok(source)
}

fn coordinate_at(
    lowerer: &mut ProgramLowerer<'_, '_>,
    ordinal: Value,
    dimensions: &[u32],
    axis: usize,
) -> Result<Value, CompileError> {
    let extent = *dimensions
        .get(axis)
        .ok_or_else(|| CompileError::Backend("tensor coordinate axis is out of range".into()))?;
    let stride = dimensions[axis + 1..]
        .iter()
        .try_fold(1u32, |count, extent| count.checked_mul(*extent))
        .ok_or_else(|| CompileError::Backend("tensor coordinate stride overflows".into()))?;
    let coordinate = if stride == 1 {
        ordinal
    } else {
        lowerer.builder.ins().udiv_imm(ordinal, i64::from(stride))
    };
    Ok(if extent == 1 {
        lowerer.builder.ins().iconst(types::I64, 0)
    } else {
        lowerer
            .builder
            .ins()
            .urem_imm(coordinate, i64::from(extent))
    })
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
