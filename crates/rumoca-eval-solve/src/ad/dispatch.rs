use super::*;

impl<'a> AdBuilder<'a> {
    // SPEC_0021: Exception - exhaustive AD lowering dispatch over every LinearOp variant.
    #[allow(clippy::too_many_lines)]
    pub(super) fn lower_op(&mut self, op: LinearOp) -> Result<(), SolveDerivationError> {
        match op {
            LinearOp::PureCallObservation { .. } => Err(unsupported(
                "assertion observation is outside the scalar AD contract",
            )),
            LinearOp::Const { dst, value } => self.lower_const(dst, value),
            LinearOp::LoadTime { dst } => self.lower_load_time(dst),
            LinearOp::LoadY { dst, index } => self.lower_load_y(dst, index),
            LinearOp::LoadP { dst, index } => self.lower_load_p(dst, index),
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => self.lower_load_indexed_p(dst, base, count, index),
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_load_indexed_register(dst, base, stride, dimensions, &indices),
            LinearOp::LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_load_indexed_fold_carried(dst, base, stride, dimensions, &indices),
            LinearOp::LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_load_indexed_fold_capture(dst, base, stride, dimensions, &indices),
            LinearOp::LoadSeed { .. } => Err(unsupported("unexpected LoadSeed in primal row")),
            LinearOp::LoadIndexedSeed { .. } => {
                Err(unsupported("unexpected LoadIndexedSeed in primal row"))
            }
            LinearOp::LoadFoldCarried { dst, index } => self.lower_load_fold_carried(dst, index),
            LinearOp::LoadFoldIndex { dst, dimension } => {
                self.lower_load_fold_index(dst, dimension)
            }
            LinearOp::LoadFoldCapture { dst, index } => self.lower_load_fold_capture(dst, index),
            LinearOp::LoadFunctionConditionalCapture { dst, index } => {
                self.lower_load_function_conditional_capture(dst, index)
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => self.lower_load_function_conditional_capture_range(dst_start, index_start, count),
            LinearOp::Move { dst, src } => self.lower_move(dst, src),
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => self.lower_linear_solve_component(dst, matrix_start, rhs_start, n, component),
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => self.lower_dot_product(
                dst,
                (
                    StridedOperand {
                        start: lhs_start,
                        stride: lhs_stride,
                    },
                    StridedOperand {
                        start: rhs_start,
                        stride: rhs_stride,
                    },
                ),
                count,
            ),
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            } => self.lower_matrix_multiply(
                dst_start,
                lhs_start,
                rhs_start,
                MatrixProductShape {
                    rows,
                    inner,
                    columns,
                    lanes,
                },
            ),
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
            } => self.lower_tensor_binary(
                dst_start,
                op,
                (
                    StridedOperand {
                        start: lhs_start,
                        stride: lhs_stride,
                    },
                    StridedOperand {
                        start: rhs_start,
                        stride: rhs_stride,
                    },
                ),
                count,
                lanes,
            ),
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes,
            } => self.lower_tensor_cross(dst_start, lhs_start, rhs_start, lanes),
            LinearOp::TableBounds { dst, table_id, max } => {
                self.lower_table_bounds(dst, table_id, max)
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => self.lower_table_lookup(dst, table_id, column, input),
            LinearOp::TableLookupSlope { .. } => {
                Err(unsupported("unexpected TableLookupSlope in primal row"))
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => self.lower_table_next_event(dst, table_id, time),
            LinearOp::RandomInitialState {
                dst,
                generator,
                local_seed,
                global_seed,
                state_len,
                state_index,
            } => self.lower_random_initial_state(
                dst,
                generator,
                local_seed,
                global_seed,
                state_len,
                state_index,
            ),
            LinearOp::RandomResult {
                dst,
                generator,
                state_start,
                state_len,
            } => self.lower_random_result(dst, generator, state_start, state_len),
            LinearOp::RandomState {
                dst,
                generator,
                state_start,
                state_len,
                state_index,
            } => self.lower_random_state(dst, generator, state_start, state_len, state_index),
            LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => {
                Err(unsupported("impure random solve-IR ops are discrete-only"))
            }
            LinearOp::Unary { dst, op, arg } => self.lower_unary(dst, op, arg),
            LinearOp::Binary { dst, op, lhs, rhs } => self.lower_binary(dst, op, lhs, rhs),
            LinearOp::Compare { dst, op, lhs, rhs } => self.lower_compare(dst, op, lhs, rhs),
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.lower_select(dst, cond, if_true, if_false),
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => self.lower_function_fold(dst_start, initial_start, capture_start, program),
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => self.lower_guarded_function_fold(
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            ),
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => self.lower_function_conditional(dst_start, capture_start, program),
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => self.lower_pure_call(dst_start, &input_starts, site),
            LinearOp::PureCallDirectional { .. } => {
                Err(unsupported("unexpected directional call in primal row"))
            }
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base,
                source_stride,
                dimensions,
                updates,
                nodes,
                result,
                lanes,
            } => self.lower_store_fold_tensor_update(FoldTensorUpdateStore {
                source_base,
                source_stride,
                dimensions: &dimensions,
                updates: &updates,
                nodes: &nodes,
                result,
                lanes,
            }),
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            } => self.lower_tensor_transpose(
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            ),
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes,
            } => self.lower_tensor_concatenate(dst_start, &sources, dimensions, axis, lanes),
            LinearOp::TensorUpdate {
                dst_start,
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
            } => self.lower_tensor_update(
                dst_start,
                base_start,
                value_start,
                dimensions,
                &subscripts,
                lanes,
            ),
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                lanes,
            } => self.lower_tensor_fill(dst_start, value_start, count, lanes),
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes,
            } => self.lower_tensor_identity(dst_start, size, lanes),
            LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                seed_start,
                lanes,
            } => self.lower_tensor_load(dst_start, input, input_start, count, seed_start, lanes),
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                result_base,
                count,
                condition,
                nested_when_true,
            } => self.lower_store_output_function_fold(
                initial,
                capture_start,
                program,
                (result_base, count),
                condition,
                nested_when_true,
            ),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => self.lower_store_range(start, count, stride),
            LinearOp::StoreOutput { src } => self.lower_store(src),
        }
    }
}
