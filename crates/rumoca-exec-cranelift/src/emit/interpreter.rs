use super::*;

#[derive(Clone, Copy)]
struct GeneralOpContext<'a> {
    inputs: RowInputs<'a>,
    fold_carried: Option<&'a [f64]>,
    fold_indices: Option<&'a [i64]>,
    fold_captures: Option<&'a [f64]>,
    conditional_captures: Option<&'a [f64]>,
}

impl<'a> From<RowInputs<'a>> for GeneralOpContext<'a> {
    fn from(inputs: RowInputs<'a>) -> Self {
        Self {
            inputs,
            fold_carried: None,
            fold_indices: None,
            fold_captures: None,
            conditional_captures: None,
        }
    }
}

#[inline(always)]
pub(super) fn execute_row(
    row: &RowPlan,
    regs_scratch: &mut Vec<f64>,
    inputs: RowInputs<'_>,
    out: &mut [f64],
) -> Result<(), CompileError> {
    validate_input_requirements(row_input_requirements(row), inputs.y, inputs.p, inputs.seed)?;
    match row {
        RowPlan::Simple(row) => {
            execute_simple_row(row, regs_scratch, inputs.y, inputs.p, inputs.t, out)
        }
        RowPlan::General(row) => execute_general_row(row, regs_scratch, inputs, out),
    }
}

#[inline(always)]
fn execute_simple_row(
    row: &SimpleRowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) -> Result<(), CompileError> {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count)?;
    for op in row.ops.iter().cloned() {
        match op {
            SimpleOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
            SimpleOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
            SimpleOp::LoadY { dst, index } => {
                let value = read_input_value("y", y, index as usize)?;
                set_reg_value(regs, dst as usize, value)
            }
            SimpleOp::LoadP { dst, index } => {
                let value = read_input_value("p", p, index as usize)?;
                set_reg_value(regs, dst as usize, value)
            }
            SimpleOp::Unary { dst, op, arg } => {
                let x = read_reg_value(regs, arg as usize);
                set_reg_value(regs, dst as usize, rumoca_eval_solve::eval_unary(op, x));
            }
            SimpleOp::Binary { dst, op, lhs, rhs } => {
                let lhs = read_reg_value(regs, lhs as usize);
                let rhs = read_reg_value(regs, rhs as usize);
                set_reg_value(
                    regs,
                    dst as usize,
                    rumoca_eval_solve::eval_binary(op, lhs, rhs),
                );
            }
            SimpleOp::Compare { dst, op, lhs, rhs } => {
                let lhs = read_reg_value(regs, lhs as usize);
                let rhs = read_reg_value(regs, rhs as usize);
                set_reg_value(
                    regs,
                    dst as usize,
                    rumoca_eval_solve::eval_compare(op, lhs, rhs),
                );
            }
            SimpleOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let cond = read_reg_value(regs, cond as usize);
                let if_true = read_reg_value(regs, if_true as usize);
                let if_false = read_reg_value(regs, if_false as usize);
                set_reg_value(
                    regs,
                    dst as usize,
                    if cond != 0.0 { if_true } else { if_false },
                );
            }
        }
    }
    for (slot, &src) in out.iter_mut().zip(row.output_srcs.iter()) {
        *slot = read_reg_value(regs, src);
    }
    Ok(())
}

#[inline(always)]
fn execute_general_row(
    row: &GeneralRowPlan,
    regs_scratch: &mut Vec<f64>,
    inputs: RowInputs<'_>,
    out: &mut [f64],
) -> Result<(), CompileError> {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count)?;
    for op in row.ops.iter().cloned() {
        execute_general_op(regs, op, inputs.into())?;
    }
    for (slot, &src) in out.iter_mut().zip(row.output_srcs.iter()) {
        *slot = read_reg_value(regs, src);
    }
    Ok(())
}

#[inline(always)]
// SPEC_0021: exhaustive dispatch over every LinearOp execution contract.
#[expect(
    clippy::too_many_lines,
    clippy::excessive_nesting,
    reason = "one exhaustive interpreter dispatch keeps LinearOp behavior reviewable"
)]
fn execute_general_op(
    regs: &mut [f64],
    op: LinearOp,
    context: GeneralOpContext<'_>,
) -> Result<(), CompileError> {
    let GeneralOpContext {
        inputs:
            RowInputs {
                y,
                p,
                t,
                seed,
                external_tables,
            },
        fold_carried,
        fold_indices,
        fold_captures,
        conditional_captures,
    } = context;
    match op {
        LinearOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
        LinearOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
        LinearOp::LoadY { dst, index } => {
            let value = read_input_value("y", y, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadP { dst, index } => {
            let value = read_input_value("p", p, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadIndexedRegister {
            dst,
            base,
            stride,
            dimensions,
            indices,
        } => {
            let offset = tensor_register_offset(regs, &dimensions, &indices);
            let value = read_reg_value(regs, base as usize + offset * stride);
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadIndexedFoldCarried {
            dst,
            base,
            stride,
            dimensions,
            indices,
        } => {
            let carried = fold_carried.ok_or_else(|| {
                CompileError::Backend(
                    "indexed function-fold carried load escaped its update body".into(),
                )
            })?;
            let offset = tensor_register_offset(regs, &dimensions, &indices);
            let value = carried[base + offset * stride];
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadIndexedFoldCapture {
            dst,
            base,
            stride,
            dimensions,
            indices,
        } => {
            let captures = fold_captures.ok_or_else(|| {
                CompileError::Backend(
                    "indexed function-fold capture load escaped its update body".into(),
                )
            })?;
            let offset = tensor_register_offset(regs, &dimensions, &indices);
            let value = captures[base + offset * stride];
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadSeed { dst, index } => {
            let seed = seed.ok_or_else(|| input_compile_error("seed", index, 0))?;
            let value = read_input_value("seed", seed, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadFoldCarried { dst, index } => {
            let value = fold_carried
                .and_then(|values| values.get(index))
                .copied()
                .ok_or_else(|| {
                    CompileError::Backend("invalid function-fold carried load".into())
                })?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadFoldIndex { dst, dimension } => {
            let value = fold_indices
                .and_then(|values| values.get(dimension))
                .copied()
                .ok_or_else(|| CompileError::Backend("invalid function-fold binder load".into()))?;
            set_reg_value(regs, dst as usize, value as f64);
        }
        LinearOp::LoadFoldCapture { dst, index } => {
            let value = fold_captures
                .and_then(|values| values.get(index))
                .copied()
                .ok_or_else(|| {
                    CompileError::Backend("invalid function-fold capture load".into())
                })?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadFunctionConditionalCapture { dst, index } => {
            let value = conditional_captures
                .and_then(|values| values.get(index))
                .copied()
                .ok_or_else(|| {
                    CompileError::Backend(
                        "function-conditional capture load escaped its region".into(),
                    )
                })?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadFunctionConditionalCaptureRange {
            dst_start,
            index_start,
            count,
        } => {
            let captures = conditional_captures.ok_or_else(|| {
                CompileError::Backend(
                    "function-conditional capture range load escaped its region".into(),
                )
            })?;
            for offset in 0..count {
                let value = *captures.get(index_start + offset).ok_or_else(|| {
                    CompileError::Backend("invalid function-conditional capture range".into())
                })?;
                set_reg_value(regs, dst_start as usize + offset, value);
            }
        }
        LinearOp::Move { dst, src } => {
            let value = read_reg_value(regs, src as usize);
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LinearSolveComponent { dst, .. } => {
            let value = eval_linear_solve_component(regs, op)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::DotProduct {
            dst,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
        } => {
            let mut value = 0.0;
            for term in 0..count {
                value += read_reg_value(regs, lhs_start as usize + term * lhs_stride)
                    * read_reg_value(regs, rhs_start as usize + term * rhs_stride);
            }
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::MatrixMultiply {
            dst_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
        } => {
            for row in 0..rows {
                for column in 0..columns {
                    let output = (row * columns + column) * lanes;
                    let mut re = 0.0;
                    let mut du = 0.0;
                    for term in 0..inner {
                        let lhs = (row * inner + term) * lanes;
                        let rhs = (term * columns + column) * lanes;
                        let lhs_re = read_reg_value(regs, lhs_start as usize + lhs);
                        let rhs_re = read_reg_value(regs, rhs_start as usize + rhs);
                        re += lhs_re * rhs_re;
                        if lanes == 2 {
                            du += read_reg_value(regs, lhs_start as usize + lhs + 1) * rhs_re
                                + lhs_re * read_reg_value(regs, rhs_start as usize + rhs + 1);
                        }
                    }
                    set_reg_value(regs, dst_start as usize + output, re);
                    if lanes == 2 {
                        set_reg_value(regs, dst_start as usize + output + 1, du);
                    }
                }
            }
        }
        LinearOp::TensorBinary {
            dst_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
        } => {
            for element in 0..count {
                let lhs = lhs_start as usize + element * lhs_stride * lanes;
                let rhs = rhs_start as usize + element * rhs_stride * lanes;
                let dst = dst_start as usize + element * lanes;
                let lhs_re = read_reg_value(regs, lhs);
                let rhs_re = read_reg_value(regs, rhs);
                let raw_primal = rumoca_eval_solve::eval_binary(op, lhs_re, rhs_re);
                let primal = if lanes == 2 && op == BinaryOp::Div && rhs_re == 0.0 && lhs_re == 0.0
                {
                    0.0
                } else {
                    raw_primal
                };
                set_reg_value(regs, dst, primal);
                if lanes == 2 {
                    let tangent = match op {
                        BinaryOp::Add => {
                            read_reg_value(regs, lhs + 1) + read_reg_value(regs, rhs + 1)
                        }
                        BinaryOp::Sub => {
                            read_reg_value(regs, lhs + 1) - read_reg_value(regs, rhs + 1)
                        }
                        BinaryOp::Mul => {
                            read_reg_value(regs, lhs + 1) * rhs_re
                                + lhs_re * read_reg_value(regs, rhs + 1)
                        }
                        BinaryOp::Div => {
                            if rhs_re == 0.0 {
                                0.0
                            } else {
                                (read_reg_value(regs, lhs + 1) * rhs_re
                                    - lhs_re * read_reg_value(regs, rhs + 1))
                                    / (rhs_re * rhs_re)
                            }
                        }
                        _ => unreachable!("validated tensor binary operator"),
                    };
                    set_reg_value(regs, dst + 1, tangent);
                }
            }
        }
        LinearOp::TensorCross {
            dst_start,
            lhs_start,
            rhs_start,
            lanes,
        } => {
            let count = 3 * lanes;
            let mut lhs = [0.0; 6];
            let mut rhs = [0.0; 6];
            for offset in 0..count {
                lhs[offset] = read_reg_value(regs, lhs_start as usize + offset);
                rhs[offset] = read_reg_value(regs, rhs_start as usize + offset);
            }
            for (component, (first, second)) in
                [(1usize, 2usize), (2, 0), (0, 1)].into_iter().enumerate()
            {
                let dst = dst_start as usize + component * lanes;
                let first = first * lanes;
                let second = second * lanes;
                set_reg_value(
                    regs,
                    dst,
                    lhs[first] * rhs[second] - lhs[second] * rhs[first],
                );
                if lanes == 2 {
                    set_reg_value(
                        regs,
                        dst + 1,
                        lhs[first + 1] * rhs[second] + lhs[first] * rhs[second + 1]
                            - lhs[second + 1] * rhs[first]
                            - lhs[second] * rhs[first + 1],
                    );
                }
            }
        }
        LinearOp::TensorTranspose {
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            lanes,
        } => {
            let value_width = element_width * lanes;
            for row in 0..rows {
                for column in 0..columns {
                    for value in 0..value_width {
                        let dst =
                            dst_start as usize + (row * columns + column) * value_width + value;
                        let src = src_start as usize + (column * rows + row) * value_width + value;
                        set_reg_value(regs, dst, read_reg_value(regs, src));
                    }
                }
            }
        }
        LinearOp::TensorConcatenate {
            dst_start,
            sources,
            dimensions,
            axis,
            lanes,
        } => {
            let inner = dimensions[axis + 1..]
                .iter()
                .fold(1usize, |count, extent| count * *extent as usize);
            let result_axis = dimensions[axis] as usize;
            let mut axis_offset = 0usize;
            for source in sources.iter() {
                let source_axis = source.dimensions[axis] as usize;
                let source_count = source
                    .dimensions
                    .iter()
                    .fold(1usize, |count, extent| count * *extent as usize);
                let source_block = source_axis * inner;
                for element in 0..source_count {
                    let outer = element / source_block;
                    let within = element % source_block;
                    let destination = outer * result_axis * inner + axis_offset * inner + within;
                    for lane in 0..lanes {
                        let value =
                            read_reg_value(regs, source.start as usize + element * lanes + lane);
                        set_reg_value(regs, dst_start as usize + destination * lanes + lane, value);
                    }
                }
                axis_offset += source_axis;
            }
        }
        LinearOp::TensorUpdate {
            dst_start,
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
        } => {
            let count = dimensions
                .iter()
                .fold(1usize, |count, extent| count * *extent as usize);
            for element in 0..count {
                let mut value_offset = 0usize;
                let mut axis_stride = count;
                let mut selected = true;
                for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
                    axis_stride /= extent as usize;
                    let coordinate = (element / axis_stride) % extent as usize;
                    match subscript {
                        rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                            value_offset = value_offset * extent as usize + coordinate;
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Index(index) => {
                            let index = match *index {
                                rumoca_ir_solve::TensorIndex::Constant(index) => index as usize,
                                rumoca_ir_solve::TensorIndex::Runtime(register) => {
                                    read_reg_value(regs, register as usize) as usize - 1
                                }
                            };
                            if index != coordinate {
                                selected = false;
                                break;
                            }
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                            let slice_count = dimensions
                                .iter()
                                .fold(1usize, |count, extent| count * *extent as usize);
                            let slice_offset = (0..slice_count).find(|offset| {
                                read_reg_value(regs, *start as usize + *offset)
                                    == (coordinate + 1) as f64
                            });
                            let Some(slice_offset) = slice_offset else {
                                selected = false;
                                break;
                            };
                            value_offset = value_offset * slice_count + slice_offset;
                        }
                    }
                }
                for lane in 0..lanes {
                    let source = if selected {
                        value_start as usize + value_offset * lanes + lane
                    } else {
                        base_start as usize + element * lanes + lane
                    };
                    set_reg_value(
                        regs,
                        dst_start as usize + element * lanes + lane,
                        read_reg_value(regs, source),
                    );
                }
            }
        }
        LinearOp::TensorFill {
            dst_start,
            value_start,
            count,
            lanes,
        } => {
            for element in 0..count {
                for lane in 0..lanes {
                    set_reg_value(
                        regs,
                        dst_start as usize + element * lanes + lane,
                        read_reg_value(regs, value_start as usize + lane),
                    );
                }
            }
        }
        LinearOp::TensorIdentity {
            dst_start,
            size,
            lanes,
        } => {
            for row in 0..size {
                for column in 0..size {
                    for lane in 0..lanes {
                        set_reg_value(
                            regs,
                            dst_start as usize + (row * size + column) * lanes + lane,
                            f64::from(lane == 0 && row == column),
                        );
                    }
                }
            }
        }
        LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count,
            seed_start,
            lanes,
        } => {
            let input = match input {
                rumoca_ir_solve::TensorInputKind::Y => y,
                rumoca_ir_solve::TensorInputKind::P => p,
            };
            for element in 0..count {
                set_reg_value(
                    regs,
                    dst_start as usize + element * lanes,
                    input[input_start + element],
                );
                if lanes == 2 {
                    let tangent = seed_start
                        .and_then(|start| seed.and_then(|seed| seed.get(start + element)))
                        .copied()
                        .unwrap_or(0.0);
                    set_reg_value(regs, dst_start as usize + element * lanes + 1, tangent);
                }
            }
        }
        LinearOp::TableBounds { dst, table_id, max } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let operation = if max { "bounds max" } else { "bounds min" };
            let value = eval_table_bound_value_in(table_id, max, external_tables)
                .map_err(|error| table_compile_error(operation, table_id, None, error))?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. } => execute_general_table_op(regs, op, external_tables)?,
        LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. } => {
            return Err(CompileError::Backend(
                "cranelift interpreter does not support discrete random solve-IR ops".into(),
            ));
        }
        LinearOp::Unary { dst, op, arg } => {
            let x = read_reg_value(regs, arg as usize);
            set_reg_value(regs, dst as usize, rumoca_eval_solve::eval_unary(op, x));
        }
        LinearOp::Binary { dst, op, lhs, rhs } => {
            let lhs = read_reg_value(regs, lhs as usize);
            let rhs = read_reg_value(regs, rhs as usize);
            set_reg_value(
                regs,
                dst as usize,
                rumoca_eval_solve::eval_binary(op, lhs, rhs),
            );
        }
        LinearOp::Compare { dst, op, lhs, rhs } => {
            let lhs = read_reg_value(regs, lhs as usize);
            let rhs = read_reg_value(regs, rhs as usize);
            set_reg_value(
                regs,
                dst as usize,
                rumoca_eval_solve::eval_compare(op, lhs, rhs),
            );
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => {
            let cond = read_reg_value(regs, cond as usize);
            let if_true = read_reg_value(regs, if_true as usize);
            let if_false = read_reg_value(regs, if_false as usize);
            set_reg_value(
                regs,
                dst as usize,
                if cond != 0.0 { if_true } else { if_false },
            );
        }
        LinearOp::FunctionFold {
            dst_start,
            initial_start,
            capture_start,
            program,
        } => {
            let mut carried = (0..program.carried_count())
                .map(|offset| read_reg_value(regs, initial_start as usize + offset))
                .collect::<Vec<_>>();
            let captures = (0..program.capture_count())
                .map(|offset| read_reg_value(regs, capture_start as usize + offset))
                .collect::<Vec<_>>();
            let update_count = program.register_count();
            let domain = program
                .domain()
                .validated()
                .map_err(|error| CompileError::Backend(error.to_string()))?;
            for indices in domain.index_tuple_iter() {
                let mut update_regs = vec![0.0; update_count];
                let mut outputs = Vec::with_capacity(program.carried_count());
                for operation in program.update().iter().cloned() {
                    match operation {
                        LinearOp::StoreOutput { src } => {
                            outputs.push(read_reg_value(&update_regs, src as usize));
                        }
                        LinearOp::StoreOutputFoldTensorUpdate {
                            source_base,
                            source_stride,
                            dimensions,
                            updates,
                            nodes,
                            result,
                            lanes,
                        } => {
                            let count = dimensions.iter().map(|&extent| extent as usize).product();
                            for element in 0..count {
                                let offsets = updates
                                    .iter()
                                    .map(|update| {
                                        tensor_update_value_offset(
                                            &update_regs,
                                            &dimensions,
                                            &update.subscripts,
                                            element,
                                        )
                                    })
                                    .collect::<Vec<_>>();
                                for lane in 0..lanes {
                                    let source =
                                        carried[source_base + element * source_stride + lane];
                                    let mut values = Vec::with_capacity(nodes.len() + 1);
                                    values.push(source);
                                    for node in &nodes {
                                        let value = match *node {
                                            rumoca_ir_solve::FoldTensorNode::Update {
                                                base,
                                                update,
                                            } => {
                                                let patch = &updates[update as usize];
                                                let enabled = patch
                                                    .condition
                                                    .map(|condition| {
                                                        read_reg_value(
                                                            &update_regs,
                                                            condition as usize,
                                                        ) != 0.0
                                                    })
                                                    .unwrap_or(true);
                                                match (enabled, offsets[update as usize]) {
                                                    (true, Some(value_element)) => read_reg_value(
                                                        &update_regs,
                                                        patch.value_start as usize
                                                            + value_element * patch.value_stride
                                                            + lane,
                                                    ),
                                                    _ => values[base as usize],
                                                }
                                            }
                                            rumoca_ir_solve::FoldTensorNode::Select {
                                                condition,
                                                if_true,
                                                if_false,
                                            } => {
                                                if read_reg_value(&update_regs, condition as usize)
                                                    != 0.0
                                                {
                                                    values[if_true as usize]
                                                } else {
                                                    values[if_false as usize]
                                                }
                                            }
                                        };
                                        values.push(value);
                                    }
                                    outputs.push(values[result as usize]);
                                }
                            }
                        }
                        LinearOp::StoreOutputFunctionFold {
                            initial,
                            capture_start,
                            program,
                            result_base,
                            count,
                            condition,
                            nested_when_true,
                        } => {
                            let mut initial_values = Vec::with_capacity(program.carried_count());
                            for source in initial.iter() {
                                match *source {
                                    rumoca_ir_solve::FoldInitialSource::Registers {
                                        start,
                                        count,
                                    } => {
                                        for offset in 0..count {
                                            initial_values.push(read_reg_value(
                                                &update_regs,
                                                start as usize + offset,
                                            ));
                                        }
                                    }
                                    rumoca_ir_solve::FoldInitialSource::ParentCarried {
                                        base,
                                        count,
                                    } => initial_values
                                        .extend_from_slice(&carried[base..base + count]),
                                }
                            }
                            let output_base = outputs.len();
                            let use_nested = condition
                                .map(|condition| {
                                    (read_reg_value(&update_regs, condition as usize) != 0.0)
                                        == nested_when_true
                                })
                                .unwrap_or(true);
                            if use_nested {
                                let captures = (0..program.capture_count())
                                    .map(|offset| {
                                        read_reg_value(
                                            &update_regs,
                                            capture_start as usize + offset,
                                        )
                                    })
                                    .collect::<Vec<_>>();
                                let folded = eval_function_fold(
                                    &program,
                                    &initial_values,
                                    &captures,
                                    context.inputs,
                                )?;
                                outputs
                                    .extend_from_slice(&folded[result_base..result_base + count]);
                            } else {
                                outputs
                                    .extend_from_slice(&carried[output_base..output_base + count]);
                            }
                        }
                        operation => {
                            execute_general_op(
                                &mut update_regs,
                                operation,
                                GeneralOpContext {
                                    fold_carried: Some(&carried),
                                    fold_indices: Some(&indices),
                                    fold_captures: Some(&captures),
                                    ..context
                                },
                            )?;
                        }
                    }
                }
                carried = outputs;
            }
            for (offset, value) in carried.into_iter().enumerate() {
                set_reg_value(regs, dst_start as usize + offset, value);
            }
        }
        LinearOp::GuardedFunctionFold {
            dst_start,
            initial_start,
            capture_start,
            activation,
            program,
        } => {
            if read_reg_value(regs, activation as usize) != 0.0 {
                execute_general_op(
                    regs,
                    LinearOp::FunctionFold {
                        dst_start,
                        initial_start,
                        capture_start,
                        program,
                    },
                    context,
                )?;
            } else {
                for offset in 0..program.carried_count() {
                    let value = read_reg_value(regs, initial_start as usize + offset);
                    set_reg_value(regs, dst_start as usize + offset, value);
                }
            }
        }
        LinearOp::FunctionConditional {
            dst_start,
            capture_start,
            program,
        } => {
            let captures = (0..program.capture_count())
                .map(|offset| read_reg_value(regs, capture_start as usize + offset))
                .collect::<Vec<_>>();
            let mut selected = None;
            for arm in program.arms() {
                let condition = eval_function_conditional_region(
                    arm.condition(),
                    arm.condition_register_count(),
                    &captures,
                    context,
                )?;
                if condition[0] != 0.0 {
                    selected = Some(eval_function_conditional_region(
                        arm.result(),
                        arm.result_register_count(),
                        &captures,
                        context,
                    )?);
                    break;
                }
            }
            let values = match selected {
                Some(values) => values,
                None => eval_function_conditional_region(
                    program.fallback(),
                    program.fallback_register_count(),
                    &captures,
                    context,
                )?,
            };
            for (offset, value) in values.into_iter().enumerate() {
                set_reg_value(regs, dst_start as usize + offset, value);
            }
        }
        LinearOp::PureCall { .. } | LinearOp::PureCallDirectional { .. } => {
            return Err(CompileError::Backend(
                "typed pure-call interpreter requires the model call table".into(),
            ));
        }
        LinearOp::StoreOutputFoldTensorUpdate { .. }
        | LinearOp::StoreOutputFunctionFold { .. }
        | LinearOp::StoreOutputRange { .. }
        | LinearOp::StoreOutput { .. } => {}
    }
    Ok(())
}

fn eval_function_fold(
    program: &rumoca_ir_solve::FunctionFoldProgram,
    initial: &[f64],
    captures: &[f64],
    inputs: RowInputs<'_>,
) -> Result<Vec<f64>, CompileError> {
    let initial_start = 0;
    let capture_start = initial.len();
    let dst_start = capture_start + captures.len();
    let mut registers = vec![0.0; dst_start + program.carried_count()];
    registers[..initial.len()].copy_from_slice(initial);
    registers[capture_start..dst_start].copy_from_slice(captures);
    execute_general_op(
        &mut registers,
        LinearOp::FunctionFold {
            dst_start: dst_start as u32,
            initial_start: initial_start as u32,
            capture_start: capture_start as u32,
            program: std::sync::Arc::new(program.clone()),
        },
        inputs.into(),
    )?;
    Ok(registers[dst_start..].to_vec())
}

fn eval_function_conditional_region(
    program: &[LinearOp],
    register_count: usize,
    captures: &[f64],
    context: GeneralOpContext<'_>,
) -> Result<Vec<f64>, CompileError> {
    let mut registers = vec![0.0; register_count];
    let mut outputs = Vec::new();
    for operation in program.iter().cloned() {
        match operation {
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for ordinal in 0..count {
                    outputs.push(read_reg_value(
                        &registers,
                        start as usize + ordinal * stride,
                    ));
                }
            }
            LinearOp::StoreOutput { src } => {
                outputs.push(read_reg_value(&registers, src as usize));
            }
            LinearOp::StoreOutputFoldTensorUpdate { .. }
            | LinearOp::StoreOutputFunctionFold { .. } => {
                return Err(CompileError::Backend(
                    "aggregate output escaped function-conditional region lowering".into(),
                ));
            }
            operation => execute_general_op(
                &mut registers,
                operation,
                GeneralOpContext {
                    conditional_captures: Some(captures),
                    ..context
                },
            )?,
        }
    }
    Ok(outputs)
}

fn tensor_register_offset(
    regs: &[f64],
    dimensions: &[u32],
    indices: &[rumoca_ir_solve::TensorIndex],
) -> usize {
    let mut offset = 0usize;
    for (&extent, index) in dimensions.iter().zip(indices) {
        let coordinate = tensor_index_coordinate(regs, *index);
        offset = offset * extent as usize + coordinate;
    }
    offset
}

fn tensor_update_value_offset(
    regs: &[f64],
    dimensions: &[u32],
    subscripts: &[rumoca_ir_solve::TensorSubscript],
    element: usize,
) -> Option<usize> {
    let mut value_offset = 0usize;
    let mut axis_stride: usize = dimensions.iter().map(|&extent| extent as usize).product();
    for (&extent, subscript) in dimensions.iter().zip(subscripts) {
        axis_stride /= extent as usize;
        let coordinate = (element / axis_stride) % extent as usize;
        match *subscript {
            rumoca_ir_solve::TensorSubscript::Whole => {
                value_offset = value_offset * extent as usize + coordinate;
            }
            rumoca_ir_solve::TensorSubscript::Index(index) => {
                let selected = tensor_index_coordinate(regs, index);
                if selected != coordinate {
                    return None;
                }
            }
        }
    }
    Some(value_offset)
}

fn tensor_index_coordinate(regs: &[f64], index: rumoca_ir_solve::TensorIndex) -> usize {
    match index {
        rumoca_ir_solve::TensorIndex::Constant(coordinate) => coordinate as usize,
        rumoca_ir_solve::TensorIndex::Runtime(register) => {
            read_reg_value(regs, register as usize) as usize - 1
        }
    }
}

fn execute_general_table_op(
    regs: &mut [f64],
    op: LinearOp,
    external_tables: &[ExternalTableData],
) -> Result<(), CompileError> {
    match op {
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let column = read_reg_value(regs, column as usize);
            let input = read_reg_value(regs, input as usize);
            let value = eval_table_lookup_value_in(table_id, column, input, external_tables)
                .map_err(|error| table_compile_error("lookup", table_id, Some(column), error))?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let column = read_reg_value(regs, column as usize);
            let input = read_reg_value(regs, input as usize);
            let value = eval_table_lookup_slope_value_in(table_id, column, input, external_tables)
                .map_err(|error| {
                    table_compile_error("lookup slope", table_id, Some(column), error)
                })?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let time = read_reg_value(regs, time as usize);
            let value = eval_time_table_next_event_value_in(table_id, time, external_tables)
                .map_err(|error| table_compile_error("next event", table_id, None, error))?;
            set_reg_value(regs, dst as usize, value);
        }
        _ => {}
    }
    Ok(())
}

fn table_compile_error(
    operation: &'static str,
    table_id: f64,
    column: Option<f64>,
    error: impl std::fmt::Display,
) -> CompileError {
    let message = if let Some(column) = column {
        format!(
            "external table {operation} failed for table id {table_id} column {column}: {error}"
        )
    } else {
        format!("external table {operation} failed for table id {table_id}: {error}")
    };
    CompileError::Input(message)
}

fn read_input_value(
    vector: &'static str,
    values: &[f64],
    index: usize,
) -> Result<f64, CompileError> {
    values
        .get(index)
        .copied()
        .ok_or_else(|| input_compile_error(vector, index, values.len()))
}

#[inline(always)]
fn runtime_reg_slice(
    regs_scratch: &mut Vec<f64>,
    reg_count: usize,
) -> Result<&mut [f64], CompileError> {
    if regs_scratch.len() < reg_count {
        let additional = reg_count - regs_scratch.len();
        regs_scratch.try_reserve(additional).map_err(|_| {
            CompileError::Backend(format!(
                "runtime register scratch allocation overflow for {reg_count} registers"
            ))
        })?;
        regs_scratch.resize(reg_count, 0.0);
    }
    Ok(&mut regs_scratch[..reg_count])
}

#[inline(always)]
fn set_reg_value(regs: &mut [f64], reg: usize, value: f64) {
    regs[reg] = value;
}

#[inline(always)]
fn read_reg_value(regs: &[f64], reg: usize) -> f64 {
    regs[reg]
}

fn eval_linear_solve_component(regs: &[f64], op: LinearOp) -> Result<f64, CompileError> {
    let LinearOp::LinearSolveComponent {
        matrix_start,
        rhs_start,
        n,
        component,
        ..
    } = op
    else {
        return Err(CompileError::Backend(
            "linear solve component evaluator received a non-linear-solve op".to_string(),
        ));
    };
    if n == 0 {
        return Err(CompileError::Backend(
            "linear solve component has zero size".to_string(),
        ));
    }
    if component >= n {
        return Err(CompileError::Backend(format!(
            "linear solve component index {component} is out of range for size {n}"
        )));
    }
    let matrix_len = checked_square_len(n, "linear solve matrix")?;
    let mut matrix = zeroed_f64_vec(matrix_len, "linear solve matrix")?;
    let mut rhs = zeroed_f64_vec(n, "linear solve rhs")?;
    for row in 0..n {
        rhs[row] = read_reg_value(regs, rhs_start as usize + row);
        for col in 0..n {
            matrix[row * n + col] = read_reg_value(regs, matrix_start as usize + row * n + col);
        }
    }
    solve_dense_component(&mut matrix, &mut rhs, n, component)
}

fn zeroed_f64_vec(len: usize, kind: &'static str) -> Result<Vec<f64>, CompileError> {
    let mut values = checked_vec_with_capacity(len, kind)?;
    values.resize(len, 0.0);
    Ok(values)
}

fn solve_dense_component(
    matrix: &mut [f64],
    rhs: &mut [f64],
    n: usize,
    component: usize,
) -> Result<f64, CompileError> {
    for col in 0..n {
        let Some(pivot) = pivot_row(matrix, n, col) else {
            return Ok(f64::NAN);
        };
        swap_dense_rows(matrix, rhs, n, col, pivot);
        let pivot_value = matrix[col * n + col];
        if pivot_value.abs() <= f64::EPSILON {
            return Ok(f64::NAN);
        }
        for row in col + 1..n {
            let factor = matrix[row * n + col] / pivot_value;
            matrix[row * n + col] = 0.0;
            for entry in col + 1..n {
                matrix[row * n + entry] -= factor * matrix[col * n + entry];
            }
            rhs[row] -= factor * rhs[col];
        }
    }

    let mut solution = zeroed_f64_vec(n, "linear solve solution")?;
    for row in (0..n).rev() {
        let tail = ((row + 1)..n)
            .map(|col| matrix[row * n + col] * solution[col])
            .sum::<f64>();
        solution[row] = (rhs[row] - tail) / matrix[row * n + row];
    }
    Ok(solution[component])
}

fn pivot_row(matrix: &[f64], n: usize, col: usize) -> Option<usize> {
    (col..n).max_by(|&lhs, &rhs| {
        matrix[lhs * n + col]
            .abs()
            .total_cmp(&matrix[rhs * n + col].abs())
    })
}

fn swap_dense_rows(matrix: &mut [f64], rhs: &mut [f64], n: usize, lhs: usize, rhs_row: usize) {
    if lhs == rhs_row {
        return;
    }
    for col in 0..n {
        matrix.swap(lhs * n + col, rhs_row * n + col);
    }
    rhs.swap(lhs, rhs_row);
}
