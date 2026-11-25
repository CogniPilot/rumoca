//! A runtime frame for one executed loop pass or conditional arm. Local slots
//! follow its working registers, so copying from a capture or carried value
//! reuses the ordinary register-move pullback.
use super::*;
use rumoca_ir_solve::FoldInitialSource;

#[derive(Clone)]
pub(super) struct Frame {
    ops: Vec<LinearOp>,
    scratch: ReverseScratch,
    carried: Range<usize>,
    captures: Range<usize>,
    outputs: Vec<(usize, Reg)>,
}

impl Frame {
    pub(super) fn new(
        width: usize,
        carried: &[f64],
        captures: &[f64],
    ) -> Result<Self, EvalSolveError> {
        let carried_range = width..width.checked_add(carried.len()).ok_or_else(capacity)?;
        let captures_range = carried_range.end
            ..carried_range
                .end
                .checked_add(captures.len())
                .ok_or_else(capacity)?;
        Reg::try_from(captures_range.end).map_err(|_| capacity())?;
        let mut scratch = ReverseScratch::default();
        scratch.regs.resize(width, 0.);
        scratch.regs.extend_from_slice(carried);
        scratch.regs.extend_from_slice(captures);
        Ok(Self {
            ops: Vec::new(),
            scratch,
            carried: carried_range,
            captures: captures_range,
            outputs: Vec::new(),
        })
    }

    fn reserve_slots(&mut self, count: usize) -> Result<Reg, EvalSolveError> {
        let start = self.scratch.regs.len();
        let end = start.checked_add(count).ok_or_else(capacity)?;
        Reg::try_from(end).map_err(|_| capacity())?;
        self.scratch.regs.resize(end, 0.);
        Ok(start as Reg)
    }

    fn emit(&mut self, inputs: &ReverseInputs<'_>, op: LinearOp) -> Result<(), EvalSolveError> {
        super::super::forward_instruction(
            self.ops.len(),
            &op,
            inputs,
            &mut self.scratch.regs,
            &mut self.scratch.history,
        )?;
        self.ops.push(op);
        Ok(())
    }

    fn store(&mut self, src: Reg, values: &mut Vec<f64>) {
        values.push(reg(&self.scratch.regs, src));
        self.outputs.push((self.ops.len(), src));
        self.ops.push(LinearOp::StoreOutput { src });
    }

    pub(super) fn run(
        &mut self,
        inputs: &ReverseInputs<'_>,
        row: &[LinearOp],
        indices: &[i64],
        mask: Option<&[bool]>,
    ) -> Result<Vec<f64>, EvalSolveError> {
        let mut values = Vec::new();
        for (i, op) in row.iter().enumerate() {
            if mask.is_none_or(|active| active[i]) {
                self.step(inputs, op, indices, &mut values)?;
            }
        }
        Ok(values)
    }

    fn step(
        &mut self,
        inputs: &ReverseInputs<'_>,
        op: &LinearOp,
        indices: &[i64],
        values: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        use LinearOp::*;
        let local = match op {
            LoadFoldCarried { dst, index } => Move {
                dst: *dst,
                src: (self.carried.start + index) as Reg,
            },
            LoadFoldCapture { dst, index } | LoadFunctionConditionalCapture { dst, index } => {
                Move {
                    dst: *dst,
                    src: (self.captures.start + index) as Reg,
                }
            }
            LoadFoldIndex { dst, dimension } => Const {
                dst: *dst,
                value: indices[*dimension] as f64,
            },
            LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            }
            | LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let start = if matches!(op, LoadIndexedFoldCarried { .. }) {
                    self.carried.start
                } else {
                    self.captures.start
                };
                let src = indexed::source(
                    (start + base) as Reg,
                    *stride,
                    dimensions,
                    indices,
                    &self.scratch.regs,
                )?;
                Move { dst: *dst, src }
            }
            LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => {
                for offset in 0..*count {
                    self.emit(
                        inputs,
                        Move {
                            dst: *dst_start + offset as Reg,
                            src: (self.captures.start + index_start + offset) as Reg,
                        },
                    )?;
                }
                return Ok(());
            }
            StoreOutput { src } => {
                self.store(*src, values);
                return Ok(());
            }
            StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for j in 0..*count {
                    self.store(*start + (j * stride) as Reg, values);
                }
                return Ok(());
            }
            StoreOutputFunctionFold { .. } => return self.nested_output(inputs, op, values),
            StoreOutputFoldTensorUpdate { .. } => return self.tensor_output(op, values),
            scalar_ops!() | tensor_ops!() | region_ops!() | PureCall { .. } => op.clone(),
            outside_first_order!() => return Err(unsupported_reverse_op(op)),
        };
        self.emit(inputs, local)
    }

    fn nested_output(
        &mut self,
        inputs: &ReverseInputs<'_>,
        op: &LinearOp,
        values: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        let LinearOp::StoreOutputFunctionFold {
            initial,
            capture_start,
            program,
            result_base,
            count,
            condition,
            nested_when_true,
        } = op
        else {
            unreachable!()
        };
        if condition.is_some_and(|r| (reg(&self.scratch.regs, r) != 0.) != *nested_when_true) {
            let source = self.carried.start + values.len();
            for j in 0..*count {
                self.store((source + j) as Reg, values);
            }
            return Ok(());
        }
        let initial_start = self.reserve_slots(program.carried_count)?;
        let mut dst = initial_start;
        for source in initial {
            let (start, n) = match *source {
                FoldInitialSource::Registers { start, count } => (start, count),
                FoldInitialSource::ParentCarried { base, count } => {
                    ((self.carried.start + base) as Reg, count)
                }
            };
            for j in 0..n {
                self.emit(
                    inputs,
                    LinearOp::Move {
                        dst,
                        src: start + j as Reg,
                    },
                )?;
                dst += 1;
            }
        }
        let dst_start = self.reserve_slots(program.carried_count)?;
        self.emit(
            inputs,
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start: *capture_start,
                program: program.clone(),
            },
        )?;
        for j in 0..*count {
            self.store(dst_start + (result_base + j) as Reg, values);
        }
        Ok(())
    }

    fn tensor_output(
        &mut self,
        op: &LinearOp,
        values: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        let LinearOp::StoreOutputFoldTensorUpdate {
            source_base,
            source_stride,
            dimensions,
            updates,
            nodes,
            result,
            lanes: 1,
        } = op
        else {
            return Err(unsupported_reverse_op(op));
        };
        let count = dimensions.iter().map(|&n| n as usize).product();
        let mut sources = Vec::with_capacity(nodes.len() + 1);
        let mut offsets = Vec::with_capacity(updates.len());
        for element in 0..count {
            offsets.clear();
            for update in updates {
                offsets.push(crate::tensor_update_register_value_offset(
                    dimensions,
                    &update.subscripts,
                    element,
                    |r| Ok::<_, EvalSolveError>(reg(&self.scratch.regs, r)),
                )?);
            }
            sources.clear();
            sources.push((self.carried.start + source_base + source_stride * element) as Reg);
            for node in nodes {
                sources.push(crate::fold_tensor_node(
                    &self.scratch.regs,
                    updates,
                    &offsets,
                    &sources,
                    0,
                    node,
                    |r| r,
                ));
            }
            self.store(sources[*result as usize], values);
        }
        Ok(())
    }

    pub(super) fn reverse(
        mut self,
        context: RowEvalContext<'_>,
        weights: &[Weight],
        cot: &mut ReverseCotangents<'_>,
    ) -> Result<(Vec<Weight>, Vec<Weight>), EvalSolveError> {
        if weights.len() != self.outputs.len() {
            return Err(unsupported(
                "structured result width differs from its cotangent",
            ));
        }
        let width = self.scratch.regs.len();
        self.scratch.adj.resize(width, 0.);
        self.scratch.untaken.resize(width, false);
        for ((i, src), &w) in self.outputs.into_iter().zip(weights) {
            self.scratch.history.output(i, src, w.value, w.untaken);
        }
        reverse_row_adjoints(&self.ops, context, &mut self.scratch, cot)?;
        let read = |range: Range<usize>| {
            range
                .map(|i| Weight {
                    value: self.scratch.adj[i],
                    untaken: self.scratch.untaken[i],
                })
                .collect()
        };
        Ok((read(self.carried), read(self.captures)))
    }
}

fn capacity() -> EvalSolveError {
    unsupported("structured reverse frame exceeds the register address space")
}
