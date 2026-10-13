//! Exact Integer provenance through the original ordered stage operations.
use super::*;
use register_sources::{RegisterSource, RegisterSourcePiece, RegisterSources};

pub(super) struct StageFlow<'a> {
    pub(super) inputs: &'a mut TypedInputs,
    pub(super) sinks: BTreeMap<usize, NativeIntegerSource>,
    pub(super) stored_output: usize,
    pub(super) bindings: NativeIntegerBindings,
}
impl StageFlow<'_> {
    pub(super) fn program(
        &mut self,
        operations: &[LinearOp],
    ) -> Result<(), NativeEvaluationRefusal> {
        let mut registers = RegisterSources::default();
        for (position, op) in operations.iter().enumerate() {
            self.operation(&mut registers, position, op)?;
        }
        self.bindings.results = registers.cells;
        self.bindings.read_results = registers.read_results;
        Ok(())
    }
    fn operation(
        &mut self,
        registers: &mut RegisterSources,
        position: usize,
        op: &LinearOp,
    ) -> Result<(), NativeEvaluationRefusal> {
        let bound = self.bound_reads(position, op, registers)?;
        let reads = match crate::linear_op::op_read_register_ranges(op) {
            Some(reads) => reads,
            None if !registers.has_integer_sources(self.inputs) => Vec::new(),
            None => return Err(NativeEvaluationRefusal::UnresolvedTypedRead),
        };
        for read in reads {
            let selected =
                NativeInputSelection::checked(read.start as usize, read.count, read.stride)
                    .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
            registers
                .read(selected, &bound, self.inputs)
                .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
        }
        registers.define(op);
        if let LinearOp::PureCall {
            dst_start, site, ..
        } = op
        {
            registers
                .call_results(position, *dst_start, site)
                .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
        }
        Ok(())
    }
    fn bound_reads(
        &mut self,
        position: usize,
        op: &LinearOp,
        registers: &RegisterSources,
    ) -> Result<Vec<NativeInputSelection>, NativeEvaluationRefusal> {
        let mut bound = Vec::new();
        match op {
            LinearOp::PureCall {
                input_starts, site, ..
            } => {
                return self.bound_call(position, input_starts, site, registers);
            }
            LinearOp::StoreOutput { src } => {
                if self.exact_sink(self.stored_output, *src, registers) {
                    bound.push(NativeInputSelection {
                        start: *src as usize,
                        count: 1,
                        stride: 1,
                    });
                }
                self.stored_output = self
                    .stored_output
                    .checked_add(1)
                    .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                bound = self.bound_output_range(*start, *count, *stride, registers)?;
            }
            _ => {}
        }
        Ok(bound)
    }
    fn bound_output_range(
        &mut self,
        start: Reg,
        count: usize,
        stride: usize,
        registers: &RegisterSources,
    ) -> Result<Vec<NativeInputSelection>, NativeEvaluationRefusal> {
        let mut bound = Vec::new();
        let end = self
            .stored_output
            .checked_add(count)
            .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
        let sinks = self.sinks.range(self.stored_output..end);
        if stride == 0 {
            if count != 0
                && sinks.clone().count() == count
                && sinks
                    .clone()
                    .all(|(&ordinal, _)| self.exact_sink(ordinal, start, registers))
            {
                bound.push(NativeInputSelection {
                    start: start as usize,
                    count: 1,
                    stride: 1,
                });
            }
            self.stored_output = end;
            return Ok(bound);
        }
        for (&ordinal, _) in sinks {
            let register = (ordinal - self.stored_output)
                .checked_mul(stride)
                .and_then(|offset| Reg::try_from(offset).ok())
                .and_then(|offset| start.checked_add(offset))
                .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
            if self.exact_sink(ordinal, register, registers) {
                bound.push(NativeInputSelection {
                    start: register as usize,
                    count: 1,
                    stride: 1,
                });
            }
        }
        self.stored_output = end;
        Ok(bound)
    }

    fn bound_call(
        &mut self,
        position: usize,
        input_starts: &[Reg],
        site: &crate::SolvePureCallSite,
        registers: &RegisterSources,
    ) -> Result<Vec<NativeInputSelection>, NativeEvaluationRefusal> {
        let mut bound = Vec::new();
        let mut flat = 0u32;
        for (&start, value) in input_starts.iter().zip(site.inputs()) {
            let count = value.scalar_count();
            let selected = NativeInputSelection::checked(start as usize, count as usize, 1)
                .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
            for piece in registers
                .pieces(selected)
                .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?
            {
                self.bound_call_piece(position, flat, start, value, piece, &mut bound)?;
            }
            flat = flat
                .checked_add(count)
                .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
        }
        Ok(bound)
    }

    fn bound_call_piece(
        &mut self,
        position: usize,
        flat: u32,
        start: Reg,
        value: &crate::SolveValueType,
        piece: RegisterSourcePiece,
        bound: &mut Vec<NativeInputSelection>,
    ) -> Result<(), NativeEvaluationRefusal> {
        let RegisterSource::Input { p_start } = piece.source.as_ref() else {
            return Ok(());
        };
        let first = p_start
            .checked_add(piece.source_offset)
            .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
        let p_selection = NativeInputSelection::checked(first, piece.selected.count, 1)
            .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
        for run in self
            .inputs
            .runs
            .iter()
            .filter(|run| run.lane() == NativeOutputLane::Integer)
        {
            let Some(p_cells) = p_selection.clipped(run.p_range()) else {
                continue;
            };
            let register = piece.selected.start + (p_cells.start - first);
            bound.push(NativeInputSelection {
                start: register,
                count: p_cells.count,
                stride: 1,
            });
            if !matches!(value.element_type(), crate::SolveScalarType::Integer(_)) {
                // The authored coercion of a Real argument reads the Real view.
                self.inputs.reads.push(NativeInputCheck {
                    selected: p_cells,
                    bound: Vec::new(),
                });
            }
            if matches!(value.element_type(), crate::SolveScalarType::Integer(_)) {
                let first_cell = flat
                    .checked_add(
                        u32::try_from(register - start as usize)
                            .map_err(|_| NativeEvaluationRefusal::UnresolvedTypedRead)?,
                    )
                    .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?;
                self.bindings.inputs.push(NativeIntegerArgumentRun {
                    operation: position,
                    first_cell,
                    count: u32::try_from(p_cells.count)
                        .map_err(|_| NativeEvaluationRefusal::UnresolvedTypedRead)?,
                    lane_offset: run
                        .offset_at(p_cells.start)
                        .ok_or(NativeEvaluationRefusal::UnresolvedTypedRead)?,
                });
            }
        }
        Ok(())
    }

    fn exact_sink(&self, ordinal: usize, register: Reg, registers: &RegisterSources) -> bool {
        let Some(pieces) = registers.pieces(NativeInputSelection {
            start: register as usize,
            count: 1,
            stride: 1,
        }) else {
            return false;
        };
        let [piece] = pieces.as_slice() else {
            return false;
        };
        match (self.sinks.get(&ordinal), piece.source.as_ref()) {
            (
                Some(NativeIntegerSource::Input { lane_offset }),
                RegisterSource::Input { p_start },
            ) => {
                p_start
                    .checked_add(piece.source_offset)
                    .and_then(|p| self.inputs.integer_lane(p))
                    == Some(*lane_offset)
            }
            (
                Some(NativeIntegerSource::CallCell { operation, cell }),
                RegisterSource::Result {
                    operation: source,
                    first_cell,
                },
            ) => {
                source == operation
                    && (*first_cell as usize).checked_add(piece.source_offset)
                        == Some(*cell as usize)
            }
            _ => false,
        }
    }
}
