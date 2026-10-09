//! Exact Integer register provenance through one canonical native stage.

use super::*;

/// Register provenance through one top-level stage program.
pub(super) struct StageFlow<'a> {
    pub(super) views: &'a BTreeMap<usize, usize>,
    pub(super) lanes: &'a [NativeInputLane],
    /// The exact source the stage's Integer derived output publishes.
    pub(super) sinks: BTreeMap<usize, NativeIntegerSource>,
    pub(super) stored_output: usize,
    pub(super) checked: &'a mut Vec<bool>,
    /// View element loads analysed here, per lane.
    pub(super) loads: Vec<usize>,
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
        self.bindings.unread_results = registers
            .cells
            .difference(&registers.read_results)
            .copied()
            .collect();
        Ok(())
    }

    /// Follow one operation: its unbound reads, then what its writes hold.
    fn operation(
        &mut self,
        registers: &mut RegisterSources,
        position: usize,
        op: &LinearOp,
    ) -> Result<(), NativeEvaluationRefusal> {
        let bound = self.bound_reads(position, op, registers);
        // An unknown read set could read any held Integer value unchecked.
        let reads = match crate::linear_op::op_read_registers(op) {
            Some(reads) => reads,
            None if registers.views.is_empty() && registers.results.is_empty() => Vec::new(),
            None => return Err(NativeEvaluationRefusal::UnresolvedTypedRead),
        };
        for register in reads
            .into_iter()
            .filter(|register| !bound.contains(register))
        {
            registers.read(register, self.checked);
        }
        registers.define(op);
        let loads = view_elements(op)
            .into_iter()
            .filter_map(|(register, slot)| Some((register, *self.views.get(&slot)?)))
            .collect::<Vec<_>>();
        for (register, ordinal) in loads {
            self.loads[ordinal] += 1;
            registers.views.insert(register, ordinal);
        }
        if let LinearOp::PureCall {
            dst_start, site, ..
        } = op
        {
            registers.call_results(position, *dst_start, site);
        }
        Ok(())
    }

    /// The registers `op` reads through a bound sink: Integer argument cells
    /// holding an Integer input view (recorded as lane bindings), and the
    /// stored register of the stage's lane-published Integer output.
    fn bound_reads(
        &mut self,
        position: usize,
        op: &LinearOp,
        registers: &RegisterSources,
    ) -> BTreeSet<Reg> {
        let views = &registers.views;
        let mut bound = BTreeSet::new();
        match op {
            LinearOp::PureCall {
                input_starts, site, ..
            } => {
                // A Real argument cell fed by the Real view of an Integer input is
                // the authored Integer-to-Real coercion of the call (MLS 3.7
                // section 10.5 coerces an Integer actual to a Real formal), so
                // it rounds per IEEE 754 and is not a checked read; the
                // Integer cells of the same call still read the lane.
                bound.extend(
                    argument_cells(input_starts, site)
                        .map(|(_, register, _)| register)
                        .filter(|register| views.contains_key(register)),
                );
                let cells = argument_cells(input_starts, site)
                    .filter(|&(_, _, integer)| integer)
                    .filter_map(|(flat, register, _)| {
                        Some((flat, register, *views.get(&register)?))
                    })
                    .collect::<Vec<_>>();
                for (flat, _, ordinal) in cells {
                    self.bindings
                        .inputs
                        .insert((position, flat), self.lanes[ordinal].lane_offset);
                }
            }
            LinearOp::StoreOutput { src } => {
                if self.exact_sink(self.stored_output, *src, registers) {
                    bound.insert(*src);
                }
                self.stored_output += 1;
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                bound.extend(self.bound_range(*start, *count, *stride, registers));
                self.stored_output += count;
            }
            _ => {}
        }
        bound
    }

    fn bound_range(
        &self,
        start: Reg,
        count: usize,
        stride: usize,
        registers: &RegisterSources,
    ) -> BTreeSet<Reg> {
        let sinks = self
            .sinks
            .range(self.stored_output..self.stored_output + count);
        if stride == 0 {
            // One source register is read by every broadcast output. An
            // unbound output of any kind keeps that register checked.
            let exact = count > 0
                && sinks.clone().count() == count
                && sinks
                    .clone()
                    .all(|(&ordinal, _)| self.exact_sink(ordinal, start, registers));
            return if exact {
                BTreeSet::from([start])
            } else {
                BTreeSet::new()
            };
        }
        sinks
            .filter_map(|(&ordinal, _)| {
                let register = (ordinal - self.stored_output)
                    .checked_mul(stride)
                    .and_then(|offset| Reg::try_from(offset).ok())
                    .and_then(|offset| start.checked_add(offset))?;
                self.exact_sink(ordinal, register, registers)
                    .then_some(register)
            })
            .collect()
    }

    fn exact_sink(&self, ordinal: usize, register: Reg, registers: &RegisterSources) -> bool {
        match self.sinks.get(&ordinal).copied() {
            Some(NativeIntegerSource::Input { lane_offset }) => registers
                .views
                .get(&register)
                .is_some_and(|&ordinal| self.lanes[ordinal].lane_offset == lane_offset),
            Some(NativeIntegerSource::CallCell { operation, cell }) => {
                registers.results.get(&register) == Some(&(operation, cell))
            }
            _ => false,
        }
    }
}
