//! Call frames: where a callee's inputs, outputs and scratch live while it runs.
use super::{
    CallFrame, Callees, CellRange, Demand, DestBuffer, FramePlan, PlacedResults, PointerCell,
    TypedCallCompileError, bytes,
};
use rumoca_ir_solve as solve;
use std::collections::BTreeMap;

impl FramePlan {
    /// Place the result registers of a call to a callee that leaves its outputs
    /// in the head of its scratch.
    ///
    /// The callee writes each result straight into its register: the
    /// registers form one run, in output order, directly below the call's
    /// private storage, and the callee's scratch starts at the run, so the
    /// results cost no copy. A large output the callee returns by reference
    /// has an eight-byte cell in the run holding the address of its value, and
    /// a buffer the caller passes in for the callee to build it in: the buffer
    /// of an output the caller returns whole is that output's own buffer.
    /// Returns where the run begins, or `None` for a callee that publishes to
    /// its output buffer.
    pub(super) fn plan_call_results(
        &mut self,
        program: &solve::TypedProgram,
        call: (solve::SolvePureCallOwnerId, &[solve::SolveRegisterId]),
        destinations: &[solve::SolveRegisterId],
        callees: &Callees<'_>,
    ) -> Result<Option<PlacedResults>, TypedCallCompileError> {
        let (owner, arguments) = call;
        let callee = self.callee(owner, destinations, callees)?.1;
        if !callee.outputs_in_scratch {
            return Ok(None);
        }
        if callee.output_slots.len() != destinations.len() {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let mut sizes = Vec::with_capacity(destinations.len());
        for (ordinal, destination) in destinations.iter().enumerate() {
            let size = bytes(&program.register_types()[destination.index()])?;
            if callee.slots[callee.output_slots[ordinal]].bytes != size {
                return Err(TypedCallCompileError::SiteMismatch);
            }
            sizes.push(size);
        }
        self.activate_forwarded_results(callee, arguments, destinations);
        // The buffers come first: the callee's scratch starts at the run and
        // extends above it.
        let mut buffers = Vec::new();
        let mut built = BTreeMap::new();
        for (ordinal, destination) in destinations.iter().enumerate() {
            // A result nothing reads is not built, so it has no buffer.
            let Some(output) = callee
                .ref_outputs
                .get(&callee.output_slots[ordinal])
                .filter(|_| self.liveness.needs(*destination))
            else {
                continue;
            };
            let buffer = match self.register_hints.get(&destination.index()) {
                Some((_, hint)) => hint.buffer,
                None => self.recycled_register(sizes[ordinal])?,
            };
            built.insert(ordinal, buffer);
            buffers.push(DestBuffer {
                cell: output.dest_cell,
                buffer,
            });
        }
        let mut start = self.cursor;
        for (ordinal, destination) in destinations.iter().enumerate() {
            let size = sizes[ordinal];
            let reference = callee
                .ref_outputs
                .contains_key(&callee.output_slots[ordinal]);
            let cell = if reference {
                self.keep(8)?
            } else {
                self.fresh_register(size)?
            };
            if ordinal == 0 {
                start = cell.offset;
            }
            if reference {
                let mut sources =
                    self.aliased_arguments(callee, callee.output_slots[ordinal], arguments);
                let named = sources.clone();
                sources.extend(built.get(&ordinal).copied());
                let range = CellRange::through_scratch_cell(cell.offset, size)?;
                self.push_register(*destination, range)?;
                self.register_reusable.push(false);
                self.lifetimes.reference(*destination, range, &sources);
                self.forward_call_result(*destination, &named);
            } else {
                self.lifetimes.own(cell);
                self.push_register(*destination, cell)?;
                self.register_reusable.push(true);
            }
        }
        Ok(Some(PlacedResults { start, buffers }))
    }

    /// The ranges of the arguments the by-reference output `slot` of `callee`
    /// may return unchanged.
    pub(super) fn aliased_arguments(
        &self,
        callee: &Self,
        slot: usize,
        arguments: &[solve::SolveRegisterId],
    ) -> Vec<CellRange> {
        callee
            .output_aliases
            .get(&slot)
            .into_iter()
            .flatten()
            .filter_map(|input| arguments.get(*input))
            .map(|argument| self.registers[argument.index()])
            .collect()
    }

    /// An enclosing conditional may forward the address of a call result that
    /// is built in its hinted buffer when every value the call may return
    /// instead outlives the region.
    fn forward_call_result(&mut self, destination: solve::SolveRegisterId, sources: &[CellRange]) {
        let Some((output, hint)) = self.register_hints.get(&destination.index()) else {
            return;
        };
        if sources
            .iter()
            .all(|source| super::outputs::parent_owned(*source, hint.floor))
        {
            self.forwards.insert(*output, sources.to_vec());
        }
    }

    pub(super) fn plan_call_frame(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        destinations: &[solve::SolveRegisterId],
        base: u32,
        callees: &Callees<'_>,
    ) -> Result<(Vec<Self>, Option<CallFrame>, u32), TypedCallCompileError> {
        let (unit, callee) = self.callee(owner, destinations, callees)?;
        let mut top = base;
        let frame = if let Some(placed) = self.call_results.take() {
            // The callee's scratch starts at its results, below its input tuple.
            let scratch = CellRange {
                base: 2,
                offset: placed.start,
                bytes: callee.scratch_bytes,
            };
            top = top.max(scratch.offset + scratch.bytes);
            CallFrame {
                unit,
                input: alloc_range(&mut top, callee.input_bytes)?,
                output: CellRange {
                    bytes: 0,
                    ..scratch
                },
                scratch,
                results_in_place: true,
                buffers: placed.buffers,
            }
        } else {
            CallFrame {
                unit,
                input: alloc_range(&mut top, callee.input_bytes)?,
                output: alloc_range(&mut top, callee.output_bytes)?,
                scratch: alloc_range(&mut top, callee.scratch_bytes)?,
                results_in_place: false,
                buffers: Vec::new(),
            }
        };
        self.adopt(u64::from(callee.input_bytes))?;
        if !frame.results_in_place {
            self.adopt(u64::from(callee.output_bytes))?;
        }
        self.adopt(callee.unshared_bytes)?;
        Ok((Vec::new(), Some(frame), top))
    }
}

fn alloc_range(cursor: &mut u32, count: u32) -> Result<CellRange, TypedCallCompileError> {
    super::alloc(2, cursor, count)
}

impl FramePlan {
    /// The unit a call of `owner` runs, built for the outputs of `destinations`
    /// that running operations read, and its plan.
    fn callee<'a>(
        &self,
        owner: solve::SolvePureCallOwnerId,
        destinations: &[solve::SolveRegisterId],
        callees: &'a Callees<'_>,
    ) -> Result<(usize, &'a FramePlan), TypedCallCompileError> {
        let demand = Demand::of_destinations(destinations, self.liveness.needed());
        let unit = callees.unit(owner, demand)?;
        Ok((unit, callees.plan(unit)?))
    }
}

impl FramePlan {
    /// Record which inputs each by-reference output may name in its result
    /// cell: the input it returns unchanged, or what its hinted reference
    /// result may name. Every other value is built in the caller's buffer.
    pub(super) fn summarize_output_aliases(&mut self, program: &solve::TypedProgram) {
        let mut summary = BTreeMap::new();
        for slot in self.ref_outputs.keys() {
            if self.liveness.drops(*slot) {
                continue;
            }
            let Some(source) =
                super::outputs::completed_return(program, program.slots()[*slot].id())
            else {
                continue;
            };
            let value = self.registers[source.index()];
            let named = match value.pointer_cell() {
                Some(PointerCell::Input(_)) if value != self.slots[*slot] => vec![value],
                Some(PointerCell::Scratch(_)) => self
                    .register_hints
                    .get(&source.index())
                    .and_then(|(ordinal, _)| self.forwards.get(ordinal))
                    .cloned()
                    .unwrap_or_default(),
                _ => Vec::new(),
            };
            let inputs = named
                .iter()
                .filter_map(|range| match range.pointer_cell() {
                    Some(PointerCell::Input(cell)) => self
                        .input_cells
                        .iter()
                        .position(|input| input.by_reference && input.offset == cell),
                    _ => None,
                })
                .collect();
            summary.insert(*slot, inputs);
        }
        self.output_aliases = summary;
    }
}
