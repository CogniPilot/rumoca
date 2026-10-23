use super::{
    CompileError, ProgramLayout, ProgramLowerer, StorageBase, ValueLocation, checked_cells,
    create_tape,
    observation::ObservationLowering,
    placement::{self, PhysicalCells, PlacedSlot},
};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{Block, InstBuilder, Value, types};
use cranelift_module::Module;
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve as solve;

struct FoldFrames {
    carried_cells: u32,
    capture_cells: u32,
    first: Value,
    second: Value,
    predicate_input: Option<Value>,
    predicate: Option<Value>,
}

impl ProgramLowerer<'_, '_> {
    pub(super) fn lower_conditional(
        &mut self,
        condition: solve::SolveRegisterId,
        captures: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        if_true: &solve::SolveProgramRegion,
        if_false: &solve::SolveProgramRegion,
    ) -> Result<(), CompileError> {
        if self.observation.is_none()
            && placement::placeable(if_true)
            && placement::placeable(if_false)
        {
            return self.lower_placed_conditional(
                condition,
                captures,
                destinations,
                if_true,
                if_false,
            );
        }
        let condition = self.register(condition)?.clone();
        let input_cells = self.register_cell_count(captures, "typed conditional captures")?;
        let output_cells = self.register_cell_count(destinations, "typed conditional results")?;
        let input = create_tape(self.builder, self.pointer_type, input_cells)?;
        let output = create_tape(self.builder, self.pointer_type, output_cells)?;
        self.pack_registers(captures, input)?;
        let zero = self.builder.ins().iconst(types::I64, 0);
        let condition = self.load_scalar(&condition, zero)?;
        let selected = self.builder.ins().icmp_imm(IntCC::NotEqual, condition, 0);
        let true_block = self.builder.create_block();
        let false_block = self.builder.create_block();
        let continuation = self.builder.create_block();
        self.builder
            .ins()
            .brif(selected, true_block, &[], false_block, &[]);

        self.builder.switch_to_block(true_block);
        self.builder.seal_block(true_block);
        self.lower_region(if_true, input, output)?;
        self.builder.ins().jump(continuation, &[]);

        self.builder.switch_to_block(false_block);
        self.builder.seal_block(false_block);
        self.lower_region(if_false, input, output)?;
        self.builder.ins().jump(continuation, &[]);

        self.builder.switch_to_block(continuation);
        self.builder.seal_block(continuation);
        self.unpack_registers(output, destinations)
    }

    /// A conditional whose branches read the captures where they live, and
    /// write the destinations directly when [`placement::placement_is_safe`]
    /// admits it; a branch it refuses writes a private tape that is unpacked
    /// after the branch, as the packed form does.
    fn lower_placed_conditional(
        &mut self,
        condition: solve::SolveRegisterId,
        captures: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        if_true: &solve::SolveProgramRegion,
        if_false: &solve::SolveProgramRegion,
    ) -> Result<(), CompileError> {
        let condition = self.register(condition)?.clone();
        let inputs = self.placed_slots(captures)?;
        let outputs = self.placed_slots(destinations)?;
        let input_cells = placement::slot_cells(&inputs);
        let output_cells = placement::slot_cells(&outputs);
        let branches = [if_true, if_false]
            .into_iter()
            .map(|region| {
                let (layout, direct) = placed_layout(region, &input_cells, &output_cells)?;
                Ok((region, layout, direct))
            })
            .collect::<Result<Vec<_>, CompileError>>()?;
        let output_tape = if branches.iter().all(|(_, _, direct)| *direct) {
            None
        } else {
            let cells = self.register_cell_count(destinations, "typed conditional results")?;
            Some(create_tape(self.builder, self.pointer_type, cells)?)
        };
        let zero = self.builder.ins().iconst(types::I64, 0);
        let condition = self.load_scalar(&condition, zero)?;
        let selected = self.builder.ins().icmp_imm(IntCC::NotEqual, condition, 0);
        let true_block = self.builder.create_block();
        let false_block = self.builder.create_block();
        let continuation = self.builder.create_block();
        self.builder
            .ins()
            .brif(selected, true_block, &[], false_block, &[]);
        for ((region, layout, direct), block) in branches.iter().zip([true_block, false_block]) {
            self.builder.switch_to_block(block);
            self.builder.seal_block(block);
            match output_tape.filter(|_| !*direct) {
                None => self.lower_placed_region(
                    region,
                    layout,
                    (self.tape, self.tape),
                    (Some(inputs.clone()), Some(outputs.clone())),
                )?,
                Some(tape) => {
                    self.lower_placed_region(
                        region,
                        layout,
                        (self.tape, tape),
                        (Some(inputs.clone()), None),
                    )?;
                    self.unpack_registers(tape, destinations)?;
                }
            }
            self.builder.ins().jump(continuation, &[]);
        }
        self.builder.switch_to_block(continuation);
        self.builder.seal_block(continuation);
        Ok(())
    }

    pub(super) fn lower_fold(
        &mut self,
        domain: &StructuredIndexDomain,
        initial: &[solve::SolveRegisterId],
        captures: &[solve::SolveRegisterId],
        state_captures: &[solve::SolveAssignedStateId],
        destinations: &[solve::SolveRegisterId],
        (transition, continuation): (
            &solve::SolveProgramRegion,
            Option<&solve::SolveProgramRegion>,
        ),
    ) -> Result<(), CompileError> {
        if self.lower_fold_in_place(
            domain,
            initial,
            captures,
            state_captures,
            destinations,
            (transition, continuation),
        )? {
            return Ok(());
        }
        let FoldFrames {
            carried_cells,
            capture_cells,
            first,
            second,
            predicate_input,
            predicate,
        } = self.prepare_fold_frames(
            domain,
            initial,
            captures,
            state_captures,
            (transition, continuation),
        )?;

        let count = domain
            .scalar_count()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let extents = domain
            .extents()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let strides = domain
            .ordinal_strides()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let count = i64::try_from(count)
            .map_err(|_| CompileError::Backend("typed fold domain exceeds i64".into()))?;
        let binder_cells = u32::try_from(domain.binders.len())
            .map_err(|_| CompileError::Backend("typed fold binder count overflows".into()))?;
        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let exit = self.builder.create_block();
        self.builder.append_block_param(header, types::I64);
        self.builder.append_block_param(header, self.pointer_type);
        self.builder.append_block_param(header, self.pointer_type);
        self.builder.append_block_param(exit, self.pointer_type);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder
            .ins()
            .jump(header, &[zero.into(), first.into(), second.into()]);
        self.builder.switch_to_block(header);
        let parameters = self.builder.block_params(header);
        let ordinal = parameters[0];
        let current = parameters[1];
        let next_frame = parameters[2];
        let in_range = self
            .builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, ordinal, count);
        self.builder
            .ins()
            .brif(in_range, body, &[], exit, &[current.into()]);

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);
        self.carrier_event(
            super::super::observation_carrier::LOOP_ENTER,
            current,
            0,
            Some(ordinal),
        )?;
        self.store_domain_binders(
            domain,
            &extents,
            &strides,
            ordinal,
            current,
            carried_cells + capture_cells,
        )?;
        // An early-exit fold tests its predicate on the current tuple and the
        // binders of the pass about to run, and leaves with that tuple when it
        // is false.
        if let (Some(continuation), Some(predicate)) = (continuation, predicate) {
            self.lower_fold_continuation(
                continuation,
                (current, predicate_input, predicate),
                (carried_cells, capture_cells, binder_cells),
                exit,
            )?;
        }
        self.lower_region(transition, current, next_frame)?;
        self.carrier_event(
            super::super::observation_carrier::LOOP_EXIT,
            current,
            0,
            None,
        )?;
        let next = self.builder.ins().iadd_imm(ordinal, 1);
        self.builder
            .ins()
            .jump(header, &[next.into(), next_frame.into(), current.into()]);
        self.builder.seal_block(header);

        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        let result = self.builder.block_params(exit)[0];
        self.unpack_registers(result, destinations)
    }

    /// A fold whose transition overwrites its carried tuple in one frame and
    /// reads its captures where they live, so an iteration copies neither a
    /// value it leaves unchanged nor a capture. Returns false, having emitted
    /// nothing that runs, when [`placement::placement_is_safe`] refuses the
    /// transition on that storage.
    fn lower_fold_in_place(
        &mut self,
        domain: &StructuredIndexDomain,
        initial: &[solve::SolveRegisterId],
        captures: &[solve::SolveRegisterId],
        state_captures: &[solve::SolveAssignedStateId],
        destinations: &[solve::SolveRegisterId],
        (transition, continuation): (
            &solve::SolveProgramRegion,
            Option<&solve::SolveProgramRegion>,
        ),
    ) -> Result<bool, CompileError> {
        if self.observation.is_some()
            || !state_captures.is_empty()
            || !placement::placeable(transition)
            || continuation.is_some_and(|continuation| !placement::placeable(continuation))
        {
            return Ok(false);
        }
        let carried_cells = self.register_cell_count(initial, "typed fold carried tuple")?;
        let capture_cells = self.register_cell_count(captures, "typed fold captures")?;
        let binder_cells = u32::try_from(domain.binders.len())
            .map_err(|_| CompileError::Backend("typed fold binder count overflows".into()))?;
        let input_cells = checked_cells(carried_cells, capture_cells, "typed fold input")?;
        let input_cells = checked_cells(input_cells, binder_cells, "typed fold binders")?;
        let plain = ProgramLayout::region(transition)?;
        let input_count = transition.inputs().len();
        if input_count != initial.len() + captures.len() + domain.binders.len()
            || transition.outputs().len() != initial.len()
        {
            return Ok(false);
        }
        let frame = create_tape(self.builder, self.pointer_type, input_cells)?;
        let layout = &plain;
        let mut inputs = frame_slots(layout, frame, 0..initial.len());
        let shared = self.share_carries(layout, (initial, destinations), captures, &mut inputs)?;
        inputs.extend(self.placed_slots_from(captures, carried_cells)?);
        inputs.extend(frame_slots(
            layout,
            frame,
            initial.len() + captures.len()..input_count,
        ));
        let mut outputs = frame_slots(layout, frame, input_count..input_count + initial.len());
        for (ordinal, shared) in shared.iter().enumerate() {
            if *shared {
                outputs[ordinal] = inputs[ordinal].clone();
            }
        }
        let (layout, safe) = placed_layout(
            transition,
            &placement::slot_cells(&inputs),
            &placement::slot_cells(&outputs),
        )?;
        if !safe {
            return Ok(false);
        }
        self.pack_unshared(initial, &shared, frame, true)?;
        self.emit_in_place_fold_loop(
            domain,
            (frame, carried_cells + capture_cells),
            (transition, &layout, continuation),
            (inputs, outputs),
        )?;
        self.pack_unshared(destinations, &shared, frame, false)?;
        Ok(true)
    }

    /// Carry on its own cells every value whose entry and result already
    /// share them: nothing packs it in or unpacks it out. The transition
    /// check sees one iteration, so a capture on those cells would read a
    /// later iteration's writes; none may touch them.
    fn share_carries(
        &mut self,
        layout: &ProgramLayout,
        (initial, destinations): (&[solve::SolveRegisterId], &[solve::SolveRegisterId]),
        captures: &[solve::SolveRegisterId],
        inputs: &mut [PlacedSlot],
    ) -> Result<Vec<bool>, CompileError> {
        let capture_cells = captures
            .iter()
            .map(|capture| {
                self.register(*capture)
                    .map(|location| self.physical(location))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut shared = vec![false; initial.len()];
        for (ordinal, (entry, result)) in initial.iter().zip(destinations).enumerate() {
            let entry_cells = self.physical(self.register(*entry)?);
            let touched = capture_cells
                .iter()
                .any(|cells| cells.overlaps(entry_cells));
            let duplicated = initial.iter().filter(|other| *other == entry).count() > 1;
            if entry_cells.count == 0
                || touched
                || duplicated
                || entry_cells != self.physical(self.register(*result)?)
            {
                continue;
            }
            let start = layout.slots[ordinal].cell;
            inputs[ordinal] = self
                .placed_slots_from(std::slice::from_ref(entry), start)?
                .remove(0);
            shared[ordinal] = true;
        }
        Ok(shared)
    }

    /// The loop of [`Self::lower_fold_in_place`]: binders, the optional
    /// early exit, and the transition, all on one frame.
    fn emit_in_place_fold_loop(
        &mut self,
        domain: &StructuredIndexDomain,
        (frame, binder_cell): (Value, u32),
        (transition, layout, continuation): (
            &solve::SolveProgramRegion,
            &ProgramLayout,
            Option<&solve::SolveProgramRegion>,
        ),
        (inputs, outputs): (Vec<PlacedSlot>, Vec<PlacedSlot>),
    ) -> Result<(), CompileError> {
        let predicate = continuation
            .map(|_| create_tape(self.builder, self.pointer_type, 1))
            .transpose()?;
        let count = domain
            .scalar_count()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let extents = domain
            .extents()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let strides = domain
            .ordinal_strides()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let count = i64::try_from(count)
            .map_err(|_| CompileError::Backend("typed fold domain exceeds i64".into()))?;
        let header = self.builder.create_block();
        let body = self.builder.create_block();
        let exit = self.builder.create_block();
        self.builder.append_block_param(header, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(header, &[zero.into()]);
        self.builder.switch_to_block(header);
        let ordinal = self.builder.block_params(header)[0];
        let in_range = self
            .builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, ordinal, count);
        self.builder.ins().brif(in_range, body, &[], exit, &[]);

        self.builder.switch_to_block(body);
        self.builder.seal_block(body);
        self.store_domain_binders(domain, &extents, &strides, ordinal, frame, binder_cell)?;
        if let (Some(continuation), Some(predicate)) = (continuation, predicate) {
            let continuation_layout = ProgramLayout::region(continuation)?;
            self.lower_placed_region(
                continuation,
                &continuation_layout,
                (frame, predicate),
                (Some(inputs.clone()), None),
            )?;
            let value = self.builder.ins().load(
                types::I64,
                cranelift_codegen::ir::MemFlags::trusted(),
                predicate,
                0,
            );
            let proceed = self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0);
            let pass = self.builder.create_block();
            self.builder.ins().brif(proceed, pass, &[], exit, &[]);
            self.builder.switch_to_block(pass);
            self.builder.seal_block(pass);
        }
        self.lower_placed_region(
            transition,
            layout,
            (frame, frame),
            (Some(inputs), Some(outputs)),
        )?;
        let next = self.builder.ins().iadd_imm(ordinal, 1);
        self.builder.ins().jump(header, &[next.into()]);
        self.builder.seal_block(header);

        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        Ok(())
    }

    /// Pack (or, with `into_frame` false, unpack) the carried registers that
    /// do not already sit on their frame slot's cells.
    fn pack_unshared(
        &mut self,
        registers: &[solve::SolveRegisterId],
        shared: &[bool],
        frame: Value,
        into_frame: bool,
    ) -> Result<(), CompileError> {
        let mut cell = 0u32;
        for (register, shared) in registers.iter().zip(shared) {
            let location = self.register(*register)?.clone();
            let slot = ValueLocation {
                base: StorageBase::Tape,
                cell,
                value_type: location.value_type.clone(),
            };
            let base = self.base(&location);
            let (source, target, source_base, target_base) = if into_frame {
                (&location, &slot, base, frame)
            } else {
                (&slot, &location, frame, base)
            };
            if !*shared {
                self.copy_between_bases(source, target, source_base, target_base)?;
            }
            cell = checked_cells(
                cell,
                location.value_type.scalar_count(),
                "typed fold carried packing",
            )?;
        }
        Ok(())
    }

    fn lower_fold_continuation(
        &mut self,
        continuation: &solve::SolveProgramRegion,
        (current, predicate_input, predicate): (Value, Option<Value>, Value),
        (carried_cells, capture_cells, binder_cells): (u32, u32, u32),
        exit: Block,
    ) -> Result<(), CompileError> {
        let predicate_source = if let Some(frame) = predicate_input {
            let ordinary_cells = checked_cells(
                carried_cells + capture_cells,
                binder_cells,
                "assigned predicate input copy",
            )?;
            self.copy_frame_cells(current, frame, ordinary_cells)?;
            frame
        } else {
            current
        };
        self.lower_region(continuation, predicate_source, predicate)?;
        let value = self.builder.ins().load(
            types::I64,
            cranelift_codegen::ir::MemFlags::trusted(),
            predicate,
            0,
        );
        let proceed = self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0);
        let pass = self.builder.create_block();
        let stopped = self.builder.create_block();
        self.builder.ins().brif(proceed, pass, &[], stopped, &[]);
        self.builder.switch_to_block(stopped);
        self.builder.seal_block(stopped);
        self.carrier_event(
            super::super::observation_carrier::LOOP_EXIT,
            current,
            0,
            None,
        )?;
        self.builder.ins().jump(exit, &[current.into()]);
        self.builder.switch_to_block(pass);
        self.builder.seal_block(pass);
        Ok(())
    }

    fn prepare_fold_frames(
        &mut self,
        domain: &StructuredIndexDomain,
        initial: &[solve::SolveRegisterId],
        captures: &[solve::SolveRegisterId],
        state_captures: &[solve::SolveAssignedStateId],
        (transition, continuation): (
            &solve::SolveProgramRegion,
            Option<&solve::SolveProgramRegion>,
        ),
    ) -> Result<FoldFrames, CompileError> {
        let carried_cells = self.register_cell_count(initial, "typed fold carried tuple")?;
        let capture_cells = self.register_cell_count(captures, "typed fold captures")?;
        let binder_cells = u32::try_from(domain.binders.len())
            .map_err(|_| CompileError::Backend("typed fold binder count overflows".into()))?;
        let input_cells = checked_cells(carried_cells, capture_cells, "typed fold input")?;
        let input_cells = checked_cells(input_cells, binder_cells, "typed fold binders")?;
        // The complete transition input is a private fold-owned frame. Two
        // frames carry the tuple by swapping roles after each transition;
        // compact tensors therefore cross the loop backedge by ownership, not
        // by an elementwise copy. Captures are immutable and initialized once
        // in both frames. The transition writes only the carried prefix.
        let ordinary_input_cells = input_cells;
        let input_cells =
            self.assigned_input_cells(transition.body().assigned_state_inputs(), input_cells)?;
        let first = create_tape(self.builder, self.pointer_type, input_cells)?;
        let second = create_tape(self.builder, self.pointer_type, input_cells)?;
        self.pack_registers(initial, first)?;
        self.pack_registers_at(captures, first, carried_cells)?;
        self.pack_registers_at(captures, second, carried_cells)?;
        self.pack_assigned_inputs(
            state_captures,
            transition.body().assigned_state_inputs(),
            first,
            ordinary_input_cells,
        )?;
        self.pack_assigned_inputs(
            state_captures,
            transition.body().assigned_state_inputs(),
            second,
            ordinary_input_cells,
        )?;
        let predicate_input =
            if let Some(continuation) = continuation.filter(|_| !state_captures.is_empty()) {
                let ordinary = ordinary_input_cells;
                let cells = self
                    .assigned_input_cells(continuation.body().assigned_state_inputs(), ordinary)?;
                let frame = create_tape(self.builder, self.pointer_type, cells)?;
                self.pack_assigned_inputs(
                    state_captures,
                    continuation.body().assigned_state_inputs(),
                    frame,
                    ordinary,
                )?;
                Some(frame)
            } else {
                None
            };
        let predicate = continuation
            .map(|_| create_tape(self.builder, self.pointer_type, 1))
            .transpose()?;

        Ok(FoldFrames {
            carried_cells,
            capture_cells,
            first,
            second,
            predicate_input,
            predicate,
        })
    }

    pub(super) fn lower_map(
        &mut self,
        domain: &StructuredIndexDomain,
        captures: &[solve::SolveRegisterId],
        destination: solve::SolveRegisterId,
        body: &solve::SolveProgramRegion,
    ) -> Result<(), CompileError> {
        if self.observation.is_none() && self.lower_flat_map(domain, captures, destination, body)? {
            return Ok(());
        }
        let destination = self.register(destination)?.clone();
        let capture_cells = self.register_cell_count(captures, "typed map captures")?;
        let binder_cells = u32::try_from(domain.binders.len())
            .map_err(|_| CompileError::Backend("typed map binder count overflows".into()))?;
        let input_cells = checked_cells(capture_cells, binder_cells, "typed map input")?;
        let input = create_tape(self.builder, self.pointer_type, input_cells)?;
        let output_cells = body.outputs()[0].scalar_count();
        let output = create_tape(self.builder, self.pointer_type, output_cells)?;
        // Captures are immutable and a region's input slots are read-only by
        // construction (`SolveSlotAccess::ReadOnly`), so they are packed into
        // the iteration frame once; only the binders change per iteration.
        self.pack_registers(captures, input)?;

        let count = domain
            .scalar_count()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let extents = domain
            .extents()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let strides = domain
            .ordinal_strides()
            .map_err(|error| CompileError::Backend(error.to_string()))?;
        let count = i64::try_from(count)
            .map_err(|_| CompileError::Backend("typed map domain exceeds i64".into()))?;
        let header = self.builder.create_block();
        let iteration = self.builder.create_block();
        let exit = self.builder.create_block();
        self.builder.append_block_param(header, types::I64);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(header, &[zero.into()]);
        self.builder.switch_to_block(header);
        let ordinal = self.builder.block_params(header)[0];
        let in_range = self
            .builder
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, ordinal, count);
        self.builder.ins().brif(in_range, iteration, &[], exit, &[]);

        self.builder.switch_to_block(iteration);
        self.builder.seal_block(iteration);
        self.store_domain_binders(domain, &extents, &strides, ordinal, input, capture_cells)?;
        self.lower_region(body, input, output)?;
        self.copy_map_output(output, &destination, ordinal, output_cells)?;
        let next = self.builder.ins().iadd_imm(ordinal, 1);
        self.builder.ins().jump(header, &[next.into()]);
        self.builder.seal_block(header);

        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        Ok(())
    }

    pub(super) fn lower_region(
        &mut self,
        region: &solve::SolveProgramRegion,
        input: Value,
        output: Value,
    ) -> Result<(), CompileError> {
        let layout = ProgramLayout::region(region)?;
        self.lower_placed_region(region, &layout, (input, output), (None, None))
    }

    /// Lower `region` on `input`/`output` frames, with any of its slots
    /// placed on enclosing cells instead.
    pub(super) fn lower_placed_region(
        &mut self,
        region: &solve::SolveProgramRegion,
        layout: &ProgramLayout,
        (input, output): (Value, Value),
        (input_placement, output_placement): (Option<Vec<PlacedSlot>>, Option<Vec<PlacedSlot>>),
    ) -> Result<(), CompileError> {
        let region_address = self
            .builder
            .ins()
            .iconst(types::I64, region as *const _ as usize as i64);
        self.carrier_event(
            super::super::observation_carrier::REGION_ENTER,
            input,
            0,
            Some(region_address),
        )?;
        let tape = create_tape(self.builder, self.pointer_type, layout.tape_cells)?;
        let mut nested = ProgramLowerer {
            builder: self.builder,
            module: self.module,
            math: self.math,
            pointer_type: self.pointer_type,
            input,
            output,
            tape,
            layout,
            outputs: self.outputs,
            functions: self.functions,
            recursive_calls: self.recursive_calls,
            flags: self.flags,
            observation: self.observation.as_ref().map(ObservationLowering::nested),
            operation: std::ptr::null(),
            input_placement,
            output_placement,
            table: self.table,
            inline_depth: self.inline_depth,
        };
        nested.lower(region.body())?;
        self.carrier_event(
            super::super::observation_carrier::REGION_EXIT,
            input,
            0,
            None,
        )
    }

    pub(super) fn register_cell_count(
        &self,
        registers: &[solve::SolveRegisterId],
        context: &str,
    ) -> Result<u32, CompileError> {
        registers.iter().try_fold(0u32, |count, register| {
            checked_cells(
                count,
                self.register(*register)?.value_type.scalar_count(),
                context,
            )
        })
    }

    /// Copy registers, in order, into one fresh contiguous tape.
    pub(super) fn packed_tape(
        &mut self,
        registers: &[solve::SolveRegisterId],
        context: &str,
    ) -> Result<Value, CompileError> {
        let cells = self.register_cell_count(registers, context)?;
        let tape = create_tape(self.builder, self.pointer_type, cells)?;
        self.pack_registers(registers, tape)?;
        Ok(tape)
    }

    pub(super) fn pack_registers(
        &mut self,
        registers: &[solve::SolveRegisterId],
        pointer: Value,
    ) -> Result<(), CompileError> {
        self.pack_registers_at(registers, pointer, 0)
    }

    fn pack_registers_at(
        &mut self,
        registers: &[solve::SolveRegisterId],
        pointer: Value,
        start: u32,
    ) -> Result<(), CompileError> {
        let mut cell = start;
        for register in registers {
            let source = self.register(*register)?.clone();
            let destination = ValueLocation {
                base: StorageBase::Tape,
                cell,
                value_type: source.value_type.clone(),
            };
            self.copy_between_bases(&source, &destination, self.base(&source), pointer)?;
            cell = checked_cells(
                cell,
                source.value_type.scalar_count(),
                "typed register packing",
            )?;
        }
        Ok(())
    }

    pub(super) fn unpack_registers(
        &mut self,
        pointer: Value,
        registers: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let mut cell = 0u32;
        for register in registers {
            let destination = self.register(*register)?.clone();
            let source = ValueLocation {
                base: StorageBase::Tape,
                cell,
                value_type: destination.value_type.clone(),
            };
            self.copy_between_bases(&source, &destination, pointer, self.base(&destination))?;
            cell = checked_cells(
                cell,
                destination.value_type.scalar_count(),
                "typed register unpacking",
            )?;
        }
        Ok(())
    }

    fn store_domain_binders(
        &mut self,
        domain: &StructuredIndexDomain,
        extents: &[usize],
        strides: &[usize],
        ordinal: Value,
        pointer: Value,
        start: u32,
    ) -> Result<(), CompileError> {
        for (axis, ((binder, extent), stride)) in
            domain.binders.iter().zip(extents).zip(strides).enumerate()
        {
            let position = if *stride == 1 {
                ordinal
            } else {
                self.builder
                    .ins()
                    .udiv_imm(ordinal, domain_extent_i64(*stride, "stride")?)
            };
            let position = if *extent == 1 {
                self.builder.ins().iconst(types::I64, 0)
            } else {
                self.builder
                    .ins()
                    .urem_imm(position, domain_extent_i64(*extent, "extent")?)
            };
            let coordinate = self.builder.ins().imul_imm(position, binder.step);
            let coordinate = self.builder.ins().iadd_imm(coordinate, binder.lower);
            let offset = i32::try_from(
                (start as usize)
                    .checked_add(axis)
                    .and_then(|cell| cell.checked_mul(std::mem::size_of::<u64>()))
                    .ok_or_else(|| CompileError::Backend("typed binder offset overflows".into()))?,
            )
            .map_err(|_| CompileError::Backend("typed binder offset exceeds i32".into()))?;
            self.builder
                .ins()
                .store(self.flags, coordinate, pointer, offset);
        }
        Ok(())
    }

    fn copy_map_output(
        &mut self,
        output: Value,
        destination: &ValueLocation,
        ordinal: Value,
        output_cells: u32,
    ) -> Result<(), CompileError> {
        if output_cells > 16 {
            // One iteration's result is a contiguous run of the destination.
            let target = self
                .builder
                .ins()
                .imul_imm(ordinal, i64::from(output_cells));
            let base = self.base(destination);
            let target = self.cell_address(base, destination.cell, target);
            let bytes = self
                .builder
                .ins()
                .iconst(self.pointer_type, i64::from(output_cells) * 8);
            let config = self.module.target_config();
            self.builder.call_memmove(config, target, output, bytes);
            return Ok(());
        }
        let source = ValueLocation {
            base: StorageBase::Tape,
            cell: 0,
            value_type: solve::SolveValueType::scalar(destination.value_type.element_type()),
        };
        self.for_each_element(output_cells, |this, element| {
            let value = this.load_scalar_from(&source, output, element)?;
            let target = this
                .builder
                .ins()
                .imul_imm(ordinal, i64::from(output_cells));
            let target = this.builder.ins().iadd(target, element);
            this.store_scalar(destination, target, value)
        })
    }
}

/// The layout `region` runs placed on `inputs`/`outputs`, and whether that
/// placement is sound: functional updates first move onto the placed cells
/// they would otherwise copy from, and when that layout is refused the
/// region's plain layout is checked instead.
fn placed_layout(
    region: &solve::SolveProgramRegion,
    inputs: &[PhysicalCells],
    outputs: &[PhysicalCells],
) -> Result<(ProgramLayout, bool), CompileError> {
    let shape = placement::Shape::of(region);
    let mut relocated = ProgramLayout::region(region)?;
    placement::relocate_updates_onto_placed(shape, &mut relocated, inputs, outputs);
    if placement::placement_is_safe(shape, &relocated, inputs, outputs)? {
        return Ok((relocated, true));
    }
    let plain = ProgramLayout::region(region)?;
    let safe = placement::placement_is_safe(shape, &plain, inputs, outputs)?;
    Ok((plain, safe))
}

/// Region slots `slots` left on their own cells of frame `root`.
fn frame_slots(
    layout: &ProgramLayout,
    root: Value,
    slots: std::ops::Range<usize>,
) -> Vec<PlacedSlot> {
    layout.slots[slots]
        .iter()
        .map(|slot| {
            PlacedSlot::on_frame(PhysicalCells {
                root,
                cell: slot.cell,
                count: slot.value_type.scalar_count(),
            })
        })
        .collect()
}

fn domain_extent_i64(value: usize, kind: &str) -> Result<i64, CompileError> {
    i64::try_from(value)
        .map_err(|_| CompileError::Backend(format!("typed domain {kind} exceeds i64")))
}
