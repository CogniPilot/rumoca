use super::{
    CompileError, ProgramLayout, ProgramLowerer, StorageBase, ValueLocation, checked_cells,
    create_tape,
};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, Value, types};
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve as solve;

impl ProgramLowerer<'_, '_> {
    pub(super) fn lower_conditional(
        &mut self,
        condition: solve::SolveRegisterId,
        captures: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        if_true: &solve::SolveProgramRegion,
        if_false: &solve::SolveProgramRegion,
    ) -> Result<(), CompileError> {
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
        self.lower_region(if_true, input, output, true)?;
        self.builder.ins().jump(continuation, &[]);

        self.builder.switch_to_block(false_block);
        self.builder.seal_block(false_block);
        self.lower_region(if_false, input, output, true)?;
        self.builder.ins().jump(continuation, &[]);

        self.builder.switch_to_block(continuation);
        self.builder.seal_block(continuation);
        self.unpack_registers(output, destinations)
    }

    pub(super) fn lower_fold(
        &mut self,
        domain: &StructuredIndexDomain,
        initial: &[solve::SolveRegisterId],
        captures: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        transition: &solve::SolveProgramRegion,
    ) -> Result<(), CompileError> {
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
        let first = create_tape(self.builder, self.pointer_type, input_cells)?;
        let second = create_tape(self.builder, self.pointer_type, input_cells)?;
        self.pack_registers(initial, first)?;
        self.pack_registers_at(captures, first, carried_cells)?;
        self.pack_registers_at(captures, second, carried_cells)?;

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
        self.store_domain_binders(
            domain,
            &extents,
            &strides,
            ordinal,
            current,
            carried_cells + capture_cells,
        )?;
        self.lower_region(transition, current, next_frame, false)?;
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

    pub(super) fn lower_map(
        &mut self,
        domain: &StructuredIndexDomain,
        captures: &[solve::SolveRegisterId],
        destination: solve::SolveRegisterId,
        body: &solve::SolveProgramRegion,
    ) -> Result<(), CompileError> {
        let destination = self.register(destination)?.clone();
        let capture_cells = self.register_cell_count(captures, "typed map captures")?;
        let binder_cells = u32::try_from(domain.binders.len())
            .map_err(|_| CompileError::Backend("typed map binder count overflows".into()))?;
        let input_cells = checked_cells(capture_cells, binder_cells, "typed map input")?;
        let capture_values = create_tape(self.builder, self.pointer_type, capture_cells)?;
        let input = create_tape(self.builder, self.pointer_type, input_cells)?;
        let output_cells = body.outputs()[0].scalar_count();
        let output = create_tape(self.builder, self.pointer_type, output_cells)?;
        self.pack_registers(captures, capture_values)?;

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
        self.copy_register_types(captures, capture_values, 0, input, 0)?;
        self.store_domain_binders(domain, &extents, &strides, ordinal, input, capture_cells)?;
        self.lower_region(body, input, output, false)?;
        self.copy_map_output(output, &destination, ordinal, output_cells)?;
        let next = self.builder.ins().iadd_imm(ordinal, 1);
        self.builder.ins().jump(header, &[next.into()]);
        self.builder.seal_block(header);

        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        Ok(())
    }

    fn lower_region(
        &mut self,
        region: &solve::SolveProgramRegion,
        input: Value,
        output: Value,
        inherit_invocations: bool,
    ) -> Result<(), CompileError> {
        let layout = ProgramLayout::region(region)?;
        let tape = create_tape(self.builder, self.pointer_type, layout.tape_cells)?;
        let mut nested = ProgramLowerer {
            builder: self.builder,
            module: self.module,
            math: self.math,
            pointer_type: self.pointer_type,
            input,
            output,
            tape,
            layout: &layout,
            functions: self.functions,
            invocation_cache: inherit_invocations
                .then_some(self.invocation_cache)
                .flatten(),
            invocation_layout: inherit_invocations
                .then_some(self.invocation_layout)
                .flatten(),
            flags: self.flags,
        };
        nested.lower(region.body())
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

    fn pack_registers(
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

    fn copy_register_types(
        &mut self,
        registers: &[solve::SolveRegisterId],
        source_pointer: Value,
        source_start: u32,
        destination_pointer: Value,
        destination_start: u32,
    ) -> Result<(), CompileError> {
        let mut source_cell = source_start;
        let mut destination_cell = destination_start;
        for register in registers {
            let value_type = self.register(*register)?.value_type.clone();
            let source = ValueLocation {
                base: StorageBase::Tape,
                cell: source_cell,
                value_type: value_type.clone(),
            };
            let destination = ValueLocation {
                base: StorageBase::Tape,
                cell: destination_cell,
                value_type: value_type.clone(),
            };
            self.copy_between_bases(&source, &destination, source_pointer, destination_pointer)?;
            source_cell =
                checked_cells(source_cell, value_type.scalar_count(), "typed tuple source")?;
            destination_cell = checked_cells(
                destination_cell,
                value_type.scalar_count(),
                "typed tuple destination",
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
                self.builder.ins().udiv_imm(
                    ordinal,
                    i64::try_from(*stride).map_err(|_| {
                        CompileError::Backend("typed domain stride exceeds i64".into())
                    })?,
                )
            };
            let position = if *extent == 1 {
                self.builder.ins().iconst(types::I64, 0)
            } else {
                self.builder.ins().urem_imm(
                    position,
                    i64::try_from(*extent).map_err(|_| {
                        CompileError::Backend("typed domain extent exceeds i64".into())
                    })?,
                )
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
