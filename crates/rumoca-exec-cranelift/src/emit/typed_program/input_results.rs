//! Native storage for one result at an owner-issued complete input coordinate.

use super::*;
use cranelift_codegen::isa::TargetFrontendConfig;
use std::cell::Cell;

/// Stable interior-mutable cells referenced only by this table's native code.
/// `Cell` makes the table !Sync, so its shared compiled callers cannot cross
/// threads. Moving an exclusively owned table preserves the boxed addresses.
/// The acyclic call graph excludes reentry into the same owner; distinct
/// owners and primal/directional forms have distinct storage.
pub(super) struct InputResults {
    cells: Box<[Cell<u64>]>,
    input_cells: u32,
    output_cells: u32,
}

impl InputResults {
    pub(super) fn new(
        coordinate: solve::SolvePureCallInputCoordinate,
    ) -> Result<Self, CompileError> {
        let input_cells = u32::try_from(coordinate.input_cells()).map_err(|_| {
            CompileError::Backend("pure call input extent exceeds native storage".into())
        })?;
        let output_cells = u32::try_from(coordinate.output_cells()).map_err(|_| {
            CompileError::Backend("pure call output extent exceeds native storage".into())
        })?;
        let count = checked_cells(input_cells, output_cells, "pure result coordinate")?;
        let count = checked_cells(count, 1, "pure result flag")?;
        // All offsets used below must be representable before any code is issued.
        cell_offset(count, "pure result storage")?;
        Ok(Self {
            cells: vec![Cell::new(0); count as usize].into_boxed_slice(),
            input_cells,
            output_cells,
        })
    }

    pub(super) fn enter(
        &self,
        builder: &mut FunctionBuilder<'_>,
        config: TargetFrontendConfig,
        input: Value,
        output: Value,
    ) -> Result<Value, CompileError> {
        let cache = builder
            .ins()
            .iconst(config.pointer_type(), self.cells.as_ptr() as usize as i64);
        let cached_input = cell_pointer(builder, cache, 1, "pure result inputs")?;
        let ready = builder.ins().load(types::I64, MemFlags::new(), cache, 0);
        let hit = builder.create_block();
        let compare = builder.create_block();
        let miss = builder.create_block();
        builder.ins().brif(ready, compare, &[], miss, &[]);
        builder.switch_to_block(compare);
        builder.seal_block(compare);
        let equal = equal_inputs(builder, config, input, cached_input, self.input_cells);
        builder.ins().brif(equal, hit, &[], miss, &[]);
        builder.switch_to_block(hit);
        builder.seal_block(hit);
        let cached_output = self.output_pointer(builder, cache)?;
        copy_cells(builder, config, cached_output, output, self.output_cells);
        status::succeed(builder);
        builder.switch_to_block(miss);
        builder.seal_block(miss);
        let zero = builder.ins().iconst(types::I64, 0);
        builder.ins().store(MemFlags::new(), zero, cache, 0);
        Ok(cache)
    }

    pub(super) fn publish(
        &self,
        builder: &mut FunctionBuilder<'_>,
        config: TargetFrontendConfig,
        cache: Value,
        input: Value,
        output: Value,
    ) -> Result<(), CompileError> {
        let cached_input = cell_pointer(builder, cache, 1, "pure result inputs")?;
        let cached_output = self.output_pointer(builder, cache)?;
        copy_cells(builder, config, input, cached_input, self.input_cells);
        copy_cells(builder, config, output, cached_output, self.output_cells);
        // Publish last, only after the complete typed body returned successfully.
        let one = builder.ins().iconst(types::I64, 1);
        builder.ins().store(MemFlags::new(), one, cache, 0);
        Ok(())
    }

    fn output_pointer(
        &self,
        builder: &mut FunctionBuilder<'_>,
        cache: Value,
    ) -> Result<Value, CompileError> {
        cell_pointer(builder, cache, 1 + self.input_cells, "pure result outputs")
    }
}

fn equal_inputs(
    builder: &mut FunctionBuilder<'_>,
    config: TargetFrontendConfig,
    input: Value,
    cached: Value,
    cells: u32,
) -> Value {
    if cells > 4 {
        let size = builder
            .ins()
            .iconst(config.pointer_type(), i64::from(cells) * CELL_BYTES as i64);
        let comparison = builder.call_memcmp(config, input, cached, size);
        return builder.ins().icmp_imm(IntCC::Equal, comparison, 0);
    }
    let mut equal = builder.ins().iconst(types::I8, 1);
    for index in 0..cells {
        let offset = (index as usize * CELL_BYTES) as i32;
        let left = builder
            .ins()
            .load(types::I64, MemFlags::new(), input, offset);
        let right = builder
            .ins()
            .load(types::I64, MemFlags::new(), cached, offset);
        let same = builder.ins().icmp(IntCC::Equal, left, right);
        equal = builder.ins().band(equal, same);
    }
    equal
}

fn copy_cells(
    builder: &mut FunctionBuilder<'_>,
    config: TargetFrontendConfig,
    source: Value,
    destination: Value,
    cells: u32,
) {
    builder.emit_small_memory_copy(
        config,
        destination,
        source,
        u64::from(cells) * CELL_BYTES as u64,
        CELL_BYTES as u8,
        CELL_BYTES as u8,
        true,
        MemFlags::new(),
    );
}
