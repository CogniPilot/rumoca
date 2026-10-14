//! Caller-owned physical storage for private native register tapes.
//!
//! A stack slot holds only a pointer. Its compilation-issued slot index names
//! one reusable buffer in the active call frame. Moving frame/buffer descriptors
//! never moves their separately allocated cells; an active buffer cannot resize.

use super::{CompileError, status};
use cranelift_codegen::ir::{
    AbiParam, InstBuilder, MemFlags, Signature, StackSlot, StackSlotData, StackSlotKind, Type,
    Value, types,
};
use cranelift_frontend::FunctionBuilder;
use std::cell::{RefCell, RefMut};

pub(super) const FAILURE: u8 = 9;

#[derive(Default)]
pub(crate) struct NativeScratch {
    root: Frame,
    frames: Vec<Frame>,
    depth: usize,
    invalid: bool,
}

#[derive(Default)]
struct Frame {
    buffers: Vec<Buffer>,
}

#[derive(Default)]
struct Buffer {
    cells: Vec<u64>,
    active: bool,
}

impl NativeScratch {
    fn enter(&mut self) -> Option<()> {
        let next = self.depth.checked_add(1)?;
        if next > 1 && self.frames.len() < next - 1 {
            self.frames.try_reserve(1).ok()?;
            self.frames.push(Frame::default());
        }
        self.depth = next;
        for buffer in &mut self.frame()?.buffers {
            buffer.active = false;
        }
        Some(())
    }

    fn frame(&mut self) -> Option<&mut Frame> {
        match self.depth {
            0 => None,
            1 => Some(&mut self.root),
            depth => self.frames.get_mut(depth - 2),
        }
    }

    fn allocate(&mut self, site: u64, cells: u64) -> Option<*mut u64> {
        let frame = self.frame()?;
        let site = usize::try_from(site).ok()?;
        let cells = usize::try_from(cells).ok()?;
        let slots = site.checked_add(1)?;
        cells.checked_mul(std::mem::size_of::<u64>())?;
        if frame.buffers.len() < slots {
            frame
                .buffers
                .try_reserve(slots - frame.buffers.len())
                .ok()?;
            frame.buffers.resize_with(slots, Buffer::default);
        }
        let buffer = frame.buffers.get_mut(site)?;
        if buffer.active && buffer.cells.len() != cells {
            return None;
        }
        if !buffer.active && buffer.cells.len() != cells {
            buffer
                .cells
                .try_reserve(cells.saturating_sub(buffer.cells.len()))
                .ok()?;
            buffer.cells.resize(cells, 0);
        }
        buffer.active = true;
        Some(buffer.cells.as_mut_ptr())
    }

    fn leave(&mut self) {
        match self.depth.checked_sub(1) {
            Some(depth) => self.depth = depth,
            None => self.invalid = true,
        }
    }
}

/// One compiled owner's physical storage; a lease excludes overlapping entry.
#[derive(Default)]
pub(crate) struct ScratchOwner(RefCell<NativeScratch>);

impl ScratchOwner {
    pub(super) fn invoke(&self, call: impl FnOnce(*mut NativeScratch) -> u8) -> u8 {
        super::native_fault::clear();
        let Ok(scratch) = self.0.try_borrow_mut() else {
            return FAILURE;
        };
        let mut lease = ScratchLease {
            scratch,
            completed: false,
        };
        let result = call(&mut *lease.scratch);
        lease.completed = lease.scratch.depth == 0 && !lease.scratch.invalid;
        if lease.completed { result } else { FAILURE }
    }
}

struct ScratchLease<'a> {
    scratch: RefMut<'a, NativeScratch>,
    completed: bool,
}

impl Drop for ScratchLease<'_> {
    fn drop(&mut self) {
        if !self.completed {
            *self.scratch = NativeScratch::default();
        }
    }
}

unsafe extern "C" fn enter_native(scratch: *mut NativeScratch) -> u8 {
    // SAFETY: private entry wrappers own this arena throughout the invocation;
    // generated calls borrow it exclusively only while this callback runs.
    match unsafe { scratch.as_mut() }.and_then(NativeScratch::enter) {
        Some(()) => 0,
        None => FAILURE,
    }
}

unsafe extern "C" fn allocate_native(
    scratch: *mut NativeScratch,
    site: u64,
    cells: u64,
) -> *mut u64 {
    // SAFETY: the private ABI passes its live caller-owned arena. Cell pointers
    // refer to separate allocations and are never borrowed by this callback.
    unsafe { scratch.as_mut() }
        .and_then(|scratch| scratch.allocate(site, cells))
        .unwrap_or(std::ptr::null_mut())
}

unsafe extern "C" fn leave_native(scratch: *mut NativeScratch) {
    // SAFETY: generated success and failure returns end exactly their own frame.
    if let Some(scratch) = unsafe { scratch.as_mut() } {
        scratch.leave();
    }
}

pub(super) fn parameter(builder: &FunctionBuilder<'_>) -> Value {
    let entry = builder
        .func
        .layout
        .entry_block()
        .expect("native function has an entry");
    *builder
        .block_params(entry)
        .last()
        .expect("native function has its scratch parameter")
}

fn host_call(
    builder: &mut FunctionBuilder<'_>,
    address: usize,
    arguments: &[Value],
    result: Option<Type>,
) -> Option<Value> {
    let mut signature = Signature::new(builder.func.signature.call_conv);
    for argument in arguments {
        signature
            .params
            .push(AbiParam::new(builder.func.dfg.value_type(*argument)));
    }
    if let Some(result) = result {
        signature.returns.push(AbiParam::new(result));
    }
    let signature = builder.import_signature(signature);
    // Every private host callback receives its live arena first. Entry has not
    // inserted an instruction yet, so its block may not be in the layout.
    let arena = *arguments
        .first()
        .expect("native host call receives its arena");
    let pointer_type = builder.func.dfg.value_type(arena);
    let address = builder.ins().iconst(pointer_type, address as i64);
    let call = builder.ins().call_indirect(signature, address, arguments);
    result.map(|_| builder.inst_results(call)[0])
}

pub(super) fn enter(builder: &mut FunctionBuilder<'_>) {
    // Only entry acquisition uses the construction block. All later accesses
    // use parameter() and therefore retain the actual layout entry identity.
    let entry = builder.current_block().expect("native entry is selected");
    let arena = *builder
        .block_params(entry)
        .last()
        .expect("native entry has its scratch parameter");
    let result = host_call(
        builder,
        enter_native as *const () as usize,
        &[arena],
        Some(types::I8),
    )
    .expect("native frame entry returns status");
    // Entry failure has not acquired a frame, so it must not execute leave.
    let failed = builder.create_block();
    let body = builder.create_block();
    builder.ins().brif(result, failed, &[], body, &[]);
    builder.switch_to_block(failed);
    builder.seal_block(failed);
    builder.ins().return_(&[result]);
    builder.switch_to_block(body);
    builder.seal_block(body);
}

pub(super) fn leave(builder: &mut FunctionBuilder<'_>) {
    let arena = parameter(builder);
    let _ = host_call(builder, leave_native as *const () as usize, &[arena], None);
}

pub(super) fn create_slot(
    builder: &mut FunctionBuilder<'_>,
    bytes: u32,
) -> Result<StackSlot, CompileError> {
    if !bytes.is_multiple_of(8) {
        return Err(CompileError::Backend(
            "native tape is not a complete 64-bit cell range".into(),
        ));
    }
    let pointer_type = builder.func.dfg.value_type(parameter(builder));
    let slot = builder.create_sized_stack_slot(StackSlotData::new(
        StackSlotKind::ExplicitSlot,
        pointer_type.bytes(),
        3,
    ));
    let arena = parameter(builder);
    let site = builder.ins().iconst(types::I64, i64::from(slot.as_u32()));
    let cells = builder
        .ins()
        .iconst(types::I64, i64::from(bytes.max(8) / 8));
    let pointer = host_call(
        builder,
        allocate_native as *const () as usize,
        &[arena, site, cells],
        Some(pointer_type),
    )
    .expect("native tape allocation returns a pointer");
    let valid = builder.ins().icmp_imm(
        cranelift_codegen::ir::condcodes::IntCC::NotEqual,
        pointer,
        0,
    );
    status::require_scratch(builder, valid);
    builder.ins().stack_store(pointer, slot, 0);
    Ok(slot)
}

pub(super) fn stack_addr(
    builder: &mut FunctionBuilder<'_>,
    pointer_type: Type,
    slot: StackSlot,
    offset: i32,
) -> Value {
    let pointer = builder.ins().stack_load(pointer_type, slot, 0);
    builder.ins().iadd_imm(pointer, i64::from(offset))
}

pub(super) fn stack_load(
    builder: &mut FunctionBuilder<'_>,
    value_type: Type,
    slot: StackSlot,
    offset: i32,
) -> Value {
    let pointer_type = builder.func.dfg.value_type(parameter(builder));
    let pointer = stack_addr(builder, pointer_type, slot, offset);
    builder.ins().load(value_type, MemFlags::new(), pointer, 0)
}

pub(super) fn stack_store(
    builder: &mut FunctionBuilder<'_>,
    value: Value,
    slot: StackSlot,
    offset: i32,
) {
    let pointer_type = builder.func.dfg.value_type(parameter(builder));
    let pointer = stack_addr(builder, pointer_type, slot, offset);
    builder.ins().store(MemFlags::new(), value, pointer, 0);
}

#[cfg(test)]
mod tests;
