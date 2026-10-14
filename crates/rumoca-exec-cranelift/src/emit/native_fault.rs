//! Private source-fault details published only with the matching native failure status.
use super::scratch;
use crate::{CompileError, NativeSourceFault};
use rumoca_ir_solve::SolveSpannedOperation;
use std::cell::RefCell;

struct Fault {
    status: u8,
    kind: NativeSourceFault,
    message: String,
    span: Option<rumoca_core::Span>,
}

thread_local! {
    static FAULT: RefCell<Option<Fault>> = const { RefCell::new(None) };
}

pub(super) fn clear() {
    FAULT.with(|fault| fault.borrow_mut().take());
}

pub(super) fn foreign(message: &'static str) {
    FAULT.with(|fault| {
        *fault.borrow_mut() = Some(Fault {
            status: 5,
            kind: NativeSourceFault::ForeignBody,
            message: message.to_owned(),
            span: None,
        })
    });
}

pub(super) fn take(status: u8) -> Option<CompileError> {
    let fault = FAULT.with(|fault| fault.borrow_mut().take())?;
    (status == fault.status).then_some(CompileError::SourceOperation {
        kind: fault.kind,
        message: fault.message,
        span: fault.span,
    })
}

unsafe fn source_span(operation: *const SolveSpannedOperation) -> Option<rumoca_core::Span> {
    // SAFETY: native emission supplies null or a source operation pinned by its compiled table.
    unsafe { operation.as_ref().map(SolveSpannedOperation::provenance) }
}

unsafe extern "C" fn index_real(value: f64, extent: u32, operation: *const SolveSpannedOperation) {
    let fault = Fault {
        status: 1,
        kind: NativeSourceFault::TensorIndex,
        message: format!("index {value} is outside 1..={extent}"),
        span: unsafe { source_span(operation) },
    };
    FAULT.with(|cell| *cell.borrow_mut() = Some(fault));
}

unsafe extern "C" fn index_integer(
    value: i64,
    extent: u32,
    operation: *const SolveSpannedOperation,
) {
    let fault = Fault {
        status: 1,
        kind: NativeSourceFault::TensorIndex,
        message: format!("index {value} is outside 1..={extent}"),
        span: unsafe { source_span(operation) },
    };
    FAULT.with(|cell| *cell.borrow_mut() = Some(fault));
}

unsafe extern "C" fn attach_span(operation: *const SolveSpannedOperation) {
    FAULT.with(|cell| {
        if let Some(fault) = cell.borrow_mut().as_mut() {
            fault.span = unsafe { source_span(operation) };
        }
    });
}

pub(super) fn register(builder: &mut cranelift_jit::JITBuilder) {
    builder.symbol("rumoca_index_real_fault", index_real as *const u8);
    builder.symbol("rumoca_index_integer_fault", index_integer as *const u8);
    builder.symbol("rumoca_fault_span", attach_span as *const u8);
}

pub(super) fn checked_index(
    module: &mut cranelift_jit::JITModule,
    builder: &mut cranelift_frontend::FunctionBuilder<'_>,
    operands: (
        cranelift_codegen::ir::Value,
        cranelift_codegen::ir::Value,
        u32,
    ),
    operation: *const SolveSpannedOperation,
) -> Result<(), CompileError> {
    use cranelift_codegen::ir::{AbiParam, InstBuilder, types};
    use cranelift_module::{Linkage, Module};
    let (valid, value, extent) = operands;
    let pointer = module.target_config().pointer_type();
    let kind = builder.func.dfg.value_type(value);
    let mut signature = module.make_signature();
    signature
        .params
        .extend([kind, types::I32, pointer].map(AbiParam::new));
    let name = if kind == types::F64 {
        "rumoca_index_real_fault"
    } else {
        "rumoca_index_integer_fault"
    };
    let function = module
        .declare_function(name, Linkage::Import, &signature)
        .map_err(super::to_backend_err)?;
    let local = super::owned_jit_module::declare_far_call_in_func(module, function, builder.func);
    let failed = builder.create_block();
    let passed = builder.create_block();
    builder.ins().brif(valid, passed, &[], failed, &[]);
    builder.switch_to_block(failed);
    builder.seal_block(failed);
    let extent = builder.ins().iconst(types::I32, i64::from(extent));
    let source = builder.ins().iconst(pointer, operation as usize as i64);
    builder.ins().call(local, &[value, extent, source]);
    let status = builder.ins().iconst(types::I8, 1);
    scratch::leave(builder);
    builder.ins().return_(&[status]);
    builder.switch_to_block(passed);
    builder.seal_block(passed);
    Ok(())
}

pub(super) fn emit_span(
    module: &mut cranelift_jit::JITModule,
    builder: &mut cranelift_frontend::FunctionBuilder<'_>,
    operation: *const SolveSpannedOperation,
) -> Result<(), CompileError> {
    use cranelift_codegen::ir::{AbiParam, InstBuilder};
    use cranelift_module::{Linkage, Module};
    let pointer = module.target_config().pointer_type();
    let mut signature = module.make_signature();
    signature.params.push(AbiParam::new(pointer));
    let function = module
        .declare_function("rumoca_fault_span", Linkage::Import, &signature)
        .map_err(super::to_backend_err)?;
    let local = super::owned_jit_module::declare_far_call_in_func(module, function, builder.func);
    let source = builder.ins().iconst(pointer, operation as usize as i64);
    builder.ins().call(local, &[source]);
    Ok(())
}
