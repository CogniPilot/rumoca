//! Explicit failure propagation across private native kernel calls.

use super::CompileError;
use cranelift_codegen::ir::{InstBuilder, Value, types};
use cranelift_frontend::FunctionBuilder;

const INDEX_OUT_OF_BOUNDS: i64 = 1;

pub(super) fn check(status: u8) -> Result<(), CompileError> {
    match status {
        0 => Ok(()),
        1 => Err(CompileError::Input(
            "native tensor index is out of bounds".into(),
        )),
        _ => Err(CompileError::Backend(format!(
            "unknown native kernel status {status}"
        ))),
    }
}

pub(super) fn succeed(builder: &mut FunctionBuilder<'_>) {
    let success = builder.ins().iconst(types::I8, 0);
    builder.ins().return_(&[success]);
}

pub(super) fn propagate(builder: &mut FunctionBuilder<'_>, status: Value) {
    let failed = builder.create_block();
    let continuation = builder.create_block();
    builder.ins().brif(status, failed, &[], continuation, &[]);
    builder.switch_to_block(failed);
    builder.seal_block(failed);
    builder.ins().return_(&[status]);
    builder.switch_to_block(continuation);
    builder.seal_block(continuation);
}

pub(super) fn require_index(builder: &mut FunctionBuilder<'_>, valid: Value) {
    let failed = builder.create_block();
    let continuation = builder.create_block();
    builder.ins().brif(valid, continuation, &[], failed, &[]);
    builder.switch_to_block(failed);
    builder.seal_block(failed);
    let status = builder.ins().iconst(types::I8, INDEX_OUT_OF_BOUNDS);
    builder.ins().return_(&[status]);
    builder.switch_to_block(continuation);
    builder.seal_block(continuation);
}
