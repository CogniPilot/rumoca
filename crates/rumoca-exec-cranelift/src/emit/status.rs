//! Explicit failure propagation across private native kernel calls.

use super::CompileError;
use cranelift_codegen::ir::{InstBuilder, Value, types};
use cranelift_frontend::FunctionBuilder;

const INDEX_OUT_OF_BOUNDS: i64 = 1;
const LINEAR_SOLVE_FAILURE: i64 = 2;
const INTEGER_QUOTIENT_FAILURE: i64 = 3;
/// A native body host call received a tape outside its catalog interface.
pub(super) const NATIVE_BODY_FAILURE: u8 = 4;
/// A native body reported its foreign error for these operands.
pub(super) const NATIVE_BODY_FOREIGN_ERROR: u8 = 5;
/// A SOLVE-C62 member call would exceed its group's declared depth limit.
const RECURSION_DEPTH_EXCEEDED: i64 = 6;

pub(super) fn check(status: u8) -> Result<(), CompileError> {
    match status {
        0 => Ok(()),
        1 => Err(CompileError::Input(
            "native tensor index is out of bounds".into(),
        )),
        2 => Err(CompileError::Input(
            "native tensor linear solve is singular or non-finite".into(),
        )),
        3 => Err(CompileError::Input(
            "native Integer quotient has a zero divisor or no representable result".into(),
        )),
        NATIVE_BODY_FAILURE => Err(CompileError::Backend(
            "native body host call received operands outside its interface".into(),
        )),
        NATIVE_BODY_FOREIGN_ERROR => Err(CompileError::Input(
            "a native foreign body reported an error for its operands".into(),
        )),
        6 => Err(CompileError::Input(
            "recursive call exceeds the execution profile's depth limit".into(),
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
    require(builder, valid, INDEX_OUT_OF_BOUNDS);
}

pub(super) fn require_linear_solve(builder: &mut FunctionBuilder<'_>, valid: Value) {
    require(builder, valid, LINEAR_SOLVE_FAILURE);
}

pub(super) fn require_recursion_depth(builder: &mut FunctionBuilder<'_>, valid: Value) {
    require(builder, valid, RECURSION_DEPTH_EXCEEDED);
}

pub(super) fn require_integer_quotient(builder: &mut FunctionBuilder<'_>, valid: Value) {
    require(builder, valid, INTEGER_QUOTIENT_FAILURE);
}

fn require(builder: &mut FunctionBuilder<'_>, valid: Value, failure: i64) {
    let failed = builder.create_block();
    let continuation = builder.create_block();
    builder.ins().brif(valid, continuation, &[], failed, &[]);
    builder.switch_to_block(failed);
    builder.seal_block(failed);
    let status = builder.ins().iconst(types::I8, failure);
    builder.ins().return_(&[status]);
    builder.switch_to_block(continuation);
    builder.seal_block(continuation);
}
