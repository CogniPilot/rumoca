//! Explicit failure propagation across private native kernel calls.

use super::{CompileError, scratch};
use crate::NativeSourceFault;
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
const SCALAR_PAYLOAD_FAILURE: i64 = 7;
const INTEGER_ARITHMETIC_FAILURE: i64 = 8;
/// A table callback retained its checked source fault in this invocation.
pub(super) const TABLE_FAILURE: u8 = 10;

pub(super) fn check(status: u8) -> Result<(), CompileError> {
    if let Some(fault) = super::native_fault::take(status) {
        return Err(fault);
    }
    match status {
        0 => Ok(()),
        1 => Err(CompileError::SourceOperation {
            span: None,
            kind: NativeSourceFault::TensorIndex,
            message: "native tensor index check failed (operand details unavailable)".into(),
        }),
        2 => Err(CompileError::SourceOperation {
            span: None,
            kind: NativeSourceFault::LinearSolve,
            message: "native tensor linear solve is singular or non-finite".into(),
        }),
        3 => Err(CompileError::SourceOperation {
            span: None,
            kind: NativeSourceFault::IntegerQuotient,
            message: "native Integer quotient has a zero divisor or no representable result".into(),
        }),
        NATIVE_BODY_FAILURE => Err(CompileError::Backend(
            "native body host call received operands outside its interface".into(),
        )),
        NATIVE_BODY_FOREIGN_ERROR => Err(CompileError::SourceOperation {
            span: None,
            kind: NativeSourceFault::ForeignBody,
            message: "a native foreign body reported an error for its operands".into(),
        }),
        6 => Err(CompileError::SourceOperation {
            span: None,
            kind: NativeSourceFault::RecursionDepth,
            message: "recursive call exceeds the execution profile's depth limit".into(),
        }),
        7 => Err(CompileError::SourceOperation {
            span: None,
            kind: NativeSourceFault::ScalarPayload,
            message:
                "Integer value is outside its domain or not exactly representable in the scalar ABI"
                    .into(),
        }),
        8 => Err(CompileError::SourceOperation {
            span: None,
            kind: NativeSourceFault::IntegerArithmetic,
            message: "native Integer arithmetic left the checked target domain".into(),
        }),
        TABLE_FAILURE => match super::host_runtime::take_table_fault() {
            Some(error) => Err(CompileError::SourceOperation {
                kind: NativeSourceFault::ExternalTable,
                message: error.to_string(),
                span: None,
            }),
            None => Err(CompileError::Backend(
                "native table status has no invocation fault".into(),
            )),
        },
        scratch::FAILURE => Err(CompileError::Backend(
            "native scratch allocation or frame contract failed".into(),
        )),
        _ => Err(CompileError::Backend(format!(
            "unknown native kernel status {status}"
        ))),
    }
}

pub(super) fn succeed(builder: &mut FunctionBuilder<'_>) {
    let success = builder.ins().iconst(types::I8, 0);
    scratch::leave(builder);
    builder.ins().return_(&[success]);
}

pub(super) fn propagate(builder: &mut FunctionBuilder<'_>, status: Value) {
    let failed = builder.create_block();
    let continuation = builder.create_block();
    builder.ins().brif(status, failed, &[], continuation, &[]);
    builder.switch_to_block(failed);
    builder.seal_block(failed);
    scratch::leave(builder);
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

pub(super) fn require_scalar_payload(builder: &mut FunctionBuilder<'_>, valid: Value) {
    require(builder, valid, SCALAR_PAYLOAD_FAILURE);
}

pub(super) fn require_integer_arithmetic(builder: &mut FunctionBuilder<'_>, valid: Value) {
    require(builder, valid, INTEGER_ARITHMETIC_FAILURE);
}

pub(super) fn require_scratch(builder: &mut FunctionBuilder<'_>, valid: Value) {
    require(builder, valid, i64::from(scratch::FAILURE));
}

fn require(builder: &mut FunctionBuilder<'_>, valid: Value, failure: i64) {
    let failed = builder.create_block();
    let continuation = builder.create_block();
    builder.ins().brif(valid, continuation, &[], failed, &[]);
    builder.switch_to_block(failed);
    builder.seal_block(failed);
    let status = builder.ins().iconst(types::I8, failure);
    scratch::leave(builder);
    builder.ins().return_(&[status]);
    builder.switch_to_block(continuation);
    builder.seal_block(continuation);
}

/// An assertion-observation form stopped at its first failed Error-level
/// assertion. It is not a failure: the stopping form has already published
/// its predicate cells, and every enclosing observation form forwards them and
/// stops in turn. The observing row absorbs it as success.
pub(super) const ASSERTION_STOP: i64 = 9;

/// Return from an observation form at its first failed Error-level assertion.
pub(super) fn stop(builder: &mut FunctionBuilder<'_>) {
    let status = builder.ins().iconst(types::I8, ASSERTION_STOP);
    scratch::leave(builder);
    builder.ins().return_(&[status]);
}

/// Split `status` into whether it is the assertion stop and the status with
/// the stop read as success, so the caller propagates only genuine failures.
pub(super) fn separate_stop(builder: &mut FunctionBuilder<'_>, status: Value) -> (Value, Value) {
    let stopped = builder.ins().icmp_imm(
        cranelift_codegen::ir::condcodes::IntCC::Equal,
        status,
        ASSERTION_STOP,
    );
    let success = builder.ins().iconst(types::I8, 0);
    let failure = builder.ins().select(stopped, success, status);
    (stopped, failure)
}
