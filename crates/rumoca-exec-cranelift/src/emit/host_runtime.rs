mod matrix;
mod memory;

#[cfg(test)]
mod tests;

use cranelift_codegen::ir::LibCall;
use cranelift_jit::JITBuilder;
use rumoca_core::ExternalTableData;
use rumoca_eval_solve::{
    eval_table_bound_value_in, eval_table_lookup_slope_value_in, eval_table_lookup_value_in,
    eval_time_table_next_event_value_in,
};
use std::cell::{Cell, RefCell};

use super::{CompileError, to_backend_err};

/// Whether Cranelift's IR verifier runs on every function a JIT module
/// defines. The verifier checks IR well-formedness and never changes the
/// emitted code; debug builds (and so the test suite) run it on every
/// definition, release builds do not.
const JIT_VERIFIER: &str = if cfg!(debug_assertions) {
    "true"
} else {
    "false"
};

/// The single JIT configuration for every module this crate emits.
pub(super) fn host_jit_builder() -> Result<JITBuilder, CompileError> {
    let libcall_names = cranelift_module::default_libcall_names();
    let memory_symbols = [
        (LibCall::Memcpy, memcpy as *const u8),
        (LibCall::Memmove, memmove as *const u8),
        (LibCall::Memset, memset as *const u8),
        (LibCall::Memcmp, memcmp as *const u8),
    ]
    .map(|(call, address)| (libcall_names(call), address));
    let mut builder = JITBuilder::with_flags(
        &[
            ("opt_level", "speed"),
            ("enable_verifier", JIT_VERIFIER),
            // Probe every frame larger than the guard page, so a large frame
            // cannot step past it whichever register allocator formed it.
            ("enable_probestack", "true"),
            ("probestack_strategy", "inline"),
        ],
        libcall_names,
    )
    .map_err(to_backend_err)?;
    builder.memory_provider(Box::new(memory::ChunkedMemory::default()));
    // Link the libcall directly: static PIE executables need not export libc symbols.
    builder.symbols(memory_symbols);
    Ok(builder)
}

unsafe extern "C" {
    fn memcpy(
        dst: *mut std::ffi::c_void,
        src: *const std::ffi::c_void,
        size: usize,
    ) -> *mut std::ffi::c_void;
    fn memmove(
        dst: *mut std::ffi::c_void,
        src: *const std::ffi::c_void,
        size: usize,
    ) -> *mut std::ffi::c_void;
    fn memset(
        dst: *mut std::ffi::c_void,
        value: std::ffi::c_int,
        size: usize,
    ) -> *mut std::ffi::c_void;
    fn memcmp(
        lhs: *const std::ffi::c_void,
        rhs: *const std::ffi::c_void,
        size: usize,
    ) -> std::ffi::c_int;
}

pub(super) fn register_math_symbols(builder: &mut JITBuilder) {
    super::native_fault::register(builder);
    builder.symbol(
        "rumoca_native_observe",
        super::observation_carrier::observe as *const u8,
    );
    builder.symbol(
        "rumoca_host_native",
        super::typed_program::rumoca_host_native as *const u8,
    );
    builder.symbol(
        "rumoca_host_matmul_f64",
        matrix::rumoca_host_matmul_f64 as *const u8,
    );
    builder.symbol(
        "rumoca_host_matmul_t_f64",
        matrix::rumoca_host_matmul_t_f64 as *const u8,
    );
    builder.symbol(
        "rumoca_host_transpose_cells",
        matrix::rumoca_host_transpose_cells as *const u8,
    );
    builder.symbol("rumoca_host_sin", rumoca_host_sin as *const u8);
    builder.symbol("rumoca_host_cos", rumoca_host_cos as *const u8);
    builder.symbol("rumoca_host_tan", rumoca_host_tan as *const u8);
    builder.symbol("rumoca_host_asin", rumoca_host_asin as *const u8);
    builder.symbol("rumoca_host_acos", rumoca_host_acos as *const u8);
    builder.symbol("rumoca_host_atan", rumoca_host_atan as *const u8);
    builder.symbol("rumoca_host_atan2", rumoca_host_atan2 as *const u8);
    builder.symbol("rumoca_host_sinh", rumoca_host_sinh as *const u8);
    builder.symbol("rumoca_host_cosh", rumoca_host_cosh as *const u8);
    builder.symbol("rumoca_host_tanh", rumoca_host_tanh as *const u8);
    builder.symbol("rumoca_host_exp", rumoca_host_exp as *const u8);
    builder.symbol("rumoca_host_log", rumoca_host_log as *const u8);
    builder.symbol("rumoca_host_log10", rumoca_host_log10 as *const u8);
    builder.symbol("rumoca_host_floor", rumoca_host_floor as *const u8);
    builder.symbol("rumoca_host_ceil", rumoca_host_ceil as *const u8);
    builder.symbol("rumoca_host_trunc", rumoca_host_trunc as *const u8);
    builder.symbol("rumoca_host_powf", rumoca_host_powf as *const u8);
    builder.symbol(
        "rumoca_host_table_bounds_min",
        rumoca_host_table_bounds_min as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_bounds_max",
        rumoca_host_table_bounds_max as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_lookup",
        rumoca_host_table_lookup as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_lookup_slope",
        rumoca_host_table_lookup_slope as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_next_event",
        rumoca_host_table_next_event as *const u8,
    );
}

#[derive(Clone, Copy)]
struct ActiveExternalTables {
    ptr: *const ExternalTableData,
    len: usize,
}

thread_local! {
    static ACTIVE_EXTERNAL_TABLES: Cell<ActiveExternalTables> = const {
        Cell::new(ActiveExternalTables {
            ptr: std::ptr::null(),
            len: 0,
        })
    };
    static TABLE_FAULT: RefCell<Option<rumoca_eval_solve::TableRuntimeError>> = const { RefCell::new(None) };
}

struct ActiveTableScope {
    tables: ActiveExternalTables,
    fault: Option<rumoca_eval_solve::TableRuntimeError>,
}

impl Drop for ActiveTableScope {
    fn drop(&mut self) {
        ACTIVE_EXTERNAL_TABLES.with(|cell| cell.set(self.tables));
        TABLE_FAULT.with(|cell| cell.replace(self.fault.take()));
    }
}

pub(super) fn with_active_external_tables<R>(
    tables: &[ExternalTableData],
    f: impl FnOnce() -> R,
) -> R {
    let previous = ACTIVE_EXTERNAL_TABLES.with(|cell| {
        cell.replace(ActiveExternalTables {
            ptr: tables.as_ptr(),
            len: tables.len(),
        })
    });
    let _scope = ActiveTableScope {
        tables: previous,
        fault: TABLE_FAULT.with(|cell| cell.replace(None)),
    };
    f()
}

pub(super) fn take_table_fault() -> Option<rumoca_eval_solve::TableRuntimeError> {
    TABLE_FAULT.with(|cell| cell.borrow_mut().take())
}

fn with_current_external_tables<R>(f: impl FnOnce(&[ExternalTableData]) -> R) -> R {
    ACTIVE_EXTERNAL_TABLES.with(|cell| {
        let active = cell.get();
        let tables = if active.len == 0 {
            &[]
        } else {
            // SAFETY: with_active_external_tables installs a slice pointer for
            // the duration of the synchronous JIT call. Host callbacks execute
            // on that same thread before the slice is restored.
            unsafe { std::slice::from_raw_parts(active.ptr, active.len) }
        };
        f(tables)
    })
}

extern "C" fn rumoca_host_sin(x: f64) -> f64 {
    #[cfg(test)]
    SIN_CALLS.with(|calls| calls.set(calls.get() + 1));
    x.sin()
}

#[cfg(test)]
thread_local! {
    static SIN_CALLS: Cell<usize> = const { Cell::new(0) };
}

#[cfg(test)]
pub(super) use matrix::take_matmul_calls;

#[cfg(test)]
pub(super) fn take_sin_calls() -> usize {
    SIN_CALLS.with(|calls| calls.replace(0))
}
extern "C" fn rumoca_host_cos(x: f64) -> f64 {
    x.cos()
}
extern "C" fn rumoca_host_tan(x: f64) -> f64 {
    x.tan()
}
extern "C" fn rumoca_host_asin(x: f64) -> f64 {
    x.asin()
}
extern "C" fn rumoca_host_acos(x: f64) -> f64 {
    x.acos()
}
extern "C" fn rumoca_host_atan(x: f64) -> f64 {
    x.atan()
}
extern "C" fn rumoca_host_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}
extern "C" fn rumoca_host_sinh(x: f64) -> f64 {
    x.sinh()
}
extern "C" fn rumoca_host_cosh(x: f64) -> f64 {
    x.cosh()
}
extern "C" fn rumoca_host_tanh(x: f64) -> f64 {
    x.tanh()
}
extern "C" fn rumoca_host_exp(x: f64) -> f64 {
    x.exp()
}
extern "C" fn rumoca_host_log(x: f64) -> f64 {
    x.ln()
}
extern "C" fn rumoca_host_log10(x: f64) -> f64 {
    x.log10()
}
extern "C" fn rumoca_host_floor(x: f64) -> f64 {
    x.floor()
}
extern "C" fn rumoca_host_ceil(x: f64) -> f64 {
    x.ceil()
}
extern "C" fn rumoca_host_trunc(x: f64) -> f64 {
    x.trunc()
}
extern "C" fn rumoca_host_powf(x: f64, y: f64) -> f64 {
    x.powf(y)
}

unsafe fn publish_table_result(
    out: *mut f64,
    value: Result<f64, rumoca_eval_solve::TableRuntimeError>,
) -> u8 {
    match value {
        Ok(value) => {
            // SAFETY: the emitted call supplies one writable result cell.
            unsafe { out.write(value) };
            0
        }
        Err(error) => {
            TABLE_FAULT.with(|cell| cell.replace(Some(error)));
            super::status::TABLE_FAILURE
        }
    }
}

/// # Safety
/// `out` addresses the emitted call's single writable result cell.
pub(super) unsafe extern "C" fn rumoca_host_table_bounds_min(table_id: f64, out: *mut f64) -> u8 {
    let value =
        with_current_external_tables(|tables| eval_table_bound_value_in(table_id, false, tables));
    // SAFETY: the emitted ABI supplies the result cell described above.
    unsafe { publish_table_result(out, value) }
}

/// # Safety
/// `out` addresses the emitted call's single writable result cell.
pub(super) unsafe extern "C" fn rumoca_host_table_bounds_max(table_id: f64, out: *mut f64) -> u8 {
    let value =
        with_current_external_tables(|tables| eval_table_bound_value_in(table_id, true, tables));
    // SAFETY: the emitted ABI supplies the result cell described above.
    unsafe { publish_table_result(out, value) }
}

/// # Safety
/// `out` addresses the emitted call's single writable result cell.
pub(super) unsafe extern "C" fn rumoca_host_table_lookup(
    table_id: f64,
    column: f64,
    input: f64,
    out: *mut f64,
) -> u8 {
    let value = with_current_external_tables(|tables| {
        eval_table_lookup_value_in(table_id, column, input, tables)
    });
    // SAFETY: the emitted ABI supplies the result cell described above.
    unsafe { publish_table_result(out, value) }
}

/// # Safety
/// `out` addresses the emitted call's single writable result cell.
pub(super) unsafe extern "C" fn rumoca_host_table_lookup_slope(
    table_id: f64,
    column: f64,
    input: f64,
    out: *mut f64,
) -> u8 {
    let value = with_current_external_tables(|tables| {
        eval_table_lookup_slope_value_in(table_id, column, input, tables)
    });
    // SAFETY: the emitted ABI supplies the result cell described above.
    unsafe { publish_table_result(out, value) }
}

/// # Safety
/// `out` addresses the emitted call's single writable result cell.
pub(super) unsafe extern "C" fn rumoca_host_table_next_event(
    table_id: f64,
    time: f64,
    out: *mut f64,
) -> u8 {
    let value = with_current_external_tables(|tables| {
        eval_time_table_next_event_value_in(table_id, time, tables)
    });
    // SAFETY: the emitted ABI supplies the result cell described above.
    unsafe { publish_table_result(out, value) }
}
