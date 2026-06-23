use cranelift_jit::JITBuilder;
use rumoca_core::ExternalTableData;
use rumoca_eval_solve::{
    eval_table_bound_value_in, eval_table_lookup_slope_value_in, eval_table_lookup_value_in,
    eval_time_table_next_event_value_in,
};
use std::cell::Cell;

pub(super) fn register_math_symbols(builder: &mut JITBuilder) {
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
}

pub(super) fn with_active_external_tables<R>(
    tables: &[ExternalTableData],
    f: impl FnOnce() -> R,
) -> R {
    ACTIVE_EXTERNAL_TABLES.with(|cell| {
        let previous = cell.replace(ActiveExternalTables {
            ptr: tables.as_ptr(),
            len: tables.len(),
        });
        let result = f();
        cell.set(previous);
        result
    })
}

fn with_current_external_tables(f: impl FnOnce(&[ExternalTableData]) -> f64) -> f64 {
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
    x.sin()
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

pub(super) extern "C" fn rumoca_host_table_bounds_min(table_id: f64) -> f64 {
    with_current_external_tables(|tables| {
        eval_table_bound_value_in(table_id, false, tables).unwrap_or(f64::NAN)
    })
}

pub(super) extern "C" fn rumoca_host_table_bounds_max(table_id: f64) -> f64 {
    with_current_external_tables(|tables| {
        eval_table_bound_value_in(table_id, true, tables).unwrap_or(f64::NAN)
    })
}

pub(super) extern "C" fn rumoca_host_table_lookup(table_id: f64, column: f64, input: f64) -> f64 {
    with_current_external_tables(|tables| {
        eval_table_lookup_value_in(table_id, column, input, tables).unwrap_or(f64::NAN)
    })
}

pub(super) extern "C" fn rumoca_host_table_lookup_slope(
    table_id: f64,
    column: f64,
    input: f64,
) -> f64 {
    with_current_external_tables(|tables| {
        eval_table_lookup_slope_value_in(table_id, column, input, tables).unwrap_or(f64::NAN)
    })
}

pub(super) extern "C" fn rumoca_host_table_next_event(table_id: f64, time: f64) -> f64 {
    with_current_external_tables(|tables| {
        eval_time_table_next_event_value_in(table_id, time, tables).unwrap_or(f64::NAN)
    })
}
