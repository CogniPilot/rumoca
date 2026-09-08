import RumocaPhaseSolve.Types

open Aeneas Aeneas.Std

/-- Correspondence to Rust's binary64 storage bitcast is trusted explicitly. -/
@[rust_fun "core::f64::{f64}::to_bits"]
def core.f64.F64.to_bits (value : F64) : Result U64 := .ok value.storageBits
