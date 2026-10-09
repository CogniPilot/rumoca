//! Portable execution and its target-independent admission controls.

#[cfg(target_arch = "wasm32")]
mod assignments;
#[cfg(any(target_arch = "wasm32", test))]
mod errors;
#[cfg(target_arch = "wasm32")]
mod expressions;
mod profile;
#[cfg(target_arch = "wasm32")]
mod target_values;

#[cfg(target_arch = "wasm32")]
pub(crate) use expressions::{admitted_native_execution_backend, execution_receipt};
