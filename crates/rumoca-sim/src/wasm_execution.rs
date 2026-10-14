//! Portable execution and its target-independent admission controls.

#[cfg(any(target_arch = "wasm32", test))]
mod assignments;
#[cfg(any(target_arch = "wasm32", test))]
mod expressions;
#[cfg(any(target_arch = "wasm32", test))]
mod jacobians;
mod profile;
#[cfg(any(target_arch = "wasm32", test))]
mod target_values;

#[cfg(target_arch = "wasm32")]
pub(crate) use expressions::{admitted_native_execution_backend, execution_receipt};
