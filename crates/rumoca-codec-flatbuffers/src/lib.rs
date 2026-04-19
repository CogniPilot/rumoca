//! FlatBuffer protocol support for lockstep I/O.
//!
//! This crate owns schema reflection, routing/config types, and codec
//! compilation. It intentionally does not own transports, simulation policy,
//! or viewer/application code.

pub mod bfbs;
pub mod codec;
pub mod config;
