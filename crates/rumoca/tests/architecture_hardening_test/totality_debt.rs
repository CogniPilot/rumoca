//! Accounting for run-time totality assertions in production back-end source.
//!
//! `scan` counts call forms; `modules` resolves test-only files from Rust items;
//! `gate` pins the per-crate counts. These are
//! reachable from the other hardening gates, because "what does the shipped
//! code say" is one question and it must have one answer.

pub(crate) mod gate;
mod modules;
pub(crate) mod scan;
