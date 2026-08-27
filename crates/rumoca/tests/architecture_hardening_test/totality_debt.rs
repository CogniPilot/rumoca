//! Accounting for run-time totality assertions in production back-end source.
//!
//! `scan` is the pure text scan; `gate` pins the per-crate counts. Both are
//! reachable from the other hardening gates, because "what does the shipped
//! code say" is one question and it must have one answer.

pub(crate) mod scan;

pub(crate) mod gate;
