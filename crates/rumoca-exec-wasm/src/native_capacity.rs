//! Wire transport of Solve-issued native source and memory32 target bounds.

mod envelope;
pub use envelope::{decode_native_source_envelope, encode_native_source_envelope};
