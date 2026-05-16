//! Codec facade for Rumoca.
//!
//! Front door for transport-neutral wire-format handling:
//!
//! - Re-exports `SignalFrame` (defined in `rumoca-signal-frame`).
//! - Defines abstract `PackCodec` / `UnpackCodec` traits — the contract
//!   every wire-format implementation provides.
//! - Re-exports the typed config (`SchemaConfig`, `MessageConfig`) consumed
//!   by today's flatbuffers backend.
//! - Provides `build_pack` / `build_unpack` factories that load the schema
//!   and instantiate concrete codecs behind boxed trait objects.
//!
//! Sim and other consumers depend only on this crate; concrete impl crates
//! (`rumoca-codec-flatbuffers`, future `rumoca-codec-protobuf`, etc.) hang
//! off this crate, not the other way around.

use std::path::Path;

pub use rumoca_signal_frame::SignalFrame;

/// Configuration types — re-exported from the active backend so consumers
/// don't need to name the backend crate.
pub mod config {
    pub use rumoca_codec_flatbuffers::config::{MessageConfig, SchemaConfig};
}

/// Encode a `SignalFrame` into wire bytes.
pub trait PackCodec: Send + Sync {
    fn pack(&self, frame: &SignalFrame) -> Vec<u8>;
}

/// Decode wire bytes into a `SignalFrame`.
pub trait UnpackCodec: Send + Sync {
    fn unpack(&self, bytes: &[u8]) -> SignalFrame;
    fn expected_size(&self) -> usize;
}

// ──────────────────────────────────────────────────────────────────────────
// FlatBuffers backend adapters.
//
// `rumoca-codec-flatbuffers` exposes concrete `PackCodec` / `UnpackCodec`
// types that don't implement our abstract traits (and can't, since that
// crate doesn't depend on this one — preserves the dep direction).  The
// adapter structs below wrap the concrete types and implement the traits.
// ──────────────────────────────────────────────────────────────────────────

struct FlatbuffersPack(rumoca_codec_flatbuffers::codec::PackCodec);

impl PackCodec for FlatbuffersPack {
    fn pack(&self, frame: &SignalFrame) -> Vec<u8> {
        self.0.pack(frame)
    }
}

struct FlatbuffersUnpack(rumoca_codec_flatbuffers::codec::UnpackCodec);

impl UnpackCodec for FlatbuffersUnpack {
    fn unpack(&self, bytes: &[u8]) -> SignalFrame {
        self.0.unpack(bytes)
    }
    fn expected_size(&self) -> usize {
        self.0.expected_size()
    }
}

fn load_flatbuffers_schema(
    schema: &config::SchemaConfig,
) -> anyhow::Result<rumoca_codec_flatbuffers::bfbs::SchemaSet> {
    let mut set = rumoca_codec_flatbuffers::bfbs::SchemaSet::new();
    for path in &schema.bfbs {
        set.load_bfbs(Path::new(path))?;
    }
    Ok(set)
}

/// Build a packing codec for the active wire format from a typed config.
///
/// Today this dispatches to the FlatBuffers backend. When alternate
/// backends (protobuf, CDR, …) are added, dispatch will key off a `kind`
/// discriminator on the config.
pub fn build_pack(
    schema: &config::SchemaConfig,
    send: &config::MessageConfig,
) -> anyhow::Result<Box<dyn PackCodec>> {
    let schema_set = load_flatbuffers_schema(schema)?;
    let codec = rumoca_codec_flatbuffers::codec::PackCodec::compile(&schema_set, send)?;
    Ok(Box::new(FlatbuffersPack(codec)))
}

/// Build an unpacking codec for the active wire format from a typed config.
pub fn build_unpack(
    schema: &config::SchemaConfig,
    receive: &config::MessageConfig,
) -> anyhow::Result<Box<dyn UnpackCodec>> {
    let schema_set = load_flatbuffers_schema(schema)?;
    let codec = rumoca_codec_flatbuffers::codec::UnpackCodec::compile(&schema_set, receive)?;
    Ok(Box::new(FlatbuffersUnpack(codec)))
}
