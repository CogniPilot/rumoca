//! Content digest of a refined algorithm block (#483).
//!
//! The C targets record this digest in every model unit they print, so a test
//! can recompute it from the checked package and prove which Solve program the
//! emitted C came from; a target name or label alone proves nothing.
//!
//! The digest hashes the block's serde form: slot table, method programs,
//! function owners, status bindings and provenance byte ranges. The analyses a
//! program derives from its operations are `serde(skip)`, and the issued plans
//! a printer reads from the wire form are marked derived and left out, so
//! neither is hashed twice. Pure-call sites serialize as owner ids (`call/wire_scope.rs`), owner
//! identities as their issue index and names as their text, so nothing
//! process-local reaches the hasher.
//!
//! The one thing left out is a span's `SourceId`, the hash of the source path:
//! the generated C is byte-identical from any checkout location (its traces
//! print paths relative to the traced files' common root), so the digest it
//! carries must be too. Byte ranges stay in, because the traces' line and
//! column come from them.
//!
//! The encoding is this module's own rather than a wire format's: every value
//! opens with a tag byte, scalars follow in fixed-width little-endian bytes,
//! text and bytes are length-prefixed and every compound value is closed by an
//! end tag. It is therefore unambiguous without knowing any length in advance,
//! and the same on every platform.

use serde::Serialize;
use serde::ser::{
    self, SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
    SerializeTupleStruct, SerializeTupleVariant,
};

use super::super::program::DERIVED;
use super::SolveAlgorithmBlock;

#[cfg(test)]
mod tests;

/// Bumped whenever the hashed byte form changes meaning.
const FORMAT_TAG: &[u8] = b"rumoca.solve-algorithm-block.v1\0";

/// The serde name of `rumoca_core::SourceId`, a derived newtype.
const SOURCE_ID: &str = "SourceId";

/// BLAKE3 digest of one [`SolveAlgorithmBlock`]'s content.
/// `Display` prints it as 64 lower-case hex digits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SolveAlgorithmBlockDigest([u8; 32]);

impl SolveAlgorithmBlockDigest {
    /// The hash function, as artifacts name it beside the hex digest.
    pub const ALGORITHM: &'static str = "BLAKE3";

    #[must_use]
    pub const fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

impl std::fmt::Display for SolveAlgorithmBlockDigest {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0
            .iter()
            .try_for_each(|byte| write!(formatter, "{byte:02x}"))
    }
}

/// A `Serialize` impl inside the block reported an error. No Solve type does
/// today; the error keeps that a reported failure rather than a digest of
/// partial content.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolveAlgorithmBlockDigestError(Box<str>);

impl std::fmt::Display for SolveAlgorithmBlockDigestError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "Solve algorithm block cannot be digested: {}",
            self.0
        )
    }
}

impl std::error::Error for SolveAlgorithmBlockDigestError {}

impl ser::Error for SolveAlgorithmBlockDigestError {
    fn custom<T: std::fmt::Display>(message: T) -> Self {
        Self(message.to_string().into())
    }
}

impl SolveAlgorithmBlock {
    /// The digest of this block's content; see the module docs for what it
    /// covers.
    pub fn digest(&self) -> Result<SolveAlgorithmBlockDigest, SolveAlgorithmBlockDigestError> {
        digest_of(self)
    }
}

fn digest_of<T: ?Sized + Serialize>(
    value: &T,
) -> Result<SolveAlgorithmBlockDigest, SolveAlgorithmBlockDigestError> {
    let mut writer = DigestWriter(blake3::Hasher::new());
    writer.0.update(FORMAT_TAG);
    value.serialize(&mut writer)?;
    Ok(SolveAlgorithmBlockDigest(*writer.0.finalize().as_bytes()))
}

/// Leading byte of every encoded value, and the end of every compound.
#[repr(u8)]
#[derive(Clone, Copy)]
enum Tag {
    Bool = 1,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Char,
    Str,
    Bytes,
    None,
    Some,
    Unit,
    UnitVariant,
    NewtypeVariant,
    SourceId,
    Seq,
    Tuple,
    TupleVariant,
    Map,
    Struct,
    StructVariant,
    /// A struct field its type skipped, so a skipped field and a present one
    /// never encode alike.
    Skipped,
    End,
}

struct DigestWriter(blake3::Hasher);

impl DigestWriter {
    fn tagged(&mut self, tag: Tag, bytes: &[u8]) {
        self.0.update(&[tag as u8]);
        self.0.update(bytes);
    }

    fn sized(&mut self, tag: Tag, bytes: &[u8]) {
        self.tagged(tag, &(bytes.len() as u64).to_le_bytes());
        self.0.update(bytes);
    }

    fn variant(&mut self, tag: Tag, index: u32) {
        self.tagged(tag, &index.to_le_bytes());
    }
}

type Done = Result<(), SolveAlgorithmBlockDigestError>;

macro_rules! fixed_width {
    ($($method:ident($ty:ty) => $tag:ident),* $(,)?) => {
        $(fn $method(self, value: $ty) -> Done {
            self.tagged(Tag::$tag, &value.to_le_bytes());
            Ok(())
        })*
    };
}

impl ser::Serializer for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;
    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fixed_width! {
        serialize_i8(i8) => I8,
        serialize_i16(i16) => I16,
        serialize_i32(i32) => I32,
        serialize_i64(i64) => I64,
        serialize_i128(i128) => I128,
        serialize_u8(u8) => U8,
        serialize_u16(u16) => U16,
        serialize_u32(u32) => U32,
        serialize_u64(u64) => U64,
        serialize_u128(u128) => U128,
    }

    fn serialize_bool(self, value: bool) -> Done {
        self.tagged(Tag::Bool, &[u8::from(value)]);
        Ok(())
    }

    /// Bits, so a NaN payload and the sign of zero are content.
    fn serialize_f32(self, value: f32) -> Done {
        self.tagged(Tag::F32, &value.to_bits().to_le_bytes());
        Ok(())
    }

    fn serialize_f64(self, value: f64) -> Done {
        self.tagged(Tag::F64, &value.to_bits().to_le_bytes());
        Ok(())
    }

    fn serialize_char(self, value: char) -> Done {
        self.tagged(Tag::Char, &u32::from(value).to_le_bytes());
        Ok(())
    }

    fn serialize_str(self, value: &str) -> Done {
        self.sized(Tag::Str, value.as_bytes());
        Ok(())
    }

    fn serialize_bytes(self, value: &[u8]) -> Done {
        self.sized(Tag::Bytes, value);
        Ok(())
    }

    fn serialize_none(self) -> Done {
        self.tagged(Tag::None, &[]);
        Ok(())
    }

    fn serialize_some<T: ?Sized + Serialize>(self, value: &T) -> Done {
        self.tagged(Tag::Some, &[]);
        value.serialize(self)
    }

    fn serialize_unit(self) -> Done {
        self.tagged(Tag::Unit, &[]);
        Ok(())
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Done {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
    ) -> Done {
        self.variant(Tag::UnitVariant, index);
        Ok(())
    }

    /// Transparent, except that a source id contributes only its tag and a
    /// value derived from hashed content contributes nothing.
    fn serialize_newtype_struct<T: ?Sized + Serialize>(
        self,
        name: &'static str,
        value: &T,
    ) -> Done {
        if name == DERIVED {
            return Ok(());
        }
        if name == SOURCE_ID {
            self.tagged(Tag::SourceId, &[]);
            return Ok(());
        }
        value.serialize(self)
    }

    fn serialize_newtype_variant<T: ?Sized + Serialize>(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Done {
        self.variant(Tag::NewtypeVariant, index);
        value.serialize(self)
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self, Self::Error> {
        self.tagged(Tag::Seq, &[]);
        Ok(self)
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self, Self::Error> {
        self.tagged(Tag::Tuple, &[]);
        Ok(self)
    }

    fn serialize_tuple_struct(self, _name: &'static str, len: usize) -> Result<Self, Self::Error> {
        self.serialize_tuple(len)
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self, Self::Error> {
        self.variant(Tag::TupleVariant, index);
        Ok(self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self, Self::Error> {
        self.tagged(Tag::Map, &[]);
        Ok(self)
    }

    /// Field names are fixed by the type, so only the values are hashed.
    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self, Self::Error> {
        self.tagged(Tag::Struct, &[]);
        Ok(self)
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self, Self::Error> {
        self.variant(Tag::StructVariant, index);
        Ok(self)
    }

    fn is_human_readable(&self) -> bool {
        false
    }
}

impl DigestWriter {
    fn close(&mut self) -> Done {
        self.tagged(Tag::End, &[]);
        Ok(())
    }
}

impl SerializeSeq for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }

    fn end(self) -> Done {
        self.close()
    }
}

impl SerializeTuple for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;

    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }

    fn end(self) -> Done {
        self.close()
    }
}

impl SerializeTupleStruct for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }

    fn end(self) -> Done {
        self.close()
    }
}

impl SerializeTupleVariant for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }

    fn end(self) -> Done {
        self.close()
    }
}

impl SerializeMap for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;

    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Done {
        key.serialize(&mut **self)
    }

    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Done {
        value.serialize(&mut **self)
    }

    fn end(self) -> Done {
        self.close()
    }
}

impl SerializeStruct for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, _key: &'static str, value: &T) -> Done {
        value.serialize(&mut **self)
    }

    fn skip_field(&mut self, _key: &'static str) -> Done {
        self.tagged(Tag::Skipped, &[]);
        Ok(())
    }

    fn end(self) -> Done {
        self.close()
    }
}

impl SerializeStructVariant for &mut DigestWriter {
    type Ok = ();
    type Error = SolveAlgorithmBlockDigestError;

    fn serialize_field<T: ?Sized + Serialize>(&mut self, _key: &'static str, value: &T) -> Done {
        value.serialize(&mut **self)
    }

    fn skip_field(&mut self, _key: &'static str) -> Done {
        self.tagged(Tag::Skipped, &[]);
        Ok(())
    }

    fn end(self) -> Done {
        self.close()
    }
}
