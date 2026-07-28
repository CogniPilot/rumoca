//! SHA-1 checksums over exact raw bytes (GAL-021).
//!
//! eFMI checksums are SHA-1 per FIPS PUB 180-4 computed over the **binary
//! content as-is**: no line-ending or encoding normalization of any kind.
//! A wrong checksum makes the whole eFMU invalid, so this module never
//! produces placeholder values — a [`Sha1Hex`] either came from real bytes
//! ([`Sha1Hex::of_bytes`]) or from strict parsing ([`Sha1Hex::parse`]).

use std::fmt;
use std::fmt::Write as _;

use sha1::{Digest, Sha1};

use crate::manifest_context::diagnostic::EfmiError;

/// Length in bytes of a SHA-1 digest (FIPS 180-4: 160 bits).
const SHA1_BYTES: usize = 20;

/// Number of characters in the lowercase-hex rendering of a SHA-1 digest.
const SHA1_HEX_CHARS: usize = SHA1_BYTES * 2;

/// A SHA-1 digest.
///
/// The type holds the **digest**, not its rendering: 20 raw bytes, with the
/// 40-character lowercase hex form produced on demand by [`Display`] and
/// [`Sha1Hex::to_hex`]. That is what makes this value an identity rather than a
/// name — the bytes come from SHA-1 over exact content and are therefore
/// reproducible across processes and machines, whereas the hex string is one
/// possible spelling of them (upper- vs. lowercase differ as text but denote
/// the same digest). Equality, ordering, and hashing are over the digest.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sha1Hex([u8; SHA1_BYTES]);

impl Sha1Hex {
    /// Compute the SHA-1 of the given bytes, exactly as provided.
    pub fn of_bytes(bytes: &[u8]) -> Self {
        Self(Sha1::digest(bytes).into())
    }

    /// Parse a checksum this crate (or a peer tool following the same
    /// lowercase convention) previously emitted. Strict: exactly 40
    /// lowercase hex characters.
    pub fn parse(value: &str) -> Result<Self, EfmiError> {
        let invalid = |reason: &str| EfmiError::InvalidChecksum {
            value: value.to_owned(),
            reason: reason.to_owned(),
        };
        if value.len() != SHA1_HEX_CHARS {
            return Err(invalid("must be exactly 40 characters"));
        }
        let mut digest = [0u8; SHA1_BYTES];
        for (byte, pair) in digest.iter_mut().zip(value.as_bytes().chunks_exact(2)) {
            let Some(value) = lowercase_hex_byte(pair) else {
                return Err(invalid("must contain only lowercase hex characters"));
            };
            *byte = value;
        }
        Ok(Self(digest))
    }

    /// The digest bytes.
    pub fn as_bytes(&self) -> &[u8; SHA1_BYTES] {
        &self.0
    }

    /// The canonical 40-character lowercase hex rendering.
    pub fn to_hex(&self) -> String {
        let mut hex = String::with_capacity(SHA1_HEX_CHARS);
        for byte in self.0 {
            write!(hex, "{byte:02x}").expect("writing to a String cannot fail");
        }
        hex
    }
}

/// Decode one lowercase-hex digit pair, rejecting uppercase and non-hex.
fn lowercase_hex_byte(pair: &[u8]) -> Option<u8> {
    let digit = |byte: u8| match byte {
        b'0'..=b'9' => Some(byte - b'0'),
        b'a'..=b'f' => Some(byte - b'a' + 10),
        _ => None,
    };
    let (high, low) = (digit(*pair.first()?)?, digit(*pair.get(1)?)?);
    Some((high << 4) | low)
}

impl fmt::Display for Sha1Hex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in self.0 {
            write!(f, "{byte:02x}")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// FIPS 180-4 known-answer vectors.
    #[test]
    fn sha1_known_vectors() {
        assert_eq!(
            Sha1Hex::of_bytes(b"abc").to_hex(),
            "a9993e364706816aba3e25717850c26c9cd0d89d"
        );
        assert_eq!(
            Sha1Hex::of_bytes(b"").to_hex(),
            "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        );
    }

    /// The digest, not its spelling, is the identity: parsing the rendering of
    /// a digest yields the same value, and rendering round-trips exactly.
    #[test]
    fn sha1_round_trips_through_its_hex_rendering() {
        let digest = Sha1Hex::of_bytes(b"eFMI manifest content");
        let hex = digest.to_hex();
        assert_eq!(Sha1Hex::parse(&hex), Ok(digest));
        assert_eq!(digest.to_string(), hex);
        assert_eq!(hex.len(), 40);
    }

    /// No normalization: CRLF and LF content hash differently.
    #[test]
    fn sha1_is_over_exact_bytes() {
        assert_ne!(Sha1Hex::of_bytes(b"a\r\nb"), Sha1Hex::of_bytes(b"a\nb"));
    }

    #[test]
    fn parse_is_strict() {
        assert!(Sha1Hex::parse("a9993e364706816aba3e25717850c26c9cd0d89d").is_ok());
        for value in [
            "",
            "a9993e364706816aba3e25717850c26c9cd0d89",
            "a9993e364706816aba3e25717850c26c9cd0d89d0",
            "A9993E364706816ABA3E25717850C26C9CD0D89D",
            "z9993e364706816aba3e25717850c26c9cd0d89d",
        ] {
            let err = Sha1Hex::parse(value).expect_err(value);
            assert_eq!(err.code(), "EFM007", "wrong code for {value}");
        }
    }
}
