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

use crate::diagnostic::EfmiError;

/// A SHA-1 digest rendered as exactly 40 lowercase hex characters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sha1Hex(String);

impl Sha1Hex {
    /// Compute the SHA-1 of the given bytes, exactly as provided.
    pub fn of_bytes(bytes: &[u8]) -> Self {
        let digest = Sha1::digest(bytes);
        let mut hex = String::with_capacity(40);
        for byte in digest {
            write!(hex, "{byte:02x}").expect("writing to a String cannot fail");
        }
        Self(hex)
    }

    /// Parse a checksum this crate (or a peer tool following the same
    /// lowercase convention) previously emitted. Strict: exactly 40
    /// lowercase hex characters.
    pub fn parse(value: &str) -> Result<Self, EfmiError> {
        let invalid = |reason: &str| EfmiError::InvalidChecksum {
            value: value.to_owned(),
            reason: reason.to_owned(),
        };
        if value.len() != 40 {
            return Err(invalid("must be exactly 40 characters"));
        }
        if !value
            .chars()
            .all(|c| c.is_ascii_digit() || ('a'..='f').contains(&c))
        {
            return Err(invalid("must contain only lowercase hex characters"));
        }
        Ok(Self(value.to_owned()))
    }

    /// The validated hex text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Sha1Hex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// FIPS 180-4 known-answer vectors.
    #[test]
    fn sha1_known_vectors() {
        assert_eq!(
            Sha1Hex::of_bytes(b"abc").as_str(),
            "a9993e364706816aba3e25717850c26c9cd0d89d"
        );
        assert_eq!(
            Sha1Hex::of_bytes(b"").as_str(),
            "da39a3ee5e6b4b0d3255bfef95601890afd80709"
        );
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
