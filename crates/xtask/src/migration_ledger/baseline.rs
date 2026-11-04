//! Verify supplied old bytes against the immutable audit, without opening paths.
//!
//! A caller may obtain these bytes from Git or an archive. Neither that origin
//! nor a supplied hash grants trust: the embedded, hash-pinned ledger must match.
//! This is a source identity fact only, never authorization to replace a region.

#[cfg(test)]
mod tests;

use super::{Ledger, REFERENCE, REFERENCE_SHA256, digest, parse_ledger, relative_path};
use anyhow::{Result, ensure};

/// One checked immutable ledger. No caller-provided reference is accepted.
pub struct FrozenBaseline {
    ledger: Ledger,
}

/// The exact borrowed bytes verified for one path of one baseline owner.
pub struct FrozenSource<'baseline, 'bytes> {
    owner: &'baseline FrozenBaseline,
    path: &'baseline str,
    bytes: &'bytes [u8],
}

impl FrozenBaseline {
    /// Check the compiled reference before accepting any proposed baseline data.
    pub fn checked() -> Result<Self> {
        ensure!(
            digest(REFERENCE.as_bytes()) == REFERENCE_SHA256,
            "immutable audit baseline changed"
        );
        Ok(Self {
            ledger: parse_ledger(REFERENCE)?,
        })
    }

    /// Verify bytes without rereading a filesystem or normalizing their content.
    pub fn source<'baseline, 'bytes>(
        &'baseline self,
        path: &str,
        bytes: &'bytes [u8],
    ) -> Result<FrozenSource<'baseline, 'bytes>> {
        relative_path(path)?;
        let Some((path, expected)) = self.ledger.files.get_key_value(path) else {
            anyhow::bail!("source is absent from the immutable baseline");
        };
        ensure!(bytes.len() <= 16 * 1024 * 1024, "baseline source too large");
        ensure!(digest(bytes) == *expected, "baseline source bytes differ");
        Ok(FrozenSource {
            owner: self,
            path,
            bytes,
        })
    }
}

impl FrozenSource<'_, '_> {
    /// Preserve the exact checked path and byte slice for the complement check.
    pub fn path(&self) -> &str {
        self.path
    }

    pub fn bytes(&self) -> &[u8] {
        self.bytes
    }

    /// Equal ledger contents do not substitute for the same checked owner.
    pub fn check_owner(&self, owner: &FrozenBaseline) -> Result<()> {
        ensure!(std::ptr::eq(self.owner, owner), "foreign baseline owner");
        Ok(())
    }
}
