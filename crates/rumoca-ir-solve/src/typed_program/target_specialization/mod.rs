//! Final target-bound construction inputs. Compact product owners retain their
//! regions and placement facts until this boundary is selected.
//!
//! Version one admits only the existing dense host layout and loop choices.
//! It carries no timing estimate or WCET claim.

use serde::{Deserialize, Serialize};

use super::{MAX_UNROLLED_OPERATIONS, MAX_UNROLLED_PASSES, SolvePureCallTable};

mod materialize;
pub use materialize::SolveMaterializedJacobian;
#[cfg(test)]
mod tests;

/// Target declaration read from the target's `target.toml` specialization table.
/// Unknown fields and unsupported schema/choices are refused on decoding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(try_from = "Declaration")]
pub struct SolveTargetProfile {
    schema_version: u32,
    name: String,
    layout: SolveTargetLayout,
    max_unrolled_passes: u32,
    max_unrolled_operations: u32,
    admits_nested_map_frames: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SolveTargetLayout {
    DenseHostV1,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Declaration {
    schema_version: u32,
    name: String,
    layout: SolveTargetLayout,
    max_unrolled_passes: u32,
    max_unrolled_operations: u32,
    admits_nested_map_frames: bool,
}

/// The declaration cannot be honored by this specialization implementation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolveTargetProfileError(&'static str);

impl std::fmt::Display for SolveTargetProfileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl std::error::Error for SolveTargetProfileError {}

impl TryFrom<Declaration> for SolveTargetProfile {
    type Error = SolveTargetProfileError;

    fn try_from(value: Declaration) -> Result<Self, Self::Error> {
        if value.schema_version != 1 {
            return Err(SolveTargetProfileError(
                "unsupported target specialization schema",
            ));
        }
        if value.name.trim().is_empty() {
            return Err(SolveTargetProfileError("empty target specialization name"));
        }
        if value.max_unrolled_passes as usize != MAX_UNROLLED_PASSES
            || value.max_unrolled_operations as usize != MAX_UNROLLED_OPERATIONS
        {
            return Err(SolveTargetProfileError(
                "target expansion bounds are not implemented",
            ));
        }
        Ok(Self {
            schema_version: value.schema_version,
            name: value.name,
            layout: value.layout,
            max_unrolled_passes: value.max_unrolled_passes,
            max_unrolled_operations: value.max_unrolled_operations,
            admits_nested_map_frames: value.admits_nested_map_frames,
        })
    }
}

/// Selected target at the final Solve IR construction boundary. There is no
/// public unchecked constructor; the profile was checked on construction.
#[derive(Debug, Clone, Copy)]
pub struct SolveTargetSelection<'profile> {
    profile: &'profile SolveTargetProfile,
}

/// Identity of the actual issued program and selected target declaration.
/// Different target profiles never share a specialization cache key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct SolveSpecializationIdentity {
    program: [u8; 32],
    profile: [u8; 32],
}

impl SolveTargetProfile {
    /// Compatibility profile for callers that request today's portable dense
    /// host product explicitly. Target adapters supply their declared profile.
    #[must_use]
    pub fn current_dense_host() -> Self {
        Self {
            schema_version: 1,
            name: "portable-dense-host".into(),
            layout: SolveTargetLayout::DenseHostV1,
            max_unrolled_passes: MAX_UNROLLED_PASSES as u32,
            max_unrolled_operations: MAX_UNROLLED_OPERATIONS as u32,
            admits_nested_map_frames: false,
        }
    }

    #[must_use]
    pub fn select(&self) -> SolveTargetSelection<'_> {
        SolveTargetSelection { profile: self }
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl SolveTargetSelection<'_> {
    /// Whether the selected target's checked native entry accepts map frames.
    /// This is declaration data; compact construction owns any final expansion.
    #[must_use]
    pub const fn admits_nested_map_frames(&self) -> bool {
        self.profile.admits_nested_map_frames
    }

    #[must_use]
    pub const fn layout(&self) -> SolveTargetLayout {
        self.profile.layout
    }

    /// Bind the complete call owner table, not a detached operation count or
    /// entry label. The existing canonical Solve encoding excludes process-local
    /// ids and derived metadata and retains source byte ranges.
    pub fn bind(
        &self,
        program: &SolvePureCallTable,
    ) -> Result<SolveSpecializationIdentity, super::SolveAlgorithmBlockDigestError> {
        use super::algorithm_block::content_digest;
        Ok(SolveSpecializationIdentity {
            program: content_digest(b"rumoca.solve-specialization.program.v1\0", program)?,
            profile: content_digest(b"rumoca.solve-specialization.profile.v1\0", self.profile)?,
        })
    }
}

impl SolveSpecializationIdentity {
    #[must_use]
    pub const fn program(&self) -> &[u8; 32] {
        &self.program
    }

    #[must_use]
    pub const fn profile(&self) -> &[u8; 32] {
        &self.profile
    }
}
