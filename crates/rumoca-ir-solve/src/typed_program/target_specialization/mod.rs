//! Final target-bound construction inputs. Compact product owners retain their
//! regions and placement facts until this boundary is selected.
//!
//! Version two admits only the existing dense host layout and loop choices,
//! and adds two required resource declarations: the call-partition bound and
//! the local value budget. Version one declarations are refused. It carries no
//! timing estimate or WCET claim.

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
    max_call_partition_operations: u32,
    local_budget_bytes: u32,
}

/// The only target specialization schema this implementation accepts.
const SCHEMA_VERSION: u32 = 2;

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
    /// The most root operations one issued call partition may hold. Regions
    /// stay atomic and each partition retains one root frame; this bounds
    /// neither expanded instructions nor frame bytes. Required by version two;
    /// absence decodes so a version one file reaches the migration refusal.
    #[serde(default)]
    max_call_partition_operations: Option<u32>,
    /// The most bytes of simultaneously live register values the target may
    /// keep in non-memory locals at a program point. Live values beyond it are
    /// placed in explicit workspace cells. A register costs its scalar count
    /// times its element size: binary64 and Integer 8, binary32 4, Boolean 8.
    /// Required by version two, with no default.
    #[serde(default)]
    local_budget_bytes: Option<u32>,
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
        if value.schema_version == 1 {
            return Err(SolveTargetProfileError(
                "target specialization schema 1 is no longer supported; declare \
                 schema_version = 2 with max_call_partition_operations and local_budget_bytes",
            ));
        }
        if value.schema_version != SCHEMA_VERSION {
            return Err(SolveTargetProfileError(
                "unsupported target specialization schema",
            ));
        }
        if value.name.trim().is_empty() {
            return Err(SolveTargetProfileError("empty target specialization name"));
        }
        let Some(max_call_partition_operations) = value.max_call_partition_operations else {
            return Err(SolveTargetProfileError(
                "max_call_partition_operations is required",
            ));
        };
        let Some(local_budget_bytes) = value.local_budget_bytes else {
            return Err(SolveTargetProfileError("local_budget_bytes is required"));
        };
        if max_call_partition_operations == 0 {
            return Err(SolveTargetProfileError(
                "call partition bound must be nonzero",
            ));
        }
        if local_budget_bytes == 0 {
            return Err(SolveTargetProfileError("local budget must be positive"));
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
            max_call_partition_operations,
            local_budget_bytes,
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
            schema_version: SCHEMA_VERSION,
            name: "portable-dense-host".into(),
            layout: SolveTargetLayout::DenseHostV1,
            max_unrolled_passes: MAX_UNROLLED_PASSES as u32,
            max_unrolled_operations: MAX_UNROLLED_OPERATIONS as u32,
            admits_nested_map_frames: false,
            max_call_partition_operations: 256,
            local_budget_bytes: 1 << 20,
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

    /// The declared root-operation bound per issued call partition.
    #[must_use]
    pub const fn max_call_partition_operations(&self) -> u32 {
        self.profile.max_call_partition_operations
    }

    /// The declared byte budget for simultaneously live local register values.
    #[must_use]
    pub const fn local_budget_bytes(&self) -> u32 {
        self.profile.local_budget_bytes
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
