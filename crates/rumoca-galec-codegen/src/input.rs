//! Projection input and options.
//!
//! [`GalecInput`] borrows the untouched canonical DAE (GAL-002: the
//! projection never mutates it). Checked DAE variables carry their canonical
//! scalar type, shape, causality, attributes, and declaration provenance, so
//! the projection needs no Flat-side compatibility map or reconstructed
//! metadata.

use rumoca_ir_dae::Dae;

/// Pinned eFMI profile the projection emits for (SPEC_0034 GAL-022).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum GalecProfile {
    /// eFMI Standard 1.0.0 Beta 1 (`efmi-1.0.0-beta-1`), the only profile
    /// currently supported.
    #[default]
    Efmi1_0_0Beta1,
}

impl GalecProfile {
    /// The eFMI profile string (GAL-022).
    #[must_use]
    pub const fn profile_string(self) -> &'static str {
        match self {
            Self::Efmi1_0_0Beta1 => crate::manifest_context::EFMI_PROFILE,
        }
    }
}

/// Options controlling the projection.
#[derive(Debug, Clone, Default)]
pub struct GalecOptions {
    /// Target eFMI profile.
    pub profile: GalecProfile,
    /// Override for the emitted GALEC block name. When `None`, the block is
    /// named after [`GalecInput::model_name`] (mangled per GAL-015 if the
    /// Modelica name is not a legal GALEC identifier).
    pub block_name: Option<String>,
}

/// Borrowed projection input: the untouched canonical DAE plus auxiliary
/// provenance (never stored, never mutated — GAL-002).
#[derive(Debug, Clone, Copy)]
pub struct GalecInput<'a> {
    /// The canonical DAE, read-only.
    pub dae: &'a Dae,
    /// Name of the compiled root model. The DAE itself carries no model
    /// name; the caller (CLI / `rumoca-compile` session) supplies it.
    pub model_name: &'a str,
}

impl<'a> GalecInput<'a> {
    /// Input over an untouched DAE without type provenance.
    #[must_use]
    pub fn new(dae: &'a Dae, model_name: &'a str) -> Self {
        Self { dae, model_name }
    }
}
