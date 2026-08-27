//! Projection input and options.
//!
//! [`GalecInput`] borrows the untouched canonical DAE (GAL-002: the
//! projection never mutates it). Checked DAE variables carry their canonical
//! scalar type, shape, causality, attributes, and declaration provenance, so
//! the projection needs no Flat-side compatibility map or reconstructed
//! metadata.

use rumoca_ir_dae::Dae;
// SPEC_0029 §8: imported, never re-exported. `EmissionPolicy` is owned by
// `rumoca-ir-galec` because the projected package carries it to the targets
// that must disclose it, so every consumer names that crate directly.
use rumoca_ir_galec::package::EmissionPolicy;

/// Options controlling the projection.
#[derive(Debug, Clone, Default)]
pub struct GalecOptions {
    /// Override for the emitted GALEC block name. When `None`, the block is
    /// named after [`GalecInput::model_name`] (mangled per GAL-015 if the
    /// Modelica name is not a legal GALEC identifier).
    pub block_name: Option<String>,
    /// The two axes the projection emits under: how much call structure it
    /// keeps, and whether it may expand a tensor operation.
    ///
    /// The default keeps every tensor, so a caller that does not think about
    /// this gets an artifact that is still eligible for the certification path.
    /// It does NOT decline inlining, because inlining takes nothing away.
    pub emission_policy: EmissionPolicy,
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
