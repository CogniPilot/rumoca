//! Projection input and options.
//!
//! [`GalecInput`] borrows the untouched canonical DAE (GAL-002: the
//! projection never mutates it). Checked DAE variables carry their canonical
//! scalar type, shape, causality, attributes, and declaration provenance, so
//! the projection needs no Flat-side compatibility map or reconstructed
//! metadata.

use rumoca_ir_dae::Dae;
use rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile;

/// Options controlling the projection.
///
/// Executable arithmetic must be selected explicitly at the calling boundary;
/// generic default construction is intentionally unavailable.
///
/// ```compile_fail
/// fn require_default<T: Default>() {}
/// require_default::<rumoca_phase_galec::GalecOptions>();
/// ```
///
/// ```compile_fail
/// let _ = rumoca_phase_galec::GalecOptions::default();
/// ```
///
#[derive(Debug, Clone)]
pub struct GalecOptions {
    /// Override for the emitted GALEC block name. When `None`, the block is
    /// named after [`GalecInput::model_name`] (mangled per GAL-015 if the
    /// Modelica name is not a legal GALEC identifier).
    pub block_name: Option<String>,
    /// Value-affecting arithmetic fixed in the constructed package.
    pub arithmetic_profile: AlgorithmCodeArithmeticProfile,
}

impl GalecOptions {
    /// Build projection options after the calling boundary has explicitly
    /// resolved value-affecting arithmetic.
    #[must_use]
    pub fn new(arithmetic_profile: AlgorithmCodeArithmeticProfile) -> Self {
        Self {
            block_name: None,
            arithmetic_profile,
        }
    }

    /// Override the emitted block name without changing either semantic input.
    #[must_use]
    pub fn with_block_name(mut self, block_name: impl Into<String>) -> Self {
        self.block_name = Some(block_name.into());
        self
    }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constructor_retains_the_explicit_arithmetic_selection() {
        for semantics in [
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
        ] {
            let options = GalecOptions::new(AlgorithmCodeArithmeticProfile::construct(
                rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
                rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
                semantics,
            ));
            assert_eq!(
                options.arithmetic_profile.source_real(),
                rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64
            );
            assert_eq!(options.arithmetic_profile.real_matrix_multiply(), semantics);
        }
    }
}
