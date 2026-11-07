//! Exact request validation and preparation identity owned by this backend.

use crate::VelImplicitConfig;
use rumoca_solver::{BackendOptionsError, NumericalBackend};

/// Process-local exact configuration value, not a digest or caller assertion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VelImplicitOptionsKey {
    step_bits: u64,
    jacobian_reuse: u32,
    sparse_lu: bool,
}

impl VelImplicitConfig {
    /// Validate an explicit request before any component construction/reset.
    /// The returned immutable value is the one the numerical factory consumes.
    pub fn validate_request(self) -> Result<Self, BackendOptionsError> {
        let field = if !self.step.is_finite() || self.step <= 0.0 {
            Some("step")
        } else if self.jacobian_reuse == 0 {
            Some("jacobian_reuse")
        } else {
            None
        };
        if let Some(field) = field {
            return Err(BackendOptionsError::InvalidValue {
                backend: NumericalBackend::VelImplicit,
                field,
            });
        }
        Ok(self)
    }

    /// Identity includes every concrete configuration field. The exhaustive
    /// destructure makes adding a new option require an explicit key decision.
    #[must_use]
    pub fn exact_key(self) -> VelImplicitOptionsKey {
        let Self {
            step,
            jacobian_reuse,
            sparse_lu,
        } = self;
        VelImplicitOptionsKey {
            step_bits: step.to_bits(),
            jacobian_reuse,
            sparse_lu,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_concrete_option_and_raw_float_bit_belongs_to_the_key() {
        let original = VelImplicitConfig::default();
        let variants = [
            original,
            VelImplicitConfig {
                step: f64::from_bits(original.step.to_bits() + 1),
                ..original
            },
            VelImplicitConfig {
                jacobian_reuse: original.jacobian_reuse + 1,
                ..original
            },
            VelImplicitConfig {
                sparse_lu: !original.sparse_lu,
                ..original
            },
        ];
        let keys: Vec<_> = variants
            .into_iter()
            .map(|config| {
                let validated = config.validate_request().unwrap();
                assert_eq!(validated, config);
                validated.exact_key()
            })
            .collect();
        for i in 0..keys.len() {
            for j in i + 1..keys.len() {
                assert_ne!(keys[i], keys[j]);
            }
        }
        assert_eq!(keys[0], VelImplicitConfig::default().exact_key());
    }

    #[test]
    fn invalid_explicit_configuration_has_a_typed_backend_failure() {
        let original = VelImplicitConfig::default();
        for step in [0.0, -0.0, -1.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
            let error = VelImplicitConfig { step, ..original }
                .validate_request()
                .unwrap_err();
            assert_eq!(
                error,
                BackendOptionsError::InvalidValue {
                    backend: NumericalBackend::VelImplicit,
                    field: "step",
                }
            );
        }
        let error = VelImplicitConfig {
            jacobian_reuse: 0,
            ..original
        }
        .validate_request()
        .unwrap_err();
        assert_eq!(
            error,
            BackendOptionsError::InvalidValue {
                backend: NumericalBackend::VelImplicit,
                field: "jacobian_reuse",
            }
        );
    }
}
