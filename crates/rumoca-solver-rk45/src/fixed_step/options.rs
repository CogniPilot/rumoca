//! Exact request validation and preparation identity owned by this backend.

use super::{FixedStepConfig, Method};
use rumoca_solver::{BackendOptionsError, NumericalBackend};

/// Process-local exact configuration value, not a digest or caller assertion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FixedStepOptionsKey {
    step_bits: u64,
    order: u32,
}

impl FixedStepConfig {
    /// Validate an explicit request before any component construction/reset.
    /// The returned immutable value is the one the numerical factory consumes.
    pub fn validate_request(self) -> Result<Self, BackendOptionsError> {
        if !self.step.is_finite() || self.step <= 0.0 {
            return Err(BackendOptionsError::InvalidValue {
                backend: NumericalBackend::FixedStep,
                field: "step",
            });
        }
        if Method::from_order(self.order).is_none() {
            return Err(invalid_order());
        }
        Ok(self)
    }

    /// Identity includes every concrete configuration field. The exhaustive
    /// destructure makes adding a new option require an explicit key decision.
    #[must_use]
    pub fn exact_key(self) -> FixedStepOptionsKey {
        let Self { step, order } = self;
        FixedStepOptionsKey {
            step_bits: step.to_bits(),
            order,
        }
    }
}

/// The typed failure of an order no method here implements.
pub(super) fn invalid_order() -> BackendOptionsError {
    BackendOptionsError::InvalidValue {
        backend: NumericalBackend::FixedStep,
        field: "order",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_default_is_a_one_millisecond_classical_step() {
        assert_eq!(
            FixedStepConfig::default(),
            FixedStepConfig {
                step: 1.0e-3,
                order: 4,
            }
        );
    }

    #[test]
    fn every_concrete_option_and_raw_float_bit_belongs_to_the_key() {
        let original = FixedStepConfig::default();
        let variants = [
            original,
            FixedStepConfig {
                step: f64::from_bits(original.step.to_bits() + 1),
                ..original
            },
            FixedStepConfig {
                order: 2,
                ..original
            },
            FixedStepConfig {
                order: 1,
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
        assert_eq!(keys[0], FixedStepConfig::default().exact_key());
    }

    #[test]
    fn invalid_explicit_configuration_has_a_typed_backend_failure() {
        let original = FixedStepConfig::default();
        for step in [0.0, -0.0, -1.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
            let error = FixedStepConfig { step, ..original }
                .validate_request()
                .unwrap_err();
            assert_eq!(
                error,
                BackendOptionsError::InvalidValue {
                    backend: NumericalBackend::FixedStep,
                    field: "step",
                }
            );
        }
        for order in [0, 3, 5, 8, u32::MAX] {
            let error = FixedStepConfig { order, ..original }
                .validate_request()
                .unwrap_err();
            assert_eq!(
                error,
                BackendOptionsError::InvalidValue {
                    backend: NumericalBackend::FixedStep,
                    field: "order",
                }
            );
        }
        for order in [1, 2, 4] {
            let config = FixedStepConfig { order, ..original };
            assert_eq!(config.validate_request(), Ok(config));
        }
    }
}
