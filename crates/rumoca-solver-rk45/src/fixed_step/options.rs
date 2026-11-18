//! Exact request validation and preparation identity owned by this backend.

use super::{FixedStepConfig, FixedStepScheme, Method};
use rumoca_solver::{BackendOptionsError, NumericalBackend};

/// Process-local exact configuration value, not a digest or caller assertion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FixedStepOptionsKey {
    step_bits: u64,
    order: u32,
    scheme: FixedStepScheme,
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
        match Method::from_config(self.order, self.scheme) {
            None => Err(invalid_order()),
            Some(None) => Err(invalid_scheme()),
            Some(Some(_)) => Ok(self),
        }
    }

    /// Identity includes every concrete configuration field. The exhaustive
    /// destructure makes adding a new option require an explicit key decision.
    #[must_use]
    pub fn exact_key(self) -> FixedStepOptionsKey {
        let Self {
            step,
            order,
            scheme,
        } = self;
        FixedStepOptionsKey {
            step_bits: step.to_bits(),
            order,
            scheme,
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

/// The typed failure of a scheme the configured order does not admit.
pub(super) fn invalid_scheme() -> BackendOptionsError {
    BackendOptionsError::InvalidValue {
        backend: NumericalBackend::FixedStep,
        field: "scheme",
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
                scheme: FixedStepScheme::Explicit,
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

    #[test]
    fn the_semi_implicit_scheme_is_its_own_key_and_needs_order_one() {
        let explicit = FixedStepConfig {
            order: 1,
            ..FixedStepConfig::default()
        };
        let semi_implicit = FixedStepConfig {
            scheme: FixedStepScheme::SemiImplicit,
            ..explicit
        };
        assert_eq!(semi_implicit.validate_request(), Ok(semi_implicit));
        assert_ne!(semi_implicit.exact_key(), explicit.exact_key());
        for order in [2, 4] {
            let error = FixedStepConfig {
                order,
                ..semi_implicit
            }
            .validate_request()
            .unwrap_err();
            assert_eq!(error, invalid_scheme());
        }
        // An order no method implements is that field's failure first.
        for order in [0, 3] {
            let error = FixedStepConfig {
                order,
                ..semi_implicit
            }
            .validate_request()
            .unwrap_err();
            assert_eq!(error, invalid_order());
        }
    }
}
