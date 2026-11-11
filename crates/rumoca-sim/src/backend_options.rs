//! Validate concrete immutable options before model work and before cache reuse.

use rumoca_solver::{
    BackendOptions, BackendOptionsError, NumericalBackend, SimOptions, SimSolverMode,
};

/// Capture a `fixed-step` configuration a host received as plain numbers,
/// such as a binding's options document.
///
/// Nothing is decided here. The selected backend checks identity, concrete
/// type, build availability and both values before any model work or cache
/// reuse, exactly as it does for every other payload, so an invalid step or
/// order is that owner's typed [`BackendOptionsError`].
#[must_use]
pub fn fixed_step_backend_options(step: f64, order: u32) -> BackendOptions {
    #[cfg(feature = "solver-rk45")]
    let payload = rumoca_solver_rk45::FixedStepConfig {
        step,
        order,
        scheme: rumoca_solver_rk45::FixedStepScheme::Explicit,
    };
    // Without the backend the request is refused as unavailable before its
    // concrete type is read.
    #[cfg(not(feature = "solver-rk45"))]
    let payload = (step, order);
    BackendOptions::new(NumericalBackend::FixedStep, payload)
}

/// Capture a `fixed-step` configuration with the semi-implicit scheme, as
/// [`fixed_step_backend_options`] does: the backend refuses an order the
/// scheme does not admit.
#[must_use]
pub fn fixed_step_semi_implicit_backend_options(step: f64, order: u32) -> BackendOptions {
    #[cfg(feature = "solver-rk45")]
    let payload = rumoca_solver_rk45::FixedStepConfig {
        step,
        order,
        scheme: rumoca_solver_rk45::FixedStepScheme::SemiImplicit,
    };
    #[cfg(not(feature = "solver-rk45"))]
    let payload = (step, order);
    BackendOptions::new(NumericalBackend::FixedStep, payload)
}

/// Only backend-owned, immutable, value types pass this boundary. No custom
/// erased implementation supplies validation, effective values or cache keys.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ValidatedOptions {
    Defaults,
    #[cfg(feature = "solver-diffsol")]
    Bdf(rumoca_solver_diffsol::DiffsolOptions),
    #[cfg(feature = "solver-rk45")]
    Rk45(rumoca_solver_rk45::Rk45Options),
    #[cfg(feature = "solver-velimplicit")]
    VelImplicit(rumoca_solver_velimplicit::VelImplicitConfig),
    #[cfg(feature = "solver-rk45")]
    FixedStep(rumoca_solver_rk45::FixedStepConfig),
}

/// Exact value identity of the validated configuration, including presence.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum OptionsIdentity {
    Absent,
    #[cfg(feature = "solver-diffsol")]
    Bdf(rumoca_solver_diffsol::DiffsolOptions),
    #[cfg(feature = "solver-rk45")]
    Rk45(rumoca_solver_rk45::Rk45Options),
    #[cfg(feature = "solver-velimplicit")]
    VelImplicit(rumoca_solver_velimplicit::VelImplicitOptionsKey),
    #[cfg(feature = "solver-rk45")]
    FixedStep(rumoca_solver_rk45::FixedStepOptionsKey),
}

impl ValidatedOptions {
    pub(crate) fn identity(self) -> OptionsIdentity {
        match self {
            Self::Defaults => OptionsIdentity::Absent,
            #[cfg(feature = "solver-diffsol")]
            Self::Bdf(options) => OptionsIdentity::Bdf(options),
            #[cfg(feature = "solver-rk45")]
            Self::Rk45(options) => OptionsIdentity::Rk45(options),
            #[cfg(feature = "solver-velimplicit")]
            Self::VelImplicit(options) => OptionsIdentity::VelImplicit(options.exact_key()),
            #[cfg(feature = "solver-rk45")]
            Self::FixedStep(options) => OptionsIdentity::FixedStep(options.exact_key()),
        }
    }
}

pub(crate) fn validate(options: &SimOptions) -> Result<ValidatedOptions, BackendOptionsError> {
    let Some(payload) = &options.backend_options else {
        return Ok(ValidatedOptions::Defaults);
    };
    let requested = match options.solver_mode {
        SimSolverMode::Auto => return Err(BackendOptionsError::CustomAuto),
        SimSolverMode::Bdf => NumericalBackend::Bdf,
        SimSolverMode::RkLike => NumericalBackend::Rk45,
        SimSolverMode::VelImplicit => NumericalBackend::VelImplicit,
        SimSolverMode::FixedStep => NumericalBackend::FixedStep,
    };
    if requested != payload.backend() {
        return Err(BackendOptionsError::BackendMismatch {
            requested,
            supplied: payload.backend(),
        });
    }
    match requested {
        #[cfg(feature = "solver-diffsol")]
        NumericalBackend::Bdf => concrete(payload).map(ValidatedOptions::Bdf),
        #[cfg(feature = "solver-rk45")]
        NumericalBackend::Rk45 => concrete(payload).map(ValidatedOptions::Rk45),
        #[cfg(feature = "solver-velimplicit")]
        NumericalBackend::VelImplicit => validate_velimplicit(payload),
        #[cfg(feature = "solver-rk45")]
        NumericalBackend::FixedStep => validate_fixed_step(payload),
        #[cfg(not(feature = "solver-diffsol"))]
        backend @ NumericalBackend::Bdf => Err(BackendOptionsError::BackendUnavailable { backend }),
        #[cfg(not(feature = "solver-rk45"))]
        backend @ (NumericalBackend::Rk45 | NumericalBackend::FixedStep) => {
            Err(BackendOptionsError::BackendUnavailable { backend })
        }
        #[cfg(not(feature = "solver-velimplicit"))]
        backend @ NumericalBackend::VelImplicit => {
            Err(BackendOptionsError::BackendUnavailable { backend })
        }
    }
}

fn concrete<T: Copy + 'static>(payload: &BackendOptions) -> Result<T, BackendOptionsError> {
    payload
        .downcast_ref::<T>()
        .copied()
        .ok_or(BackendOptionsError::ConcreteType {
            backend: payload.backend(),
        })
}

#[cfg(feature = "solver-velimplicit")]
fn validate_velimplicit(payload: &BackendOptions) -> Result<ValidatedOptions, BackendOptionsError> {
    let value: rumoca_solver_velimplicit::VelImplicitConfig = concrete(payload)?;
    value.validate_request().map(ValidatedOptions::VelImplicit)
}

#[cfg(feature = "solver-rk45")]
fn validate_fixed_step(payload: &BackendOptions) -> Result<ValidatedOptions, BackendOptionsError> {
    let value: rumoca_solver_rk45::FixedStepConfig = concrete(payload)?;
    value.validate_request().map(ValidatedOptions::FixedStep)
}

/// Bind explicit configuration to the factory actually used, including direct
/// backend entry points that bypass the public solver-mode dispatcher.
pub(crate) fn validate_for_backend(
    options: &SimOptions,
    actual: NumericalBackend,
) -> Result<ValidatedOptions, BackendOptionsError> {
    let value = validate(options)?;
    if let Some(payload) = &options.backend_options
        && payload.backend() != actual
    {
        return Err(BackendOptionsError::BackendMismatch {
            requested: actual,
            supplied: payload.backend(),
        });
    }
    Ok(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn absent_options_keep_auto_defaults_but_explicit_payload_does_not() {
        let mut opts = SimOptions::default();
        assert_eq!(validate(&opts).unwrap().identity(), OptionsIdentity::Absent);
        opts.backend_options = Some(BackendOptions::new(NumericalBackend::Bdf, 7_u32));
        assert_eq!(
            validate(&opts).unwrap_err(),
            BackendOptionsError::CustomAuto
        );
        opts.solver_mode = SimSolverMode::RkLike;
        assert!(matches!(
            validate(&opts),
            Err(BackendOptionsError::BackendMismatch {
                requested: NumericalBackend::Rk45,
                supplied: NumericalBackend::Bdf
            })
        ));
    }

    #[cfg(feature = "solver-rk45")]
    #[test]
    fn rk_payload_requires_its_actual_concrete_type_and_actual_factory() {
        let mut opts = SimOptions {
            solver_mode: SimSolverMode::RkLike,
            ..Default::default()
        };
        opts.backend_options = Some(BackendOptions::new(NumericalBackend::Rk45, 1_u32));
        assert_eq!(
            validate(&opts).unwrap_err(),
            BackendOptionsError::ConcreteType {
                backend: NumericalBackend::Rk45
            }
        );
        opts.backend_options = Some(BackendOptions::new(
            NumericalBackend::Rk45,
            rumoca_solver_rk45::Rk45Options,
        ));
        assert_eq!(
            validate(&opts).unwrap().identity(),
            OptionsIdentity::Rk45(rumoca_solver_rk45::Rk45Options)
        );
        assert!(matches!(
            validate_for_backend(&opts, NumericalBackend::Bdf),
            Err(BackendOptionsError::BackendMismatch { .. })
        ));
        assert_eq!(
            validate_for_backend(&opts, NumericalBackend::Rk45)
                .unwrap()
                .identity(),
            OptionsIdentity::Rk45(rumoca_solver_rk45::Rk45Options)
        );
    }

    #[cfg(feature = "solver-diffsol")]
    #[test]
    fn bdf_default_presence_and_concrete_schema_are_exact() {
        let mut opts = SimOptions {
            solver_mode: SimSolverMode::Bdf,
            ..Default::default()
        };
        let absent = validate(&opts).unwrap().identity();
        opts.backend_options = Some(BackendOptions::new(
            NumericalBackend::Bdf,
            rumoca_solver_diffsol::DiffsolOptions::default(),
        ));
        let explicit = validate_for_backend(&opts, NumericalBackend::Bdf)
            .unwrap()
            .identity();
        assert_eq!(
            explicit,
            OptionsIdentity::Bdf(rumoca_solver_diffsol::DiffsolOptions::default())
        );
        assert_ne!(absent, explicit);
        opts.backend_options = Some(BackendOptions::new(NumericalBackend::Bdf, 9_u32));
        assert_eq!(
            validate(&opts).unwrap_err(),
            BackendOptionsError::ConcreteType {
                backend: NumericalBackend::Bdf
            }
        );
    }

    #[cfg(feature = "solver-velimplicit")]
    #[test]
    fn velimplicit_rejects_invalid_values_and_keys_every_consumed_field() {
        let mut opts = SimOptions {
            solver_mode: SimSolverMode::VelImplicit,
            ..Default::default()
        };
        let original = rumoca_solver_velimplicit::VelImplicitConfig::default();
        let mut keys = Vec::new();
        for value in [
            original,
            rumoca_solver_velimplicit::VelImplicitConfig {
                step: f64::from_bits(original.step.to_bits() + 1),
                ..original
            },
            rumoca_solver_velimplicit::VelImplicitConfig {
                jacobian_reuse: original.jacobian_reuse + 1,
                ..original
            },
            rumoca_solver_velimplicit::VelImplicitConfig {
                sparse_lu: !original.sparse_lu,
                ..original
            },
        ] {
            opts.backend_options = Some(BackendOptions::new(NumericalBackend::VelImplicit, value));
            keys.push(
                validate_for_backend(&opts, NumericalBackend::VelImplicit)
                    .unwrap()
                    .identity(),
            );
        }
        for i in 0..keys.len() {
            for j in i + 1..keys.len() {
                assert_ne!(keys[i], keys[j]);
            }
        }
        for step in [0.0, -0.0, -1.0, f64::INFINITY, f64::NAN] {
            opts.backend_options = Some(BackendOptions::new(
                NumericalBackend::VelImplicit,
                rumoca_solver_velimplicit::VelImplicitConfig { step, ..original },
            ));
            assert!(matches!(
                validate(&opts),
                Err(BackendOptionsError::InvalidValue { field: "step", .. })
            ));
        }
        opts.backend_options = Some(BackendOptions::new(
            NumericalBackend::VelImplicit,
            rumoca_solver_velimplicit::VelImplicitConfig {
                jacobian_reuse: 0,
                ..original
            },
        ));
        assert!(matches!(
            validate(&opts),
            Err(BackendOptionsError::InvalidValue {
                field: "jacobian_reuse",
                ..
            })
        ));
    }

    #[cfg(feature = "solver-rk45")]
    #[test]
    fn fixed_step_rejects_invalid_values_and_keys_every_consumed_field() {
        let mut opts = SimOptions {
            solver_mode: SimSolverMode::FixedStep,
            ..Default::default()
        };
        let original = rumoca_solver_rk45::FixedStepConfig::default();
        let absent = validate_for_backend(&opts, NumericalBackend::FixedStep)
            .unwrap()
            .identity();
        assert_eq!(absent, OptionsIdentity::Absent);
        let mut keys = vec![absent];
        for (step, order) in [
            (original.step, original.order),
            (f64::from_bits(original.step.to_bits() + 1), original.order),
            (original.step, 2),
            (original.step, 1),
        ] {
            opts.backend_options = Some(fixed_step_backend_options(step, order));
            let validated = validate_for_backend(&opts, NumericalBackend::FixedStep).unwrap();
            let ValidatedOptions::FixedStep(consumed) = validated else {
                panic!("the factory must receive the validated concrete configuration");
            };
            assert_eq!(consumed.step.to_bits(), step.to_bits());
            assert_eq!(consumed.order, order);
            keys.push(validated.identity());
        }
        for i in 0..keys.len() {
            for j in i + 1..keys.len() {
                assert_ne!(keys[i], keys[j]);
            }
        }
        for step in [0.0, -0.0, -1.0, f64::INFINITY, f64::NAN] {
            opts.backend_options = Some(fixed_step_backend_options(step, original.order));
            assert_eq!(
                validate(&opts).unwrap_err(),
                BackendOptionsError::InvalidValue {
                    backend: NumericalBackend::FixedStep,
                    field: "step",
                }
            );
        }
        for order in [0, 3, 5, u32::MAX] {
            opts.backend_options = Some(fixed_step_backend_options(original.step, order));
            assert_eq!(
                validate(&opts).unwrap_err(),
                BackendOptionsError::InvalidValue {
                    backend: NumericalBackend::FixedStep,
                    field: "order",
                }
            );
        }
        opts.backend_options = Some(fixed_step_backend_options(original.step, original.order));
        assert!(matches!(
            validate_for_backend(&opts, NumericalBackend::Rk45),
            Err(BackendOptionsError::BackendMismatch {
                requested: NumericalBackend::Rk45,
                supplied: NumericalBackend::FixedStep,
            })
        ));
    }

    #[cfg(feature = "solver-rk45")]
    #[test]
    fn fixed_step_semi_implicit_scheme_is_keyed_and_refused_above_order_one() {
        let mut opts = SimOptions {
            solver_mode: SimSolverMode::FixedStep,
            ..Default::default()
        };
        let step = rumoca_solver_rk45::FixedStepConfig::default().step;
        opts.backend_options = Some(fixed_step_semi_implicit_backend_options(step, 1));
        let semi_implicit = validate_for_backend(&opts, NumericalBackend::FixedStep).unwrap();
        let ValidatedOptions::FixedStep(consumed) = semi_implicit else {
            panic!("the factory must receive the validated concrete configuration");
        };
        assert_eq!(
            consumed.scheme,
            rumoca_solver_rk45::FixedStepScheme::SemiImplicit
        );
        opts.backend_options = Some(fixed_step_backend_options(step, 1));
        let explicit = validate_for_backend(&opts, NumericalBackend::FixedStep).unwrap();
        assert_ne!(semi_implicit.identity(), explicit.identity());
        for order in [2, 4] {
            opts.backend_options = Some(fixed_step_semi_implicit_backend_options(step, order));
            assert_eq!(
                validate_for_backend(&opts, NumericalBackend::FixedStep).unwrap_err(),
                BackendOptionsError::InvalidValue {
                    backend: NumericalBackend::FixedStep,
                    field: "scheme",
                }
            );
        }
    }

    #[cfg(feature = "solver-velimplicit")]
    #[test]
    fn captured_configuration_is_the_same_immutable_value_for_key_and_factory() {
        let mut original = rumoca_solver_velimplicit::VelImplicitConfig::default();
        let captured = original;
        let payload = BackendOptions::new(NumericalBackend::VelImplicit, original);
        let opts = SimOptions {
            solver_mode: SimSolverMode::VelImplicit,
            backend_options: Some(payload.clone()),
            ..Default::default()
        };
        original.step *= 2.0;
        original.jacobian_reuse += 1;
        original.sparse_lu = !original.sparse_lu;
        assert_ne!(original, captured);
        drop(payload);

        let keyed = validate(&opts).unwrap();
        let factory = validate_for_backend(&opts, NumericalBackend::VelImplicit).unwrap();
        assert_eq!(keyed.identity(), factory.identity());
        let ValidatedOptions::VelImplicit(consumed) = factory else {
            panic!("the factory must receive the validated concrete configuration");
        };
        assert_eq!(consumed.exact_key(), captured.exact_key());
        assert_ne!(consumed.exact_key(), original.exact_key());
        assert_eq!(consumed.step.to_bits(), captured.step.to_bits());
        assert_eq!(consumed.jacobian_reuse, captured.jacobian_reuse);
        assert_eq!(consumed.sparse_lu, captured.sparse_lu);
    }
}
