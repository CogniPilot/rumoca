//! Owned, solver-neutral transport for concrete numerical-backend options.
//!
//! The common solver layer cannot interpret the payload. The selected backend
//! must check both its identity and its concrete option type before any model
//! work or reuse. Concrete immutable option types belong to their backend crate.

use std::{any::Any, fmt, sync::Arc};

/// The numerical backend an explicit option payload belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericalBackend {
    Bdf,
    Rk45,
    VelImplicit,
}

trait OptionPayload: fmt::Debug + Send + Sync {
    fn value(&self) -> &dyn Any;
}

impl<T: Any + fmt::Debug + Send + Sync> OptionPayload for T {
    fn value(&self) -> &dyn Any {
        self
    }
}

/// Cloneable typed transport, without a field bag or caller-supplied cache key.
/// Cloning shares ownership; callers cannot replace the captured value.
#[derive(Debug, Clone)]
pub struct BackendOptions {
    backend: NumericalBackend,
    payload: Arc<dyn OptionPayload>,
}

impl BackendOptions {
    /// Capture one backend-owned value. Identity alone never validates its type.
    #[must_use]
    pub fn new<T: Any + fmt::Debug + Send + Sync>(backend: NumericalBackend, value: T) -> Self {
        Self {
            backend,
            payload: Arc::new(value),
        }
    }

    #[must_use]
    pub const fn backend(&self) -> NumericalBackend {
        self.backend
    }

    /// Recover an actual concrete type; no caller-defined downcast hook runs.
    #[must_use]
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        self.payload.as_ref().value().downcast_ref()
    }
}

/// An explicit request failed before numerical construction or model evaluation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BackendOptionsError {
    /// Auto has no implicit policy for distributing a custom payload.
    CustomAuto,
    BackendMismatch {
        requested: NumericalBackend,
        supplied: NumericalBackend,
    },
    ConcreteType {
        backend: NumericalBackend,
    },
    BackendUnavailable {
        backend: NumericalBackend,
    },
    InvalidValue {
        backend: NumericalBackend,
        field: &'static str,
    },
}

impl fmt::Display for BackendOptionsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CustomAuto => f.write_str("custom numerical options require an explicit solver"),
            Self::BackendMismatch {
                requested,
                supplied,
            } => write!(
                f,
                "{requested:?} options requested, but the payload belongs to {supplied:?}"
            ),
            Self::BackendUnavailable { backend } => write!(f, "{backend:?} backend is not built"),
            Self::ConcreteType { backend } => write!(f, "invalid concrete {backend:?} option type"),
            Self::InvalidValue { backend, field } => {
                write!(f, "invalid {backend:?} option `{field}`")
            }
        }
    }
}

impl std::error::Error for BackendOptionsError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq)]
    struct Concrete(u64);

    #[test]
    fn payload_is_owned_cloneable_and_checked_by_actual_type() {
        fn thread_safe<T: Send + Sync>() {}
        thread_safe::<BackendOptions>();
        let original = BackendOptions::new(NumericalBackend::Bdf, Concrete(23));
        let copy = original.clone();
        drop(original);
        assert_eq!(copy.backend(), NumericalBackend::Bdf);
        assert_eq!(copy.downcast_ref::<Concrete>(), Some(&Concrete(23)));
        assert!(copy.downcast_ref::<u64>().is_none());
        assert!(format!("{copy:?}").contains("Concrete"));
    }

    #[test]
    fn configuration_failures_keep_their_discriminated_values() {
        let errors = [
            BackendOptionsError::CustomAuto,
            BackendOptionsError::BackendMismatch {
                requested: NumericalBackend::Rk45,
                supplied: NumericalBackend::Bdf,
            },
            BackendOptionsError::ConcreteType {
                backend: NumericalBackend::VelImplicit,
            },
            BackendOptionsError::BackendUnavailable {
                backend: NumericalBackend::VelImplicit,
            },
            BackendOptionsError::InvalidValue {
                backend: NumericalBackend::VelImplicit,
                field: "step",
            },
        ];
        for error in errors {
            assert_eq!(error, error.clone());
            assert!(!error.to_string().is_empty());
        }
    }
}
