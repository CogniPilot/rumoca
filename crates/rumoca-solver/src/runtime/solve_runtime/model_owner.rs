//! Immutable runtime storage, retaining correlated FMI source identity.

use std::{ops::Deref, sync::Arc};

use rumoca_ir_solve::{RetainedAssertionResolutionError, RetainedAssertionRoot, SolveModel};

/// Read-only runtime model storage. Only correlated FMI construction can
/// supply retained assertion authority; standalone models remain separate.
#[derive(Clone)]
pub struct SolveRuntimeModel(ModelOwner);

#[derive(Clone)]
enum ModelOwner {
    Correlated(RetainedAssertionRoot),
    Standalone(Arc<SolveModel>),
}

impl SolveRuntimeModel {
    pub(crate) fn retained(root: RetainedAssertionRoot) -> Self {
        Self(ModelOwner::Correlated(root))
    }

    pub(crate) fn copied(model: &SolveModel) -> Self {
        Self(ModelOwner::Standalone(Arc::new(model.clone())))
    }

    /// The exact immutable FMI source, when this is a correlated runtime.
    #[must_use]
    pub fn retained_assertion_root(&self) -> Option<&RetainedAssertionRoot> {
        match &self.0 {
            ModelOwner::Correlated(root) => Some(root),
            ModelOwner::Standalone(_) => None,
        }
    }

    pub(crate) fn resolve_start_time(
        self,
        start_time: f64,
    ) -> Result<Self, RetainedAssertionResolutionError> {
        match self.0 {
            ModelOwner::Correlated(root) => root
                .resolved_periodic_schedules_at(start_time)
                .map(Self::retained),
            ModelOwner::Standalone(model) => {
                let resolved = model
                    .resolved_periodic_schedules_at(start_time)
                    .map_err(RetainedAssertionResolutionError::Clock)?;
                let owner = match resolved {
                    std::borrow::Cow::Borrowed(_) => model,
                    std::borrow::Cow::Owned(resolved) => Arc::new(resolved),
                };
                Ok(Self(ModelOwner::Standalone(owner)))
            }
        }
    }
}

impl Deref for SolveRuntimeModel {
    type Target = SolveModel;

    fn deref(&self) -> &Self::Target {
        match &self.0 {
            ModelOwner::Correlated(root) => root.model(),
            ModelOwner::Standalone(model) => model,
        }
    }
}
