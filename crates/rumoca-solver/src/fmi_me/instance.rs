//! One private component resource shared by the retained host and its controller.

use std::cell::{Ref, RefCell, RefMut};

use super::{MeError, MeFmuState, MeInstanceConfig, SolveMeKernel};

#[cfg(feature = "native-wasm")]
use super::MePendingNativeComponent;

pub(super) struct MeKernelCell {
    resource: RefCell<MeKernelResource>,
    configuration: RefCell<Option<MeInstanceConfig>>,
    scalar_pristine: Option<MeFmuState>,
}

enum MeKernelResource {
    Active(Box<SolveMeKernel>),
    #[cfg(feature = "native-wasm")]
    Prepared(Box<MePendingNativeComponent>),
    /// Held only while an ownership transition moves the resource; every
    /// transition stores its outcome before returning.
    #[cfg(feature = "native-wasm")]
    Vacated,
}

impl MeKernelCell {
    pub(super) fn terminate(&self) -> Result<(), MeError> {
        match &mut *self.resource.borrow_mut() {
            MeKernelResource::Active(kernel) => kernel.terminate(),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Prepared(pending) => pending.terminate(),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Vacated => Err(vacated_refusal()),
        }
    }
    pub(super) fn initialization_configuration(&self) -> Result<MeInstanceConfig, MeError> {
        let kernel = self.borrow()?;
        if kernel.initialization_state() != super::lifecycle::MeState::InitializationMode {
            return Err(MeError::Contract {
                reason: "initialization configuration requires actual Initialization Mode".into(),
            });
        }
        Ok(kernel.instance_configuration())
    }

    #[cfg(feature = "native-wasm")]
    pub(super) fn native_value_count(
        &self,
        references: &[u32],
        kind: rumoca_ir_solve::SolveVariableValueKind,
    ) -> Result<usize, MeError> {
        match &*self.resource.borrow() {
            MeKernelResource::Active(kernel) => kernel.native_value_count(references, kind),
            MeKernelResource::Prepared(pending) => pending.native_value_count(references, kind),
            MeKernelResource::Vacated => Err(vacated_refusal()),
        }
    }
    pub(super) fn new(kernel: SolveMeKernel) -> Self {
        let configuration = kernel.instance_configuration();
        let scalar_pristine = if kernel.has_native_resource() {
            None
        } else {
            Some(
                kernel
                    .fmu_state()
                    .expect("scalar-only instance construction has no native allocation"),
            )
        };
        Self {
            resource: RefCell::new(MeKernelResource::Active(Box::new(kernel))),
            configuration: RefCell::new(Some(configuration)),
            scalar_pristine,
        }
    }

    #[cfg(feature = "native-wasm")]
    pub(super) fn prepared(pending: MePendingNativeComponent) -> Self {
        Self {
            resource: RefCell::new(MeKernelResource::Prepared(Box::new(pending))),
            configuration: RefCell::new(None),
            scalar_pristine: None,
        }
    }

    fn restart_configuration(
        &self,
        start: f64,
        stop: Option<f64>,
    ) -> Result<MeInstanceConfig, MeError> {
        self.configuration.borrow().as_ref().ok_or_else(|| MeError::Contract {
            reason: "prepared instance requires its actual initialization request before a host lease".to_owned(),
        })?.restarted_at(start, stop)
    }

    #[cfg(feature = "native-wasm")]
    pub(super) fn enter_initialization_mode(
        &self,
        config: &MeInstanceConfig,
    ) -> Result<(), MeError> {
        if self
            .configuration
            .borrow()
            .as_ref()
            .is_some_and(|original| original.instance_name != config.instance_name)
        {
            return Err(MeError::Contract {
                reason: "native initialization cannot change the retained instance name".to_owned(),
            });
        }
        self.require_native_engine_admission()?;
        self.activate_prepared_owned(config)
    }

    pub(super) fn borrow(&self) -> Result<Ref<'_, SolveMeKernel>, MeError> {
        Ref::filter_map(self.resource.borrow(), |resource| match resource {
            MeKernelResource::Active(kernel) => Some(kernel.as_ref()),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Prepared(_) | MeKernelResource::Vacated => None,
        })
        .map_err(|_| prepared_refusal())
    }

    pub(super) fn with_model<T>(
        &self,
        inspect: impl FnOnce(&rumoca_ir_solve::SolveModel) -> T,
    ) -> Option<T> {
        match &*self.resource.borrow() {
            MeKernelResource::Active(kernel) => Some(inspect(kernel.source_model())),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Prepared(pending) => Some(inspect(pending.original_model())),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Vacated => None,
        }
    }

    pub(super) fn borrow_mut(&self) -> Result<RefMut<'_, SolveMeKernel>, MeError> {
        RefMut::filter_map(self.resource.borrow_mut(), |resource| match resource {
            MeKernelResource::Active(kernel) => Some(kernel.as_mut()),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Prepared(_) | MeKernelResource::Vacated => None,
        })
        .map_err(|_| prepared_refusal())
    }

    /// FMI Reset, with no fabricated next initialization coordinate.
    pub(super) fn reset(&self) -> Result<(), MeError> {
        let mut slot = self.resource.borrow_mut();
        let Some(pristine) = &self.scalar_pristine else {
            return reset_native(&mut slot);
        };
        match &mut *slot {
            MeKernelResource::Active(kernel) => kernel.reset_to_fmu_state(pristine),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Prepared(_) | MeKernelResource::Vacated => Err(prepared_refusal()),
        }
    }

    pub(super) fn prepare_host(&self, start: f64, stop: Option<f64>) -> Result<(), MeError> {
        #[cfg(feature = "native-wasm")]
        let config = self.restart_configuration(start, stop)?;
        #[cfg(not(feature = "native-wasm"))]
        self.restart_configuration(start, stop)?;
        #[cfg(feature = "native-wasm")]
        if matches!(&*self.resource.borrow(), MeKernelResource::Prepared(_)) {
            return self.activate_prepared(&config);
        }
        #[cfg(feature = "native-wasm")]
        let kernel = self.borrow()?;
        #[cfg(feature = "native-wasm")]
        if kernel.has_native_resource()
            && (kernel.native_start_time().map(f64::to_bits) != Some(start.to_bits())
                || kernel.instance_configuration().start_time.to_bits() != start.to_bits())
        {
            return Err(MeError::Contract {
                reason: "native host request differs from its actual initialization coordinate"
                    .to_owned(),
            });
        }
        Ok(())
    }

    pub(super) fn prepare_lease(&self, start: f64, stop: Option<f64>) -> Result<(), MeError> {
        self.restart_configuration(start, stop)?;
        #[cfg(feature = "native-wasm")]
        if matches!(&*self.resource.borrow(), MeKernelResource::Prepared(_)) {
            return self.prepare_host(start, stop);
        }
        let completing = {
            let kernel = self.borrow()?;
            kernel.has_native_resource()
                && kernel.initialization_state() == super::lifecycle::MeState::InitializationMode
        };
        if completing {
            self.prepare_host(start, stop)
        } else {
            self.restart(start, stop)
        }
    }

    /// The common session restart composes source reset with actual initialization.
    pub(super) fn restart(&self, start: f64, stop: Option<f64>) -> Result<(), MeError> {
        #[cfg(feature = "native-wasm")]
        let config = self.restart_configuration(start, stop)?;
        #[cfg(not(feature = "native-wasm"))]
        self.restart_configuration(start, stop)?;
        if let Some(pristine) = &self.scalar_pristine {
            return self.borrow_mut()?.restart_from_fmu_state(pristine, start);
        }
        #[cfg(feature = "native-wasm")]
        {
            self.require_native_engine_admission()?;
            self.reset()?;
            self.activate_prepared(&config)
        }
        #[cfg(not(feature = "native-wasm"))]
        Err(no_scalar_snapshot())
    }

    #[cfg(feature = "native-wasm")]
    fn require_native_engine_admission(&self) -> Result<(), MeError> {
        match &*self.resource.borrow() {
            MeKernelResource::Prepared(pending) => pending.require_engine_admission(),
            MeKernelResource::Active(kernel) => kernel.require_native_engine_admission(),
            MeKernelResource::Vacated => Err(vacated_refusal()),
        }
    }

    #[cfg(feature = "native-wasm")]
    fn activate_prepared(&self, config: &MeInstanceConfig) -> Result<(), MeError> {
        self.require_native_engine_admission()?;
        self.activate_prepared_owned(config)
    }

    #[cfg(feature = "native-wasm")]
    fn activate_prepared_owned(&self, config: &MeInstanceConfig) -> Result<(), MeError> {
        let result = transition(&mut self.resource.borrow_mut(), |resource| match resource {
            MeKernelResource::Prepared(pending) => {
                match (*pending).enter_initialization_mode_owned(config) {
                    Ok(kernel) => (MeKernelResource::Active(Box::new(kernel)), Ok(())),
                    Err((pending, error)) => (MeKernelResource::Prepared(pending), Err(error)),
                }
            }
            other => (
                other,
                Err(MeError::Contract {
                    reason: "native initialization requires its prepared instance resource"
                        .to_owned(),
                }),
            ),
        });
        if result.is_ok() {
            *self.configuration.borrow_mut() = Some(config.clone());
        }
        result
    }

    pub(super) fn diagnostics(&self) -> Vec<crate::SimDiagnostic> {
        match &*self.resource.borrow() {
            MeKernelResource::Active(kernel) => kernel.diagnostics(),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Prepared(_) | MeKernelResource::Vacated => Vec::new(),
        }
    }

    pub(super) fn diagnostics_since(&self, first: usize) -> (Vec<crate::SimDiagnostic>, usize) {
        match &*self.resource.borrow() {
            MeKernelResource::Active(kernel) => kernel.diagnostics_since(first),
            #[cfg(feature = "native-wasm")]
            MeKernelResource::Prepared(_) | MeKernelResource::Vacated => (Vec::new(), 0),
        }
    }
}

/// Move the resource through an ownership transition; the outcome is stored
/// before this returns, so the vacated placeholder is never observable.
#[cfg(feature = "native-wasm")]
fn transition<R>(
    slot: &mut MeKernelResource,
    next: impl FnOnce(MeKernelResource) -> (MeKernelResource, R),
) -> R {
    let (resource, result) = next(std::mem::replace(slot, MeKernelResource::Vacated));
    *slot = resource;
    result
}

/// FMI Reset of a native component: the active kernel becomes a prepared
/// resource, which later initialization activates.
#[cfg(feature = "native-wasm")]
fn reset_native(slot: &mut MeKernelResource) -> Result<(), MeError> {
    transition(slot, |resource| match resource {
        MeKernelResource::Active(kernel) => {
            match MePendingNativeComponent::from_reset_kernel(*kernel) {
                Ok(pending) => (MeKernelResource::Prepared(Box::new(pending)), Ok(())),
                Err((kernel, error)) => (MeKernelResource::Active(kernel), Err(error)),
            }
        }
        MeKernelResource::Prepared(mut pending) => {
            let result = pending.reset();
            (MeKernelResource::Prepared(pending), result)
        }
        MeKernelResource::Vacated => (MeKernelResource::Vacated, Err(vacated_refusal())),
    })
}

#[cfg(not(feature = "native-wasm"))]
fn reset_native(_slot: &mut MeKernelResource) -> Result<(), MeError> {
    Err(no_scalar_snapshot())
}

#[cfg(not(feature = "native-wasm"))]
fn no_scalar_snapshot() -> MeError {
    MeError::Contract {
        reason: "a non-native instance has no scalar pristine snapshot".to_owned(),
    }
}

#[cfg(feature = "native-wasm")]
fn vacated_refusal() -> MeError {
    MeError::Contract {
        reason: "component resource is unavailable after an interrupted ownership transition"
            .to_owned(),
    }
}

fn prepared_refusal() -> MeError {
    MeError::Contract {
        reason: "native resource is prepared; actual initialization has not activated its kernel"
            .to_owned(),
    }
}

#[cfg(all(test, feature = "native-wasm"))]
mod tests;

#[cfg(feature = "native-wasm")]
macro_rules! native_accessors {
    ($set:ident, $get:ident, $value:ty) => {
        impl MeKernelCell {
            pub(super) fn $set(
                &self,
                references: &[u32],
                values: &[$value],
            ) -> Result<(), MeError> {
                match &mut *self.resource.borrow_mut() {
                    MeKernelResource::Active(kernel) => kernel.$set(references, values),
                    MeKernelResource::Prepared(pending) => pending.$set(references, values),
                    MeKernelResource::Vacated => Err(vacated_refusal()),
                }
            }
            pub(super) fn $get(
                &self,
                references: &[u32],
                values: &mut [$value],
            ) -> Result<(), MeError> {
                match &*self.resource.borrow() {
                    MeKernelResource::Active(kernel) => kernel.$get(references, values),
                    MeKernelResource::Prepared(pending) => pending.$get(references, values),
                    MeKernelResource::Vacated => Err(vacated_refusal()),
                }
            }
        }
    };
}
#[cfg(feature = "native-wasm")]
native_accessors!(set_native_integer, get_native_integer, i64);
#[cfg(feature = "native-wasm")]
native_accessors!(set_boolean, get_boolean, bool);
#[cfg(feature = "native-wasm")]
native_accessors!(set_native_float64, get_native_float64, f64);
