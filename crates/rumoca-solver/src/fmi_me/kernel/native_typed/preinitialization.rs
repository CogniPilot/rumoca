//! Pending storage follows the existing ME lifecycle, without an execution anchor.

#[cfg(test)]
mod tests;
mod values;

use super::super::component::ResolvedInstanceFacts;
use super::super::*;
use rumoca_exec_wasm::{
    NativeTransport, NativeTransportSnapshot, PreparedNativeStorage, PreparedNativeWasmCode,
};

/// An instantiated native resource before the caller supplies initialization time.
/// This owns no invocation, output publication, full FMU state or session capability.
pub struct MePendingNativeComponent {
    model: crate::SolveRuntimeModel,
    facts: Option<ResolvedInstanceFacts>,
    resource: Option<PendingNativeResource>,
}

enum PendingNativeResource {
    Prepared(PreparedNativeStorage),
    Resolved(NativeTransport),
}

/// A same-resource pre-initialization storage checkpoint, never an FMU state.
pub struct MePendingNativeState {
    storage: NativeTransportSnapshot,
    lifecycle: MeState,
}

impl MePendingNativeComponent {
    pub(in crate::fmi_me) fn terminate(&mut self) -> Result<(), MeError> {
        let facts = self.facts.as_mut().expect("pending facts are owned");
        let next = facts
            .lifecycle()
            .next(MeLifecycleCommand::Terminate)
            .map_err(lifecycle_contract)?;
        facts.restore_lifecycle(next);
        Ok(())
    }

    fn require_active(&self, operation: &'static str) -> Result<(), MeError> {
        self.facts
            .as_ref()
            .expect("pending facts are owned")
            .lifecycle()
            .require_active(operation)
    }
    /// Prepare once from the actual correlated source, without resolving at zero.
    pub fn instantiate(
        source: MeModelSource<'_>,
        code: &PreparedNativeWasmCode,
    ) -> Result<Self, MeError> {
        let (model, facts) = SolveMeKernel::instance_source_facts(source)?;
        let root = model.retained_assertion_root().ok_or_else(|| {
            contract("native preparation requires the original correlated component")
        })?;
        let issuing = code.inputs().root();
        if !root.shares_root_owner(issuing) || !std::ptr::eq(root.metadata(), issuing.metadata()) {
            return Err(contract(
                "native preparation belongs to another component source",
            ));
        }
        if root.resolved_start_time().is_some() {
            return Err(contract(
                "pending native preparation requires an unresolved source",
            ));
        }
        let resource = code
            .original_storage_from_source()
            .map_err(super::transport_error)?;
        Ok(Self {
            model,
            facts: Some(facts),
            resource: Some(PendingNativeResource::Prepared(resource)),
        })
    }

    pub(in crate::fmi_me) fn continuous_state_count(&self) -> usize {
        self.model.state_scalar_count()
    }

    pub(in crate::fmi_me) fn original_model(&self) -> &rumoca_ir_solve::SolveModel {
        &self.model
    }

    /// Preserve pre-init values and resource identity, without granting execution.
    pub fn snapshot(&self) -> Result<MePendingNativeState, MeError> {
        let storage = match self.resource.as_ref().expect("pending resource is owned") {
            PendingNativeResource::Prepared(resource) => resource.snapshot(),
            PendingNativeResource::Resolved(resource) => resource.snapshot(),
        }
        .map_err(super::transport_error)?;
        Ok(MePendingNativeState {
            storage,
            lifecycle: self
                .facts
                .as_ref()
                .expect("pending facts are owned")
                .lifecycle()
                .state(),
        })
    }

    pub fn restore(&mut self, saved: &MePendingNativeState) -> Result<(), MeError> {
        match self.resource.as_mut().expect("pending resource is owned") {
            PendingNativeResource::Prepared(resource) => resource.restore(&saved.storage),
            PendingNativeResource::Resolved(resource) => resource.restore(&saved.storage),
        }
        .map_err(super::transport_error)?;
        self.facts
            .as_mut()
            .expect("pending facts are owned")
            .restore_lifecycle(saved.lifecycle);
        Ok(())
    }

    /// Move the same resource and lifecycle into the existing initialization transition.
    /// A refused request returns this actual owner, including all accepted pre-init writes.
    pub fn enter_initialization_mode(
        self,
        config: &MeInstanceConfig,
    ) -> Result<SolveMeKernel, (Box<Self>, MeError)> {
        if let Err(error) = self.require_engine_admission() {
            return Err((Box::new(self), error));
        }
        self.enter_initialization_mode_owned(config)
    }

    pub(in crate::fmi_me) fn require_engine_admission(&self) -> Result<(), MeError> {
        match self.resource.as_ref().expect("pending resource is owned") {
            PendingNativeResource::Prepared(resource) => resource.require_execution_entry(),
            PendingNativeResource::Resolved(resource) => resource.require_execution_entry(),
        }
        .map_err(|error| contract(error.to_string()))
    }

    pub(in crate::fmi_me) fn enter_initialization_mode_owned(
        mut self,
        config: &MeInstanceConfig,
    ) -> Result<SolveMeKernel, (Box<Self>, MeError)> {
        if let Err(error) = self
            .facts
            .as_ref()
            .expect("pending facts are owned")
            .lifecycle()
            .next(MeLifecycleCommand::EnterInitializationMode)
            .map_err(lifecycle_contract)
        {
            return Err((Box::new(self), error));
        }
        if let Err(error) = self.activate(config) {
            return Err((Box::new(self), error));
        }
        let Some(PendingNativeResource::Resolved(transport)) = self.resource.take() else {
            unreachable!("successful activation retains the resolved resource")
        };
        let mut resource = Some(transport);
        let constructed = SolveMeKernel::adopt_native_source_facts(
            &self.model,
            &mut self.facts,
            config,
            &mut resource,
        );
        let mut kernel = match constructed {
            Ok(kernel) => kernel,
            Err(error) => {
                self.resource = Some(PendingNativeResource::Resolved(
                    resource.expect("failed adoption retains its resource"),
                ));
                return Err((Box::new(self), error));
            }
        };
        if let Err(error) = kernel.enter_initialization_mode() {
            let (facts, transport) = ResolvedInstanceFacts::recover_native(kernel);
            self.facts = Some(facts);
            self.resource = Some(PendingNativeResource::Resolved(transport));
            return Err((Box::new(self), error));
        }
        Ok(kernel)
    }

    /// FMI Reset restores actual source defaults, independently of continuation state.
    pub fn reset(&mut self) -> Result<(), MeError> {
        match self.resource.take().expect("pending resource is owned") {
            PendingNativeResource::Prepared(mut resource) => {
                let result = resource.reset_to_source().map_err(super::transport_error);
                self.resource = Some(PendingNativeResource::Prepared(resource));
                result?;
            }
            PendingNativeResource::Resolved(resource) => match resource.reset_to_preparation() {
                Ok(resource) => self.resource = Some(PendingNativeResource::Prepared(resource)),
                Err((resource, error)) => {
                    self.resource = Some(PendingNativeResource::Resolved(*resource));
                    return Err(super::transport_error(error));
                }
            },
        }
        self.facts
            .as_mut()
            .expect("pending facts are owned")
            .restore_lifecycle(MeState::Instantiated);
        Ok(())
    }

    pub(in crate::fmi_me) fn from_reset_kernel(
        mut kernel: SolveMeKernel,
    ) -> Result<Self, (Box<SolveMeKernel>, MeError)> {
        let Some(resource) = kernel.native_transport.take() else {
            return Err((
                Box::new(kernel),
                contract("native reset requires its actual resource"),
            ));
        };
        let prepared = match resource.into_inner().reset_to_preparation() {
            Ok(prepared) => prepared,
            Err((resource, error)) => {
                kernel.native_transport = Some(RefCell::new(*resource));
                return Err((Box::new(kernel), super::transport_error(error)));
            }
        };
        let model = crate::SolveRuntimeModel::retained(prepared.inputs().root().clone());
        let mut facts = ResolvedInstanceFacts::recover_facts(kernel);
        facts.restore_lifecycle(MeState::Instantiated);
        Ok(Self {
            model,
            facts: Some(facts),
            resource: Some(PendingNativeResource::Prepared(prepared)),
        })
    }

    fn activate(&mut self, config: &MeInstanceConfig) -> Result<(), MeError> {
        if matches!(
            self.resource.as_ref(),
            Some(PendingNativeResource::Resolved(_))
        ) {
            return Ok(());
        }
        let root = self
            .model
            .retained_assertion_root()
            .expect("constructor retained the correlated root")
            .resolved_periodic_schedules_at(config.start_time)
            .map_err(|error| contract(error.to_string()))?;
        let Some(PendingNativeResource::Prepared(prepared)) = self.resource.take() else {
            unreachable!("pending resource starts prepared")
        };
        match prepared.activate(root) {
            Ok(transport) => {
                self.resource = Some(PendingNativeResource::Resolved(transport));
                Ok(())
            }
            Err((resource, error)) => {
                self.resource = Some(PendingNativeResource::Prepared(*resource));
                Err(crate::runtime::native_wasm_error::execution_error(error).into())
            }
        }
    }
}
