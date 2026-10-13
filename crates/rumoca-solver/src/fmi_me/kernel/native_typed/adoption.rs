//! Consuming adoption of the already activated native resource, without rebinding.

#[cfg(test)]
mod tests;

use super::super::*;
use rumoca_exec_wasm::NativeTransport;

impl SolveMeKernel {
    /// Source construction is shared by production admission and owner controls.
    #[cfg(test)]
    fn adopt_native_resource(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
        transport: NativeTransport,
    ) -> Result<Self, (Box<NativeTransport>, MeError)> {
        let mut resource = Some(transport);
        let constructed = Self::prepare_native_adoption(source, config, &mut resource);
        constructed.map_err(|error| {
            (
                Box::new(resource.expect("all fallible construction precedes resource transfer")),
                error,
            )
        })
    }

    #[cfg(test)]
    fn prepare_native_adoption(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
        resource: &mut Option<NativeTransport>,
    ) -> Result<Self, MeError> {
        let (original, facts) = Self::instance_source_facts(source)?;
        Self::adopt_native_source_facts(&original, &mut Some(facts), config, resource)
    }

    pub(super) fn adopt_native_source_facts(
        original: &crate::SolveRuntimeModel,
        facts: &mut Option<super::super::component::ResolvedInstanceFacts>,
        config: &MeInstanceConfig,
        resource: &mut Option<NativeTransport>,
    ) -> Result<Self, MeError> {
        let transport = resource
            .as_ref()
            .expect("consuming factory owns the actual resource");
        let source_root = original.retained_assertion_root().ok_or_else(|| {
            contract("native adoption requires the original correlated component")
        })?;
        let root = transport.product().inputs().root();
        if !source_root.shares_root_owner(root)
            || !std::ptr::eq(source_root.metadata(), root.metadata())
        {
            return Err(contract(
                "native adoption source differs from its actual resource",
            ));
        }
        let root = root.clone();
        let runtime = SolveRuntime::new_with_native_owner(
            crate::SolveRuntimeModel::retained(root),
            transport.host(),
            config.start_time,
        )?;
        Self::finish_instantiation_owned(runtime, facts, config, resource)
    }
}
