//! Concrete native execution within the existing ME lifecycle and rollback owner.

mod adoption;
#[cfg(test)]
mod owner_fixture;
mod preinitialization;
mod real;
pub use preinitialization::{MePendingNativeComponent, MePendingNativeState};
mod request;
mod values;

use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_ir_solve::fmi::reads_input_lanes;

use super::*;

impl SolveMeKernel {
    pub(in crate::fmi_me) fn require_native_engine_admission(&self) -> Result<(), MeError> {
        self.native_transport
            .as_ref()
            .ok_or_else(|| contract("native execution requires its original resource"))?
            .borrow()
            .require_execution_entry()
            .map_err(|error| contract(error.to_string()))
    }

    pub(in crate::fmi_me) fn native_value_count(
        &self,
        references: &[u32],
        kind: SolveVariableValueKind,
    ) -> Result<usize, MeError> {
        self.require_active_lifecycle("native_value_count")?;
        let transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("native request counts require the original native resource"))?
            .borrow();
        let (_, count) = request::read_request(
            transport.product().inputs().root().metadata(),
            references,
            kind,
            false,
        )?;
        Ok(count)
    }

    /// Prepare one browser-native entry from the instance's resolved component root.
    /// Other engines remain refused until their exact linked entry is sealed.
    pub fn instantiate_native_wasm(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
    ) -> Result<Self, MeError> {
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            let (model, facts) = Self::resolve_instance_source(source, config)?;
            let root = model.retained_assertion_root().ok_or_else(|| {
                contract("native FMI execution requires the original correlated component")
            })?;
            let schedule = root
                .native_schedule()
                .map_err(|error| contract(error.to_string()))?;
            let product = rumoca_exec_wasm::compile_retained_native_program_wasm(schedule)
                .map_err(crate::runtime::native_wasm_error::execution_error)?;
            let runtime =
                SolveRuntime::new_with_native_owner(model, product.into_host(), config.start_time)?;
            Self::finish_instantiation(runtime, facts, config)
        }
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        {
            let _ = (source, config);
            Err(contract(
                "native FMI execution has no sealed entry for this engine",
            ))
        }
    }

    pub(in crate::fmi_me) fn check_native_integer_inputs(
        &self,
        refs: &[u32],
        payload: &[i64],
    ) -> Result<(), MeError> {
        self.check_native_values(refs, payload)
    }
    pub(in crate::fmi_me) fn check_boolean_inputs(
        &self,
        refs: &[u32],
        payload: &[bool],
    ) -> Result<(), MeError> {
        self.check_native_values(refs, payload)
    }
    /// Parameters are writable only until `ExitInitializationMode`.
    fn input_access(&self) -> request::Access {
        if self.lifecycle.state() == MeState::InitializationMode {
            request::Access::WriteInputsAndParameters
        } else {
            request::Access::WriteInputs
        }
    }

    fn check_native_values<T: values::TypedScalar>(
        &self,
        references: &[u32],
        payload: &[T],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("check_native_inputs")?;
        let transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("typed native inputs require a prepared native component"))?;
        let transport = transport.borrow();
        let batches = values::input_batches(
            transport.product().inputs().root().metadata(),
            references,
            payload,
            self.input_access(),
        )?;
        for (base, batch) in batches {
            transport
                .check_typed_inputs(base, batch)
                .map_err(transport_error)?;
        }
        Ok(())
    }

    pub fn set_native_integer(
        &mut self,
        references: &[u32],
        values: &[i64],
    ) -> Result<(), MeError> {
        self.set_native_values(references, values)
    }

    pub fn set_boolean(&mut self, references: &[u32], values: &[bool]) -> Result<(), MeError> {
        self.set_native_values(references, values)
    }

    pub fn get_native_integer(
        &self,
        references: &[u32],
        values: &mut [i64],
    ) -> Result<(), MeError> {
        self.get_native_values(references, values)
    }

    pub fn get_boolean(&self, references: &[u32], values: &mut [bool]) -> Result<(), MeError> {
        self.get_native_values(references, values)
    }

    fn set_native_values<T: values::TypedScalar>(
        &mut self,
        references: &[u32],
        values: &[T],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("set_native_typed")?;
        let transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("typed native setters require a prepared native component"))?;
        let mut transport = transport.borrow_mut();
        let root = transport.product().inputs().root().clone();
        let batches =
            values::input_batches(root.metadata(), references, values, self.input_access())?;
        transport
            .set_typed_input_batches(&batches)
            .map_err(transport_error)?;
        drop(transport);
        self.clear_runtime_caches();
        Ok(())
    }

    fn get_native_values<T: values::TypedScalar>(
        &self,
        references: &[u32],
        values: &mut [T],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("get_native_typed")?;
        let transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("typed native getters require a prepared native component"))?;
        let root = transport.borrow().product().inputs().root().clone();
        let variables = request::checked_request(
            root.metadata(),
            references,
            values.len(),
            T::KIND,
            request::Access::Read,
        )?;
        for variable in &variables {
            values::check_membership(&transport.borrow(), variable)?;
        }
        if variables
            .iter()
            .any(|variable| !reads_input_lanes(variable))
        {
            self.native_coordinate(
                self.public_time_eval_time(self.time),
                &self.solver_y_guess.borrow(),
                &self.params,
            )?;
        }
        let transport = transport.borrow();
        // Check complete membership, presence and types before writing any caller cell.
        for variable in &variables {
            values::visit(&transport, variable, |_, value| {
                T::decode(value).map(|_| ())
            })?;
        }
        let mut base = 0;
        for variable in variables {
            values::visit(&transport, variable, |index, value| {
                values[base + index] = T::decode(value)?;
                Ok(())
            })?;
            base += variable
                .storage()
                .expect("request checked original storage")
                .scalar_count();
        }
        Ok(())
    }

    pub(super) fn native_coordinate(
        &self,
        time: f64,
        y: &[f64],
        p: &[f64],
    ) -> Result<Vec<f64>, MeError> {
        let mut transport = self
            .native_transport
            .as_ref()
            .ok_or_else(|| contract("native execution requires the prepared component entry"))?
            .borrow_mut();
        transport.set_coordinates(y, p).map_err(transport_error)?;
        let outcome = transport
            .enter()
            .map_err(transport_error)?
            .execute(time)
            .map_err(crate::runtime::native_wasm_error::execution_error)?;
        let y = outcome.y().to_vec();
        outcome.commit_buffers();
        Ok(y)
    }

    pub(super) fn native_parameter_is_real(&self, index: usize) -> bool {
        let Some(transport) = &self.native_transport else {
            return true;
        };
        transport
            .borrow()
            .product()
            .inputs()
            .root()
            .metadata()
            .variables()
            .iter()
            .any(|variable| {
                variable.value_kind() == SolveVariableValueKind::Real
                    && variable.storage().is_some_and(|storage| {
                        storage.column() == rumoca_ir_solve::fmi::FmiStorageColumn::P
                            && index >= storage.base()
                            && index - storage.base() < storage.scalar_count()
                    })
            })
    }

    pub(super) fn sync_native_coordinates(&self) -> Result<(), MeError> {
        if let Some(transport) = &self.native_transport {
            transport
                .borrow_mut()
                .set_coordinates(&self.solver_y_guess.borrow(), &self.params)
                .map_err(transport_error)?;
        }
        Ok(())
    }
}

pub(super) fn transport_error(error: rumoca_exec_wasm::NativeTransportError) -> MeError {
    contract(error.to_string())
}

#[cfg(test)]
fn require_same_source(
    runtime: &rumoca_ir_solve::RetainedAssertionRoot,
    compiled: &rumoca_ir_solve::RetainedAssertionRoot,
) -> Result<(), MeError> {
    if !runtime.shares_root_owner(compiled)
        || !std::ptr::eq(runtime.metadata(), compiled.metadata())
    {
        return Err(contract(
            "prepared native entry belongs to another component source",
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prepared_entry_binding_requires_original_root_and_inventory_allocations() {
        let mut model = rumoca_ir_solve::SolveModel::default();
        model.problem.continuous.refresh_owners =
            rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
                .expect("issue legal empty-model refresh owners");
        let component = rumoca_ir_solve::fmi::FmiComponent::construct(model, vec![]).unwrap();
        let source = component.runtime_view().retain_assertion_root();
        let first = source.resolved_periodic_schedules_at(1.0).unwrap();
        let second = source.resolved_periodic_schedules_at(2.0).unwrap();
        require_same_source(&first, &second).unwrap();
        let foreign = rumoca_ir_solve::fmi::FmiComponent::construct(
            rumoca_ir_solve::SolveModel::default(),
            vec![],
        )
        .unwrap();
        let foreign = foreign
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(1.0)
            .unwrap();
        assert!(require_same_source(&first, &foreign).is_err());
        drop(component);
        require_same_source(&first, &second).unwrap();
    }

    #[test]
    fn ordinary_component_cannot_fabricate_typed_native_reads() {
        let mut model = rumoca_ir_solve::SolveModel::default();
        model.problem.continuous.refresh_owners =
            rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
                .expect("ordinary scalar fixture refresh owners construct");
        let component = rumoca_ir_solve::fmi::FmiComponent::construct(model, vec![]).unwrap();
        let config = MeInstanceConfig::new("typed-refusal", 1e-6, 0.0, 1.0).unwrap();
        let mut kernel =
            SolveMeKernel::instantiate(MeModelSource::new(&component), &config).unwrap();
        let before = kernel.fmu_state().unwrap();
        let mut integer = [i64::MAX];
        let mut boolean = [true];
        let mut real = [13.0];
        assert!(kernel.get_native_integer(&[1], &mut integer).is_err());
        assert!(kernel.get_boolean(&[1], &mut boolean).is_err());
        assert!(kernel.get_native_float64(&[1], &mut real).is_err());
        assert!(kernel.set_native_integer(&[1], &[i64::MAX]).is_err());
        assert!(kernel.set_boolean(&[1], &[true]).is_err());
        assert!(kernel.set_native_float64(&[1], &[13.0]).is_err());
        assert_eq!(integer, [i64::MAX]);
        assert_eq!(boolean, [true]);
        assert_eq!(real, [13.0]);
        assert!(kernel.verification_matches_snapshot(&before));
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        assert!(
            SolveMeKernel::instantiate_native_wasm(MeModelSource::new(&component), &config)
                .is_err()
        );
    }
}
