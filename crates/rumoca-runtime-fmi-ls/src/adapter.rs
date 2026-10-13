//! Fixed WIT projection of the original retained ME resource.
use crate::bindings::exports::fmi::fmi3::co_simulation::{
    CoSimulationInstance, DiscreteStatesInfo, DoStepResult, Guest as CoSimulationGuest,
    GuestCoSimulationInstance, IntervalFraction, IntervalQualifier, Status, VariableDependency,
};
use crate::bindings::exports::fmi::fmi3::common::Guest as CommonGuest;
use crate::{FmiLsResource, FmiLsResourceError};
use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_solver::fmi_me::MeInstanceConfig;
use std::cell::{Cell, RefCell};

pub(crate) struct RumocaFmu;
pub(crate) struct Instance {
    resource: RefCell<FmiLsResource>,
    instance_name: String,
    logging: Cell<bool>,
    categories: RefCell<Vec<String>>,
    diagnostic_cursor: Cell<usize>,
}

impl Instance {
    fn log(&self, status: Status, category: &str, message: &str) {
        let enabled = self.logging.get() && {
            let categories = self.categories.borrow();
            categories.is_empty() || categories.iter().any(|value| value == category)
        };
        if enabled {
            crate::bindings::fmi::fmi3::callbacks::log_message(
                &self.instance_name,
                status,
                category,
                message,
            );
        }
    }

    fn refuse(&self, operation: &str) -> Status {
        self.log(
            Status::Error,
            "error",
            &format!("{operation} is outside the advertised fixed native profile"),
        );
        Status::Error
    }

    fn run<T>(
        &self,
        operation: impl FnOnce(&mut FmiLsResource) -> Result<T, FmiLsResourceError>,
    ) -> Result<T, Status> {
        let mut resource = match self.resource.try_borrow_mut() {
            Ok(resource) => resource,
            Err(_) => return Err(Status::Error),
        };
        let result = operation(&mut resource);
        let (diagnostics, next) = resource.diagnostics_since(self.diagnostic_cursor.get());
        drop(resource);
        self.diagnostic_cursor.set(next);
        for diagnostic in diagnostics {
            let (status, category) = if diagnostic.code == rumoca_solver::WARNING_ASSERTION_CODE {
                (Status::Warning, "warning")
            } else {
                (Status::Ok, "message")
            };
            self.log(status, category, &diagnostic.message);
        }
        result.map_err(|error| {
            self.log(Status::Error, "error", &error.to_string());
            Status::Error
        })
    }

    fn status(
        &self,
        operation: impl FnOnce(&mut FmiLsResource) -> Result<(), FmiLsResourceError>,
    ) -> Status {
        self.run(operation)
            .map_or_else(|error| error, |_| Status::Ok)
    }

    fn get<T: Default + Clone>(
        &self,
        refs: &[u32],
        kind: SolveVariableValueKind,
        getter: impl FnOnce(&FmiLsResource, &[u32], &mut [T]) -> Result<(), FmiLsResourceError>,
    ) -> Result<Vec<T>, Status> {
        self.run(|resource| {
            let count = resource.value_count(refs, kind)?;
            let mut values = crate::resource::result_buffer(count)?;
            getter(resource, refs, &mut values)?;
            Ok(values)
        })
    }
}

impl CommonGuest for RumocaFmu {
    fn get_version() -> String {
        "3.0".to_owned()
    }
}
impl CoSimulationGuest for RumocaFmu {
    type CoSimulationInstance = Instance;
}

impl GuestCoSimulationInstance for Instance {
    fn instantiate_co_simulation(
        instance_name: String,
        instantiation_token: String,
        _resource_path: String,
        visible: bool,
        logging_on: bool,
        event_mode_used: bool,
        early_return_allowed: bool,
        required_intermediate_variables: Vec<u32>,
    ) -> Option<CoSimulationInstance> {
        let prepared = if instance_name.is_empty()
            || visible
            || event_mode_used
            || early_return_allowed
            || !required_intermediate_variables.is_empty()
        {
            Err("requested instance capabilities are outside the fixed native profile".to_owned())
        } else {
            super::bootstrap::prepare(&instantiation_token)
        };
        match prepared {
            Ok(component) => Some(CoSimulationInstance::new(Self {
                resource: RefCell::new(FmiLsResource::from_retained(component)),
                instance_name,
                logging: Cell::new(logging_on),
                categories: RefCell::new(Vec::new()),
                diagnostic_cursor: Cell::new(0),
            })),
            Err(error) => {
                if logging_on {
                    crate::bindings::fmi::fmi3::callbacks::log_message(
                        &instance_name,
                        Status::Error,
                        "error",
                        &error,
                    );
                }
                None
            }
        }
    }

    fn set_debug_logging(&self, logging_on: bool, categories: Vec<String>) -> Status {
        if categories
            .iter()
            .any(|category| !matches!(category.as_str(), "error" | "warning" | "message"))
        {
            return self.refuse("unsupported logging category");
        }
        *self.categories.borrow_mut() = categories;
        self.logging.set(logging_on);
        Status::Ok
    }

    fn enter_initialization_mode(
        &self,
        tolerance: Option<f64>,
        start: f64,
        stop: Option<f64>,
    ) -> Status {
        let tolerance = match tolerance
            .map(Ok)
            .unwrap_or_else(super::bootstrap::default_tolerance)
        {
            Ok(tolerance) => tolerance,
            Err(error) => {
                self.log(Status::Error, "error", &error);
                return Status::Error;
            }
        };
        let config = match stop {
            Some(stop) => MeInstanceConfig::new(self.instance_name.clone(), tolerance, start, stop),
            None => MeInstanceConfig::open_ended(self.instance_name.clone(), tolerance, start),
        };
        match config {
            Ok(config) => self.status(|resource| resource.enter_initialization_mode(&config)),
            Err(error) => {
                self.log(Status::Error, "error", &error.to_string());
                Status::Error
            }
        }
    }
    fn exit_initialization_mode(&self) -> Status {
        self.status(FmiLsResource::complete_initialization)
    }
    fn terminate(&self) -> Status {
        self.status(FmiLsResource::terminate)
    }
    fn reset(&self) -> Status {
        let result = self.status(FmiLsResource::reset);
        if matches!(result, Status::Ok) {
            self.diagnostic_cursor.set(0);
        }
        result
    }
    fn enter_event_mode(&self) -> Status {
        self.refuse("enter_event_mode")
    }
    fn update_discrete_states(&self) -> Result<DiscreteStatesInfo, Status> {
        Err(self.refuse("update_discrete_states"))
    }
    fn enter_configuration_mode(&self) -> Status {
        self.refuse("enter_configuration_mode")
    }
    fn exit_configuration_mode(&self) -> Status {
        self.refuse("exit_configuration_mode")
    }

    fn get_float64(&self, refs: Vec<u32>) -> Result<Vec<f64>, Status> {
        self.get(
            &refs,
            SolveVariableValueKind::Real,
            FmiLsResource::get_native_float64,
        )
    }
    fn set_float64(&self, refs: Vec<u32>, values: Vec<f64>) -> Status {
        self.status(|resource| resource.set_native_float64(&refs, &values))
    }
    fn get_int64(&self, refs: Vec<u32>) -> Result<Vec<i64>, Status> {
        self.get(
            &refs,
            SolveVariableValueKind::Integer,
            FmiLsResource::get_native_integer,
        )
    }
    fn set_int64(&self, refs: Vec<u32>, values: Vec<i64>) -> Status {
        self.status(|resource| resource.set_native_integer(&refs, &values))
    }
    fn get_boolean(&self, refs: Vec<u32>) -> Result<Vec<bool>, Status> {
        self.get(
            &refs,
            SolveVariableValueKind::Boolean,
            FmiLsResource::get_boolean,
        )
    }
    fn set_boolean(&self, refs: Vec<u32>, values: Vec<bool>) -> Status {
        self.status(|resource| resource.set_boolean(&refs, &values))
    }
    fn get_int32(&self, _: Vec<u32>) -> Result<Vec<i32>, Status> {
        Err(self.refuse("get_int32"))
    }
    fn set_int32(&self, _: Vec<u32>, _: Vec<i32>) -> Status {
        self.refuse("set_int32")
    }
    fn get_float32(&self, _: Vec<u32>) -> Result<Vec<f32>, Status> {
        Err(self.refuse("get_float32"))
    }
    fn get_int8(&self, _: Vec<u32>) -> Result<Vec<i8>, Status> {
        Err(self.refuse("get_int8"))
    }
    fn get_int16(&self, _: Vec<u32>) -> Result<Vec<i16>, Status> {
        Err(self.refuse("get_int16"))
    }
    fn get_uint8(&self, _: Vec<u32>) -> Result<Vec<u8>, Status> {
        Err(self.refuse("get_uint8"))
    }
    fn get_uint16(&self, _: Vec<u32>) -> Result<Vec<u16>, Status> {
        Err(self.refuse("get_uint16"))
    }
    fn get_uint32(&self, _: Vec<u32>) -> Result<Vec<u32>, Status> {
        Err(self.refuse("get_uint32"))
    }
    fn get_uint64(&self, _: Vec<u32>) -> Result<Vec<u64>, Status> {
        Err(self.refuse("get_uint64"))
    }
    fn get_string(&self, _: Vec<u32>) -> Result<Vec<String>, Status> {
        Err(self.refuse("get_string"))
    }
    fn get_binary(&self, _: Vec<u32>) -> Result<Vec<Vec<u8>>, Status> {
        Err(self.refuse("get_binary"))
    }
    fn get_clock(&self, _: Vec<u32>) -> Result<Vec<bool>, Status> {
        Err(self.refuse("get_clock"))
    }

    // ── All other typed setters – outside this draft's mapped profile ────────

    fn set_float32(&self, _: Vec<u32>, _: Vec<f32>) -> Status {
        self.refuse("set_float32")
    }
    fn set_int8(&self, _: Vec<u32>, _: Vec<i8>) -> Status {
        self.refuse("set_int8")
    }
    fn set_int16(&self, _: Vec<u32>, _: Vec<i16>) -> Status {
        self.refuse("set_int16")
    }
    fn set_uint8(&self, _: Vec<u32>, _: Vec<u8>) -> Status {
        self.refuse("set_uint8")
    }
    fn set_uint16(&self, _: Vec<u32>, _: Vec<u16>) -> Status {
        self.refuse("set_uint16")
    }
    fn set_uint32(&self, _: Vec<u32>, _: Vec<u32>) -> Status {
        self.refuse("set_uint32")
    }
    fn set_uint64(&self, _: Vec<u32>, _: Vec<u64>) -> Status {
        self.refuse("set_uint64")
    }
    fn set_string(&self, _: Vec<u32>, _: Vec<String>) -> Status {
        self.refuse("set_string")
    }
    fn set_binary(&self, _: Vec<u32>, _: Vec<Vec<u8>>) -> Status {
        self.refuse("set_binary")
    }
    fn set_clock(&self, _: Vec<u32>, _: Vec<bool>) -> Status {
        self.refuse("set_clock")
    }

    fn get_number_of_variable_dependencies(&self, _vr: u32) -> Result<u64, Status> {
        Err(self.refuse("get_number_of_variable_dependencies"))
    }

    fn get_variable_dependencies(
        &self,
        _dependent: u32,
    ) -> Result<Vec<VariableDependency>, Status> {
        Err(self.refuse("get_variable_dependencies"))
    }

    fn get_fmu_state(&self) -> Result<Vec<u8>, Status> {
        Err(self.refuse("get_fmu_state"))
    }

    fn set_fmu_state(&self, _state: Vec<u8>) -> Status {
        self.refuse("set_fmu_state")
    }

    fn get_directional_derivative(
        &self,
        _unknowns: Vec<u32>,
        _knowns: Vec<u32>,
        _seed: Vec<f64>,
    ) -> Result<Vec<f64>, Status> {
        Err(self.refuse("get_directional_derivative"))
    }

    fn get_adjoint_derivative(
        &self,
        _unknowns: Vec<u32>,
        _knowns: Vec<u32>,
        _seed: Vec<f64>,
    ) -> Result<Vec<f64>, Status> {
        Err(self.refuse("get_adjoint_derivative"))
    }

    fn get_interval_decimal(&self, _: Vec<u32>) -> Result<Vec<(f64, IntervalQualifier)>, Status> {
        Err(self.refuse("get_interval_decimal"))
    }
    fn get_interval_fraction(
        &self,
        _: Vec<u32>,
    ) -> Result<Vec<(IntervalFraction, IntervalQualifier)>, Status> {
        Err(self.refuse("get_interval_fraction"))
    }
    fn get_shift_decimal(&self, _: Vec<u32>) -> Result<Vec<f64>, Status> {
        Err(self.refuse("get_shift_decimal"))
    }
    fn get_shift_fraction(&self, _: Vec<u32>) -> Result<Vec<IntervalFraction>, Status> {
        Err(self.refuse("get_shift_fraction"))
    }
    fn set_interval_decimal(&self, _: Vec<u32>, _: Vec<f64>) -> Status {
        self.refuse("set_interval_decimal")
    }
    fn set_interval_fraction(&self, _: Vec<u32>, _: Vec<IntervalFraction>) -> Status {
        self.refuse("set_interval_fraction")
    }
    fn set_shift_decimal(&self, _: Vec<u32>, _: Vec<f64>) -> Status {
        self.refuse("set_shift_decimal")
    }
    fn set_shift_fraction(&self, _: Vec<u32>, _: Vec<IntervalFraction>) -> Status {
        self.refuse("set_shift_fraction")
    }
    fn evaluate_discrete_states(&self) -> Status {
        self.refuse("evaluate_discrete_states")
    }

    fn do_step(
        &self,
        current: f64,
        step: f64,
        _no_prior_state: bool,
    ) -> Result<DoStepResult, Status> {
        self.run(|resource| resource.do_step(current, step))
            .map(|result| DoStepResult {
                last_successful_time: result.last_successful_time,
                event_handling_needed: false,
                terminate_simulation: result.terminate_simulation,
                early_return: result.early_return,
            })
    }
    fn set_input_derivatives(&self, _: Vec<(u32, u32)>, _: Vec<f64>) -> Status {
        self.refuse("set_input_derivatives")
    }
    fn get_output_derivatives(&self, _: Vec<(u32, u32)>) -> Result<Vec<f64>, Status> {
        Err(self.refuse("get_output_derivatives"))
    }
    fn enter_step_mode(&self) -> Status {
        self.refuse("enter_step_mode")
    }
}
