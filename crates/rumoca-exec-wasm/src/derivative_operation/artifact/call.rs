//! Borrowed runtime storage; layout and failure effects were already issued.
use super::*;
use rumoca_ir_solve::fmi::FmiDerivativeStatus as Status;

pub struct FmiDerivativeCall<'a> {
    pub y: &'a mut [f64],
    pub parameters: &'a [f64],
    pub time: f64,
    pub output: &'a mut [f64],
    pub workspace: &'a mut [u64],
    pub cache_values: &'a mut [f64],
    pub cache_present: &'a mut [u8],
    pub cache_parameters: &'a mut [f64],
    pub cache_valid: &'a mut bool,
}

#[derive(Debug)]
pub enum FmiDerivativeFailure {
    Entry(WasmCompileError),
    Helper {
        phase: Status,
        error: WasmCompileError,
    },
    NonFinite {
        output: usize,
    },
}
impl FmiDerivativeFailure {
    pub fn phase(&self) -> Option<Status> {
        match self {
            Self::Entry(_) => None,
            Self::Helper { phase, .. } => Some(*phase),
            Self::NonFinite { .. } => Some(Status::NonFinite),
        }
    }
}

/// The kinematic entry's storage: `y` is read, never written, and only the
/// issued kinematic rows of `output` are written.
pub struct FmiKinematicCall<'a> {
    pub y: &'a [f64],
    pub parameters: &'a [f64],
    pub time: f64,
    pub output: &'a mut [f64],
    pub workspace: &'a mut [u64],
}

impl BoundFmiDerivativeOperation {
    /// The kinematic rows over a settled `y`. A refused helper or a non-finite
    /// kinematic row is the same typed failure as a complete evaluation's.
    pub fn call_kinematic(&self, frame: FmiKinematicCall<'_>) -> Result<(), FmiDerivativeFailure> {
        let lengths = [
            (frame.y.len(), self.workspace.y_bytes()),
            (frame.parameters.len(), self.workspace.regions()[1].1),
            (frame.output.len(), self.workspace.output_bytes()),
        ];
        if self.kinematic.is_none()
            || lengths
                .iter()
                .any(|&(cells, bytes)| cells.checked_mul(8) != Some(bytes as usize))
            || frame
                .workspace
                .len()
                .checked_mul(8)
                .is_none_or(|bytes| bytes < self.workspace.bytes() as usize)
        {
            return Err(FmiDerivativeFailure::Entry(WasmCompileError::Input(
                "kinematic derivative input differs from issued layout".into(),
            )));
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            let mut frame = frame;
            frame.workspace[..2].fill(0);
            let status = self.runtime.call_kinematic(&mut frame).map_err(|error| {
                FmiDerivativeFailure::Helper {
                    phase: Status::Values,
                    error,
                }
            })?;
            self.returned_status(frame.output.len(), frame.workspace, status, true)
        }
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        {
            let _ = frame;
            Err(FmiDerivativeFailure::Entry(WasmCompileError::Input(
                "kinematic FMI derivative has no admitted engine on this target".into(),
            )))
        }
    }
}

impl BoundFmiDerivativeOperation {
    /// Private entry only. Its public FMI runtime must retain the original root.
    /// No interpreted retry occurs after a native failure or engine exception.
    pub fn call(&self, mut frame: FmiDerivativeCall<'_>) -> Result<(), FmiDerivativeFailure> {
        self.validate_frame(&frame)
            .map_err(FmiDerivativeFailure::Entry)?;
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            frame.workspace[..2].fill(0);
            let result = self.runtime.call(&mut frame);
            if frame.workspace[1] >> 32 != 0 {
                *frame.cache_valid = true;
            }
            let status = result.map_err(|error| self.engine_failure(&mut frame, error))?;
            self.returned_status(frame.output.len(), frame.workspace, status, false)
        }
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        {
            let _ = &mut frame;
            Err(FmiDerivativeFailure::Entry(WasmCompileError::Input(
                "complete FMI derivative has no admitted engine on this target".into(),
            )))
        }
    }

    fn validate_frame(&self, frame: &FmiDerivativeCall<'_>) -> Result<(), WasmCompileError> {
        let lengths = [
            frame.y.len().checked_mul(8),
            frame.parameters.len().checked_mul(8),
            Some(0),
            frame.output.len().checked_mul(8),
            frame.workspace.len().checked_mul(8),
            frame.cache_values.len().checked_mul(8),
            Some(frame.cache_present.len()),
            frame.cache_parameters.len().checked_mul(8),
        ];
        for (arg, required) in self.workspace.regions() {
            if arg >= 5 && required == 0 {
                continue;
            }
            let bytes = lengths[arg as usize];
            let valid = if arg == 4 {
                bytes.is_some_and(|bytes| bytes >= required as usize)
            } else {
                bytes == Some(required as usize)
            };
            if !valid {
                return Err(WasmCompileError::Input(
                    "complete derivative input differs from issued layout".into(),
                ));
            }
        }
        Ok(())
    }

    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    fn engine_failure(
        &self,
        frame: &mut FmiDerivativeCall<'_>,
        mut error: WasmCompileError,
    ) -> FmiDerivativeFailure {
        let phase = Status::from_code(frame.workspace[0] as u32).filter(|phase| {
            matches!(
                phase,
                Status::StaticRefresh | Status::DynamicRefresh | Status::Values
            )
        });
        if phase.is_some_and(Status::restores_coordinate)
            && let Err(recovery) = super::runtime::restore_coordinate(frame, self.workspace)
        {
            error = WasmCompileError::Backend(format!(
                "{error}; coordinate recovery failed: {recovery}"
            ));
        }
        match phase {
            Some(phase) => FmiDerivativeFailure::Helper { phase, error },
            None => FmiDerivativeFailure::Entry(error),
        }
    }

    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    fn returned_status(
        &self,
        outputs: usize,
        workspace: &[u64],
        status: u32,
        kinematic: bool,
    ) -> Result<(), FmiDerivativeFailure> {
        let Some(phase) = Status::from_code(status) else {
            return Err(FmiDerivativeFailure::Entry(WasmCompileError::Backend(
                "invalid complete derivative status".into(),
            )));
        };
        if phase == Status::Success {
            return Ok(());
        }
        if phase == Status::NonFinite {
            let output = workspace[1] as u32 as usize;
            if output < outputs {
                return Err(FmiDerivativeFailure::NonFinite { output });
            }
            return Err(FmiDerivativeFailure::Entry(WasmCompileError::Backend(
                "invalid non-finite derivative index".into(),
            )));
        }
        let helper = match phase {
            Status::StaticRefresh => self.static_program.as_ref(),
            Status::DynamicRefresh => self.dynamic_program.as_ref(),
            Status::Values if kinematic => self.kinematic.as_ref(),
            Status::Values => Some(&self.values),
            _ => None,
        };
        let error = helper
            .map(|helper| helper.status_error((workspace[0] >> 32) as u32))
            .unwrap_or_else(|| {
                WasmCompileError::Input("complete derivative buffer admission refused".into())
            });
        Err(FmiDerivativeFailure::Helper { phase, error })
    }
}

#[cfg(all(test, not(target_arch = "wasm32")))]
mod tests {
    use super::*;
    #[test]
    fn source_sized_buffers_do_not_grant_an_unavailable_engine_or_mutate_storage() {
        let component = super::super::tests::build_component(3);
        let root = component.runtime_view().retain_assertion_root();
        let operation = root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        let compiled = compile_fmi_derivative_operation_wasm(&operation).unwrap();
        let bound = PrecompiledFmiDerivativeOperation::from_compiled(&compiled)
            .bind(&operation)
            .unwrap();
        let regions = bound.workspace().regions();
        let mut y = root.model().initial_y.to_vec();
        let p = root.model().parameters.to_vec();
        let mut output = vec![91.0; operation.source().vector_extent()];
        let mut workspace = vec![0x1234; (bound.workspace().bytes() as usize).div_ceil(8)];
        let mut values = vec![7.; regions[4].1 as usize / 8];
        let mut present = vec![1; regions[5].1 as usize];
        let mut keys = vec![2.; regions[6].1 as usize / 8];
        let mut valid = true;
        let before = (
            y.clone(),
            output.clone(),
            workspace.clone(),
            values.clone(),
            present.clone(),
            keys.clone(),
        );
        let frame = FmiDerivativeCall {
            y: &mut y,
            parameters: &p,
            time: 0.,
            output: &mut output,
            workspace: &mut workspace,
            cache_values: &mut values,
            cache_present: &mut present,
            cache_parameters: &mut keys,
            cache_valid: &mut valid,
        };
        assert!(bound.validate_frame(&frame).is_ok());
        assert!(matches!(
            bound.call(frame),
            Err(FmiDerivativeFailure::Entry(_))
        ));
        assert_eq!(
            (y.clone(), output, workspace, values, present, keys),
            before
        );
        assert!(valid);
    }
}
