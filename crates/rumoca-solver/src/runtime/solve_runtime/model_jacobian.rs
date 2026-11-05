//! Optional prepared model-J owner, reused by the existing colored JVP callers.
use super::*;
#[cfg(test)]
mod tests;

#[derive(Clone)]
pub(super) enum PreparedModelJacobian {
    NotRequested,
    Compiled {
        code: Rc<dyn CompiledSolveModelStateJacobian>,
        cache: RefCell<JacobianCache>,
    },
    Unsupported {
        reason: String,
    },
}

#[derive(Clone)]
pub(super) struct JacobianCache {
    valid: bool,
    time: u64,
    state: Vec<f64>,
    parameters: Vec<f64>,
    matrix: Vec<f64>,
    output: Vec<f64>,
}

fn zeros(n: usize) -> Result<Vec<f64>, String> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(n)
        .map_err(|error| error.to_string())?;
    values.resize(n, 0.0);
    Ok(values)
}

impl PreparedModelJacobian {
    pub(super) fn prepare(
        backend: Option<&Rc<dyn SolveExecutionBackend>>,
        source: solve::SolveExecutionSource,
    ) -> Result<Self, EvalSolveError> {
        let Some(backend) = backend else {
            return Ok(Self::Unsupported {
                reason: "explicit reference runtime has no execution backend".into(),
            });
        };
        let n = source.model().state_scalar_count();
        let p = source.model().problem.layout.p_scalars();
        let prepare = || -> Result<Self, String> {
            match backend.compile_model_state_jacobian(source)? {
                ModelStateJacobianPreparation::Unsupported { reason } => {
                    Ok(Self::Unsupported { reason })
                }
                ModelStateJacobianPreparation::Compiled(code) => {
                    let cells = n.checked_mul(n).ok_or("model Jacobian extent overflow")?;
                    Ok(Self::Compiled {
                        code,
                        cache: RefCell::new(JacobianCache {
                            valid: false,
                            time: 0,
                            state: zeros(n)?,
                            parameters: zeros(p)?,
                            matrix: zeros(cells)?,
                            output: zeros(n)?,
                        }),
                    })
                }
            }
        };
        prepare().map_err(|message| EvalSolveError::CompiledExecution {
            message: format!("model state Jacobian preparation failed: {message}"),
            span: None,
        })
    }

    pub(super) fn unsupported_reason(&self) -> Option<&str> {
        match self {
            Self::Unsupported { reason } => Some(reason),
            Self::NotRequested | Self::Compiled { .. } => None,
        }
    }

    pub(super) fn jvp(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Option<Result<(), RuntimeSolveError>> {
        match self {
            Self::NotRequested | Self::Unsupported { .. } => None,
            Self::Compiled { code, cache } => {
                Some(
                    cache
                        .borrow_mut()
                        .apply(code.as_ref(), time, state, parameters, seed, out),
                )
            }
        }
    }
}

fn same_bits(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(a, b)| a.to_bits() == b.to_bits())
}

fn refused(message: &str) -> RuntimeSolveError {
    RuntimeSolveError::CompiledExecution {
        context: "model state Jacobian",
        message: message.into(),
        source_spans: Box::new([]),
    }
}

impl JacobianCache {
    fn apply(
        &mut self,
        code: &dyn CompiledSolveModelStateJacobian,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let n = self.state.len();
        if state.len() != n || parameters.len() < self.parameters.len() || out.len() != n {
            return Err(refused("state/parameter/seed/output extent mismatch"));
        }
        let parameters = &parameters[..self.parameters.len()];
        // State-only JVPs zero-extend short seeds and ignore the trailing
        // parameter span, exactly as the existing directional base does.
        let seed = &seed[..seed.len().min(n)];
        if !seed.iter().all(|value| value.is_finite()) {
            return Err(refused("nonfinite directional seed"));
        }
        if !self.valid
            || self.time != time.to_bits()
            || !same_bits(&self.state, state)
            || !same_bits(&self.parameters, parameters)
        {
            // Failure never leaves an accepted coordinate or permits fallback.
            self.valid = false;
            code.call(state, parameters, time, &mut self.matrix)?;
            if !self.matrix.iter().all(|value| value.is_finite()) {
                return Err(refused("nonfinite prepared model Jacobian"));
            }
            self.state.copy_from_slice(state);
            self.parameters.copy_from_slice(parameters);
            self.time = time.to_bits();
            self.valid = true;
        }
        for (row, value) in self.output.iter_mut().enumerate() {
            *value = self.matrix[row * n..(row + 1) * n]
                .iter()
                .zip(seed)
                .map(|(coefficient, seed)| coefficient * seed)
                .sum();
        }
        if !self.output.iter().all(|value| value.is_finite()) {
            return Err(refused("nonfinite prepared model Jacobian action"));
        }
        out.copy_from_slice(&self.output);
        Ok(())
    }
}

impl SolveRuntime {
    pub(crate) fn model_state_jacobian_request(&self) -> ModelStateJacobianRequest {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => match &scalar.model_jacobian {
                PreparedModelJacobian::NotRequested => ModelStateJacobianRequest::NotRequested,
                _ => ModelStateJacobianRequest::FiniteAssembly,
            },
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => ModelStateJacobianRequest::NotRequested,
        }
    }

    /// Explicit finite full-Jacobian action for implicit-integrator assembly.
    /// General directional APIs never enter this narrower model product.
    pub(crate) fn prepared_finite_state_jacobian_action(
        &self,
        time: f64,
        state: &[f64],
        parameters: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Option<Result<(), RuntimeSolveError>> {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => scalar
                .model_jacobian
                .jvp(time, state, parameters, seed, out),
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => None,
        }
    }

    /// Why this runtime retains its directional base instead of the optional
    /// prepared full-model product. Native ME hosts also retain this in declines.
    pub fn model_state_jacobian_fallback_reason(&self) -> Option<&str> {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => scalar.model_jacobian.unsupported_reason(),
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => Some("native owner has no model-J adapter"),
        }
    }
}
