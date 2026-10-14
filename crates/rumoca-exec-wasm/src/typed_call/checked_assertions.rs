//! An observation adapter retains its exact borrowed invocation authority.
pub(crate) mod decode;
#[cfg(test)]
mod tests;

use crate::typed_call::observations::ObservationPlan;
use crate::typed_call::{emit, link::LinkedOwners};
use crate::{CompiledTypedCallWasm, TypedCallCompileError, TypedCallFault, WasmCompileError};
use rumoca_ir_solve as solve;

/// One initialized typed range, preserving integer/Boolean and Binary64 bits.
#[derive(Debug)]
pub struct NativeAssertionCells {
    value_type: solve::SolveValueType,
    bits: Vec<u64>,
}

impl NativeAssertionCells {
    pub fn value_type(&self) -> &solve::SolveValueType {
        &self.value_type
    }
    pub fn bits(&self) -> &[u64] {
        &self.bits
    }
}

/// Borrowed source frame plus the actual values of that dynamic invocation.
pub struct NativeAssertionFrame<'model> {
    owner: &'model solve::SolvePureCallOwner,
    program: &'model solve::TypedProgram,
    operation: usize,
    region_path: Vec<(usize, usize)>,
    domain_points: Vec<Vec<i64>>,
    arguments: Vec<NativeAssertionCells>,
}

impl<'model> NativeAssertionFrame<'model> {
    pub fn owner(&self) -> &'model solve::SolvePureCallOwner {
        self.owner
    }
    pub fn program(&self) -> &'model solve::TypedProgram {
        self.program
    }
    pub const fn operation(&self) -> usize {
        self.operation
    }
    pub fn region_path(&self) -> &[(usize, usize)] {
        &self.region_path
    }
    pub fn domain_points(&self) -> &[Vec<i64>] {
        &self.domain_points
    }
    pub fn arguments(&self) -> &[NativeAssertionCells] {
        &self.arguments
    }
}

/// Diagnostic projection of one actual record, never an ordinary value tuple.
pub struct NativeAssertionObservation<'model, 'proof> {
    action: &'proof solve::CheckedAssertionAction<'model>,
    provenance: rumoca_core::Span,
    origin_predicate: usize,
    frames: Vec<NativeAssertionFrame<'model>>,
    captures: Vec<(usize, NativeAssertionCells)>,
}

impl<'model, 'proof> NativeAssertionObservation<'model, 'proof> {
    pub const fn action(&self) -> &'proof solve::CheckedAssertionAction<'model> {
        self.action
    }
    pub const fn source_span(&self) -> rumoca_core::Span {
        self.provenance
    }
    pub fn frames(&self) -> &[NativeAssertionFrame<'model>] {
        &self.frames
    }
    pub fn captures(&self) -> &[(usize, NativeAssertionCells)] {
        &self.captures
    }
}

pub enum CheckedAssertionWasmOutcome<'model, 'proof> {
    Complete {
        values: Vec<NativeAssertionCells>,
        observations: Vec<NativeAssertionObservation<'model, 'proof>>,
    },
    Stopped {
        observations: Vec<NativeAssertionObservation<'model, 'proof>>,
    },
    Fault {
        fault: TypedCallFault,
        observations: Vec<NativeAssertionObservation<'model, 'proof>>,
    },
}

/// Keeps table, source program, ordered actions and generated artifact together.
/// Module bytes and diagnostic metadata cannot construct or replace this proof.
pub struct CompiledCheckedAssertionWasm<'model> {
    invocation: solve::CheckedAssertionInvocation<'model>,
    artifact: CompiledTypedCallWasm,
    #[cfg(any(
        test,
        all(target_arch = "wasm32", target_os = "unknown"),
        all(feature = "host-engine", not(target_arch = "wasm32"))
    ))]
    carrier: Option<ObservationPlan>,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: crate::WasmKernelRuntime,
}

impl<'model> CompiledCheckedAssertionWasm<'model> {
    pub fn invocation(&self) -> &solve::CheckedAssertionInvocation<'model> {
        &self.invocation
    }
    pub fn artifact(&self) -> &CompiledTypedCallWasm {
        &self.artifact
    }

    #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
    pub fn prepare_host(self) -> Result<PreparedCheckedAssertionHost<'model>, WasmCompileError> {
        let runtime = crate::host_typed_call_runtime::HostTypedCallRuntime::new(&self.artifact)?;
        Ok(PreparedCheckedAssertionHost {
            source: self,
            runtime,
        })
    }

    fn validate_arguments(&self, arguments: &[u64]) -> Result<(), WasmCompileError> {
        if arguments.len().checked_mul(8) != Some(self.artifact.layout().input_bytes as usize) {
            return Err(WasmCompileError::Input(
                "checked invocation input footprint differs".into(),
            ));
        }
        Ok(())
    }

    /// Executes the retained issued module. No status/byte injection surface.
    /// FMI decides transaction publication; this adapter only returns records.
    pub fn call<'proof>(
        &'proof self,
        arguments: &[u64],
    ) -> Result<CheckedAssertionWasmOutcome<'model, 'proof>, WasmCompileError> {
        self.validate_arguments(arguments)?;
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            let input = arguments.to_vec();
            let mut output = vec![0u64; (self.artifact.layout().output_bytes / 8) as usize];
            let mut scratch = vec![0u64; (self.artifact.layout().scratch_bytes / 8) as usize];
            let status = self.runtime.call_typed(&input, &mut output, &mut scratch)?;
            self.decode_execution(&input, &output, &scratch, status)
        }
        #[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
        Err(WasmCompileError::Input(
            "checked native observations have no admitted engine on this target".into(),
        ))
    }
}

/// Explicit host execution of one exact issued assertion invocation.
/// Its private fields retain source authority together with the admitted engine.
///
/// ```compile_fail,E0451
/// use rumoca_exec_wasm::{CompiledCheckedAssertionWasm, PreparedCheckedAssertionHost};
/// fn rebind<'model>(
///     source: CompiledCheckedAssertionWasm<'model>,
///     original: PreparedCheckedAssertionHost<'model>,
/// ) -> PreparedCheckedAssertionHost<'model> {
///     PreparedCheckedAssertionHost { source, ..original }
/// }
/// ```
#[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
pub struct PreparedCheckedAssertionHost<'model> {
    source: CompiledCheckedAssertionWasm<'model>,
    runtime: crate::host_typed_call_runtime::HostTypedCallRuntime,
}

#[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
impl<'model> PreparedCheckedAssertionHost<'model> {
    pub fn invocation(&self) -> &solve::CheckedAssertionInvocation<'model> {
        self.source.invocation()
    }

    pub fn artifact(&self) -> &CompiledTypedCallWasm {
        self.source.artifact()
    }

    pub fn call<'proof>(
        &'proof self,
        arguments: &[u64],
    ) -> Result<CheckedAssertionWasmOutcome<'model, 'proof>, WasmCompileError> {
        self.source.validate_arguments(arguments)?;
        let input = arguments.to_vec();
        let layout = self.source.artifact.layout();
        let mut output = vec![0u64; (layout.output_bytes / 8) as usize];
        let mut scratch = vec![0u64; (layout.scratch_bytes / 8) as usize];
        let status = self.runtime.call_typed(&input, &mut output, &mut scratch)?;
        self.source
            .decode_execution(&input, &output, &scratch, status)
    }
}

/// Bytes-only compilation of an already checked observation projection.
/// Model assignment/C25 profiles remain separately admitted and still refuse.
pub fn compile_checked_assertion_wasm<'model>(
    invocation: solve::CheckedAssertionInvocation<'model>,
) -> Result<CompiledCheckedAssertionWasm<'model>, WasmCompileError> {
    if invocation.table().arithmetic().real_format() != solve::SolveRealFormat::Binary64 {
        return Err(compile_error(TypedCallCompileError::UnsupportedArithmetic));
    }
    let linked = LinkedOwners::construct_checked(&invocation).map_err(compile_error)?;
    let plan = linked
        .plan(invocation.owner().id())
        .map_err(compile_error)?;
    let layout = crate::TypedCallLayout {
        input_bytes: plan.input_bytes,
        output_bytes: plan.output_bytes,
        scratch_bytes: plan.scratch_bytes,
        inputs: invocation.owner().inputs().to_vec(),
        outputs: invocation.owner().outputs().to_vec(),
    };
    #[cfg(any(
        test,
        all(target_arch = "wasm32", target_os = "unknown"),
        all(feature = "host-engine", not(target_arch = "wasm32"))
    ))]
    let carrier = plan.observations.clone();
    let (module_bytes, faults) =
        emit::compile(invocation.table(), &linked, invocation.owner().id())
            .map_err(compile_error)?;
    let artifact = CompiledTypedCallWasm {
        module_bytes,
        layout,
        faults,
        math_imports: linked
            .math_imports
            .iter()
            .map(|import| import.symbol())
            .collect(),
    };
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    let runtime = crate::WasmKernelRuntime::new_named(artifact.module_bytes(), "eval_typed_call")?;
    Ok(CompiledCheckedAssertionWasm {
        invocation,
        artifact,
        #[cfg(any(
            test,
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32"))
        ))]
        carrier,
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        runtime,
    })
}

fn compile_error(error: TypedCallCompileError) -> WasmCompileError {
    WasmCompileError::TypedAdmission(error)
}
