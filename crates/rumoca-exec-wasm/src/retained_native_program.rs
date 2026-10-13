//! Complete native preparation bound to its immutable FMI source allocation.

mod cells;
mod execution;
#[cfg(all(
    feature = "fixed-native-guest-receiver",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
mod linked_guest;
mod transport;
pub use execution::NativeInvocationOutcome;
#[cfg(all(
    feature = "fixed-native-guest-receiver",
    target_arch = "wasm32",
    target_os = "wasi",
    target_env = "p2"
))]
pub use linked_guest::{LinkedNativeGuestModel, receive_fixed_native_guest_model};
pub use transport::{
    NativeInvocationLease, NativeTransport, NativeTransportError, NativeTransportSnapshot,
    NativeTypedInput, NativeTypedInputBatch, PreparedNativeStorage,
};

use std::{cell::Cell, rc::Rc};

use rumoca_ir_solve::{RetainedAssertionRoot, RetainedNativePreparation, RetainedNativeSchedule};

use crate::{CompiledNativeCallProgramWasm, WasmCompileError};

/// One compiled code and private-arena allocation, without execution admission.
struct NativeCode {
    inputs: RetainedNativePreparation,
    compiled: CompiledNativeCallProgramWasm,
    entered: Cell<bool>,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: crate::WasmKernelRuntime,
    #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
    host: crate::host_runtime::HostKernelRuntime,
    #[cfg(all(
        feature = "fixed-native-guest-receiver",
        target_arch = "wasm32",
        target_os = "wasi",
        target_env = "p2"
    ))]
    linked_entry: Option<linked_guest::StaticLinkedEntry>,
}

impl NativeCode {
    #[cfg(any(
        all(target_arch = "wasm32", target_os = "unknown"),
        all(feature = "host-engine", not(target_arch = "wasm32")),
        all(
            feature = "fixed-native-guest-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        )
    ))]
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        scratch: &mut [u64],
        typed: &mut [u64],
    ) -> Result<u32, WasmCompileError> {
        self.require_execution_entry()?;
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            self.runtime.call_native(y, p, time, scratch, typed)
        }
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        {
            self.host.call_native(y, p, time, scratch, typed)
        }
        #[cfg(all(
            feature = "fixed-native-guest-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        {
            self.linked_entry
                .as_ref()
                .expect("actual entry checked above")
                .call(y, p, time, scratch, typed)
        }
    }

    fn require_execution_entry(&self) -> Result<(), WasmCompileError> {
        if !self.compiled.carried_state().is_empty() {
            return Err(WasmCompileError::Input(
                "carried state publishes into P, which this transport passes read-only".into(),
            ));
        }
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        {
            // This variant owns the actual instantiated browser engine.
            Ok(())
        }
        #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
        {
            // The embedded host engine was instantiated with the module.
            Ok(())
        }
        #[cfg(all(
            feature = "fixed-native-guest-receiver",
            target_arch = "wasm32",
            target_os = "wasi",
            target_env = "p2"
        ))]
        {
            self.linked_entry.as_ref().map(|_| ()).ok_or_else(|| {
                WasmCompileError::Input(
                    "native FMI execution has no sealed entry for this engine".into(),
                )
            })
        }
        #[cfg(not(any(
            all(target_arch = "wasm32", target_os = "unknown"),
            all(feature = "host-engine", not(target_arch = "wasm32")),
            all(
                feature = "fixed-native-guest-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            )
        )))]
        {
            Err(WasmCompileError::Input(
                "native FMI execution has no sealed entry for this engine".into(),
            ))
        }
    }
}

/// An opaque compile-once owner. Cloning shares code, never a source copy.
/// Prepared storage grants no execution lease or outcome before activation.
#[derive(Clone)]
pub struct PreparedNativeWasmCode {
    code: Rc<NativeCode>,
}

impl PreparedNativeWasmCode {
    /// Inspect this actual code owner; selecting a target or feature grants nothing.
    pub fn require_execution_entry(&self) -> Result<(), WasmCompileError> {
        self.code.require_execution_entry()
    }

    pub fn inputs(&self) -> &RetainedNativePreparation {
        &self.code.inputs
    }

    pub fn compiled(&self) -> &CompiledNativeCallProgramWasm {
        &self.code.compiled
    }

    /// Bind this code to the actual instance's once-resolved original root.
    pub fn bind_resolved(
        &self,
        root: RetainedAssertionRoot,
    ) -> Result<CompiledRetainedNativeProgramWasm, WasmCompileError> {
        let inputs = self
            .code
            .inputs
            .bind_resolved(root)
            .map_err(|error| WasmCompileError::Input(error.to_string()))?;
        Ok(CompiledRetainedNativeProgramWasm {
            inputs,
            code: Rc::clone(&self.code),
        })
    }
}

/// Exact resolved execution inputs and their shared compiled code. This is not
/// an FMI deployment capability or an assertion-observation adapter.
pub struct CompiledRetainedNativeProgramWasm {
    inputs: RetainedNativeSchedule,
    code: Rc<NativeCode>,
}

/// One retained execution product shared by its resource transports.
/// No raw owner or independent invocation constructor escapes this handle.
#[derive(Clone)]
pub struct RetainedNativeWasmHost {
    product: Rc<CompiledRetainedNativeProgramWasm>,
}

impl RetainedNativeWasmHost {
    pub fn product(&self) -> &CompiledRetainedNativeProgramWasm {
        &self.product
    }
}

impl CompiledRetainedNativeProgramWasm {
    pub fn require_execution_entry(&self) -> Result<(), WasmCompileError> {
        self.code.require_execution_entry()
    }

    pub fn into_host(self) -> RetainedNativeWasmHost {
        RetainedNativeWasmHost {
            product: Rc::new(self),
        }
    }

    pub fn inputs(&self) -> &RetainedNativeSchedule {
        &self.inputs
    }

    pub fn compiled(&self) -> &CompiledNativeCallProgramWasm {
        &self.code.compiled
    }
}

/// Compile the already-issued stateless source once without resolving an
/// instance start time or granting execution authority.
pub fn compile_retained_native_preparation_wasm(
    inputs: RetainedNativePreparation,
) -> Result<PreparedNativeWasmCode, WasmCompileError> {
    compile_retained_native_preparation_wasm_with_policy(
        inputs,
        crate::CheckedNativeWasmCapacityPolicy::default(),
    )
}

/// Compile once with explicit checked bounds; no execution entry is admitted.
pub fn compile_retained_native_preparation_wasm_with_policy(
    inputs: RetainedNativePreparation,
    policy: crate::CheckedNativeWasmCapacityPolicy,
) -> Result<PreparedNativeWasmCode, WasmCompileError> {
    compile_prepared(
        inputs,
        policy,
        &rumoca_ir_solve::NativeCarriedState::default(),
    )
}

/// Compile once with `carried` state published into P instead of the host Y
/// (SOLVE-C101). The code is for hosts that own the linear memory of the module;
/// the transports of this crate pass P read-only and admit no execution entry
/// for it.
pub fn compile_retained_native_preparation_wasm_carrying(
    inputs: RetainedNativePreparation,
    policy: crate::CheckedNativeWasmCapacityPolicy,
    carried: &rumoca_ir_solve::NativeCarriedState,
) -> Result<PreparedNativeWasmCode, WasmCompileError> {
    compile_prepared(inputs, policy, carried)
}

fn compile_prepared(
    inputs: RetainedNativePreparation,
    policy: crate::CheckedNativeWasmCapacityPolicy,
    carried: &rumoca_ir_solve::NativeCarriedState,
) -> Result<PreparedNativeWasmCode, WasmCompileError> {
    let model = inputs.root().model();
    if !model.external_tables.is_empty() {
        return Err(WasmCompileError::Input(
            "retained native preparation has no external table storage".into(),
        ));
    }
    let carrying;
    let schedule = if carried.is_empty() {
        inputs.schedule()
    } else {
        carrying = inputs.schedule().with_carried_state(carried.clone());
        &carrying
    };
    let compiled = crate::compile_native_assignment_schedule_with_calls_wasm_and_policy(
        schedule,
        &model.problem.layout,
        &model.pure_calls,
        policy,
    )?;
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    let runtime = crate::WasmKernelRuntime::new_named(compiled.module_bytes(), "eval_assignments")?;
    #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
    let host = crate::host_runtime::HostKernelRuntime::new_named(
        compiled.module_bytes(),
        "eval_assignments",
    )?;
    Ok(PreparedNativeWasmCode {
        code: Rc::new(NativeCode {
            inputs,
            compiled,
            entered: Cell::new(false),
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            runtime,
            #[cfg(all(feature = "host-engine", not(target_arch = "wasm32")))]
            host,
            #[cfg(all(
                feature = "fixed-native-guest-receiver",
                target_arch = "wasm32",
                target_os = "wasi",
                target_env = "p2"
            ))]
            linked_entry: None,
        }),
    })
}

/// Prepare and activate the existing status-returning entry from exact resolved
/// inputs. Repeated execution never recompiles code or reconstructs its source.
pub fn compile_retained_native_program_wasm(
    inputs: RetainedNativeSchedule,
) -> Result<CompiledRetainedNativeProgramWasm, WasmCompileError> {
    let root = inputs.root().clone();
    let preparation = root
        .native_preparation()
        .map_err(|error| WasmCompileError::Input(error.to_string()))?;
    compile_retained_native_preparation_wasm(preparation)?.bind_resolved(root)
}
